#!/usr/bin/env Rscript
#==============================================================================
# Synthetic raw data for the ride-hailing A/B/C + bandit case
#
# The original analysis ran on a case dataset that cannot be redistributed. This
# script writes fake data with the same 23 columns, types and value ranges, and
# with the same kinds of data-quality problems the cleaning script handles
# (mixed timestamp formats, region codes, a mistyped age group, out-of-range and
# negative values, extreme outliers, duplicate session IDs, missing keys).
#
# The three recommendation strategies (the arms) get different, KNOWN effects,
# so the analysis has something real to find:
#   request rate         price_first 21.5%, rating_first 19.5%, time_first 20.0%
#   completion | request price_first 88%,   rating_first 90%,   time_first 93%
#
# Usage: Rscript scripts/make_synthetic_data.R   ->  data/raw/sessions_raw.csv
#==============================================================================
set.seed(42)
n <- 60000

strategy <- sample(c("price_first", "rating_first", "time_first"), n, replace = TRUE)
region   <- sample(c("LON", "BER", "PAR"), n, replace = TRUE, prob = c(0.60, 0.20, 0.20))
os       <- sample(c("Android", "iOS"), n, replace = TRUE, prob = c(0.525, 0.475))
age      <- sample(c("18-24", "25-34", "35-49", "50+"), n, replace = TRUE, prob = c(0.25, 0.35, 0.25, 0.15))
route    <- sample(c("commute", "airport", "nightlife", "city_center"), n, replace = TRUE,
                   prob = c(0.40, 0.15, 0.15, 0.30))

# timestamps: 2025-02-01 .. 2025-04-30, more sessions at commute peaks and evenings
day  <- as.Date("2025-02-01") + sample(0:88, n, replace = TRUE)
hour <- sample(0:23, n, replace = TRUE,
               prob = c(1,1,1,1,1,2,4,7,8,6,4,4,5,5,4,5,6,8,8,7,6,5,3,2))
ts   <- as.POSIXct(paste(day, sprintf("%02d:%02d:%02d", hour, sample(0:59, n, TRUE), sample(0:59, n, TRUE))),
                   tz = "Europe/Berlin")
fmt  <- sample(1:3, n, replace = TRUE, prob = c(0.70, 0.15, 0.15))     # three export formats
session_timestamp <- ifelse(fmt == 1, format(ts, "%Y-%m-%d %H:%M:%S"),
                     ifelse(fmt == 2, format(ts, "%d/%m/%Y %H:%M"), format(ts, "%d-%m-%Y %H:%M:%S")))

weather    <- round(runif(n), 3)
event_peak <- rbinom(n, 1, 0.07)
density    <- c(LON = 4530, BER = 4100, PAR = 5023)[region]
surge      <- ifelse(runif(n) < 0.30 + 0.25 * event_peak, round(1 + rexp(n, 1 / 0.35), 2), 1)
surge      <- pmin(surge, 2.8)
fare       <- round(exp(rnorm(n, log(8.9), 0.25)) * ifelse(strategy == "price_first", 0.93, 1), 1)
fare       <- pmin(pmax(fare, 6), 23.3)
wait_est   <- pmin(pmax(round(rnorm(n, 6, 1.6) - ifelse(strategy == "time_first", 0.8, 0)), 3), 11)
rating     <- round(pmin(5, rnorm(n, 4.72 + ifelse(strategy == "rating_first", 0.06, 0), 0.12)), 2)
options    <- pmin(rpois(n, 2.8), 10)
inspect    <- sample(c(0, 25, 50, 75, 100), n, replace = TRUE)
promo      <- pmin(rpois(n, 1.3), 10)
errors     <- rpois(n, 1.28)

# outcomes with planted strategy effects
p_req <- c(price_first = 0.215, rating_first = 0.195, time_first = 0.200)[strategy]
p_req <- pmin(pmax(p_req + 0.03 * (hour %in% c(7:9, 17:19)) - 0.02 * (surge > 1) - 0.01 * (errors >= 3), 0), 1)
requested <- rbinom(n, 1, p_req)
p_comp <- c(price_first = 0.88, rating_first = 0.90, time_first = 0.93)[strategy]
completed <- ifelse(requested == 1, rbinom(n, 1, p_comp), NA)
cancel    <- ifelse(requested == 1 & completed == 0, rbinom(n, 1, 0.02), 0)
wait_act  <- ifelse(requested == 1, round(pmax(1, wait_est + rnorm(n, 0.45, 1.2)), 1), NA)

raw <- data.frame(
  user_id = sample(100000:119999, n, replace = TRUE), session_id = 1:n, region = region,
  operating_system = os, recommendation_strategy = strategy, session_timestamp = session_timestamp,
  weather_index = weather, event_peak = event_peak, city_density = density, fare_estimate = fare,
  surge_multiplier = surge, wait_time_estimate_min = wait_est, driver_rating_shown = rating,
  options_viewed_count = options, pct_offer_inspection = inspect, promo_impressions = promo,
  app_errors_count = errors, ride_requested = requested, ride_completed = completed,
  cancel_after_request = cancel, wait_time_actual_min = wait_act, user_age_group = age,
  primary_route_type = route, stringsAsFactors = FALSE)

# data-quality problems of the kind the cleaning script is written for
i <- sample(n, 40);  raw$user_age_group[i]  <- "25_34"          # mistyped category
i <- sample(n, 30);  raw$weather_index[i]   <- round(runif(30, 1.01, 1.5), 3)
i <- sample(n, 25);  raw$surge_multiplier[i] <- 0.9
i <- sample(n, 15);  raw$fare_estimate[i]   <- -raw$fare_estimate[i]
i <- sample(n, 20);  raw$city_density[i]    <- -raw$city_density[i]
i <- sample(n, 5);   raw$app_errors_count[i] <- 198L              # logging bug
i <- sample(n, 10);  raw$session_id[i]      <- raw$session_id[i - 1]  # duplicated IDs
i <- sample(n, 16);  raw$region[i]          <- NA                 # missing key -> dropped

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
write.csv(raw, "data/raw/sessions_raw.csv", row.names = FALSE, na = "")
cat(sprintf("wrote data/raw/sessions_raw.csv: %d rows x %d columns\n", nrow(raw), ncol(raw)))
cat(sprintf("request rate %.1f%%; completion | request %.1f%%\n",
            100 * mean(raw$ride_requested), 100 * mean(raw$ride_completed, na.rm = TRUE)))
