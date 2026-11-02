#!/usr/bin/env Rscript
#===============================================================================
# Ride-hailing Data Cleaning Script
# Author: Data Analyst
# Date: 2025
# Description: Production-quality data cleaning for ride-sharing dataset
#===============================================================================

# Load required packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(stringr)
  library(readr)
  library(cli)
  library(glue)
  library(fs)
  library(checkmate)
})

# Set working directory and create output structure
# Run from the repository root, e.g.  Rscript R/01_data_cleaning.R

# Create output directories
dir_create("data/processed", recursive = TRUE)
dir_create("results/logs", recursive = TRUE)

# Configure logging
log_file <- "results/logs/data_cleaning.log"
cat("=== Data Cleaning Started ===\n", file = log_file, append = FALSE)
cat("Timestamp:", as.character(Sys.time()), "\n", file = log_file, append = TRUE)

#===============================================================================
# Data Loading Functions
#===============================================================================

#' Load and validate raw data
#' @return tibble with raw data
load_raw_data <- function(file_path = "data/raw/sessions_raw.csv") {
  cli_h2("Loading raw data")
  
  # Validate file exists
  assert_file_exists(file_path, access = "r")
  
  # Load data with proper column specifications
  raw_data <- read_csv(
    file_path,
    col_types = cols(
      user_id = col_integer(),
      session_id = col_integer(),
      region = col_character(),
      operating_system = col_character(),
      recommendation_strategy = col_character(),
      session_timestamp = col_character(),  # Will parse later
      weather_index = col_double(),
      event_peak = col_integer(),
      city_density = col_double(),
      fare_estimate = col_double(),
      surge_multiplier = col_double(),
      wait_time_estimate_min = col_integer(),
      driver_rating_shown = col_double(),
      options_viewed_count = col_integer(),
      pct_offer_inspection = col_integer(),
      promo_impressions = col_integer(),
      app_errors_count = col_integer(),
      ride_requested = col_integer(),
      ride_completed = col_integer(),
      cancel_after_request = col_integer(),
      wait_time_actual_min = col_double(),
      user_age_group = col_character(),
      primary_route_type = col_character()
    ),
    locale = locale(encoding = "UTF-8"),
    na = c("", "NA", "NULL", " ")
  )
  
  cli_alert_success("Loaded {nrow(raw_data)} rows, {ncol(raw_data)} columns")
  cat("Raw data loaded:", nrow(raw_data), "rows\n", file = log_file, append = TRUE)
  
  return(raw_data)
}

#===============================================================================
# Data Validation Functions
#===============================================================================

#' Validate data structure and basic constraints
#' @param data tibble to validate
#' @return list with validation results
validate_data_structure <- function(data) {
  cli_h2("Validating data structure")
  
  validation_results <- list()
  
  # Check required columns
  required_cols <- c(
    "user_id", "session_id", "region", "operating_system", 
    "recommendation_strategy", "session_timestamp", "ride_requested", 
    "ride_completed", "user_age_group", "primary_route_type"
  )
  
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    cli_alert_danger("Missing required columns: {paste(missing_cols, collapse = ', ')}")
    stop("Missing required columns")
  }
  
  # Check for duplicate session IDs
  duplicate_sessions <- data %>%
    group_by(session_id) %>%
    summarise(count = n(), .groups = "drop") %>%
    filter(count > 1)
  
  if (nrow(duplicate_sessions) > 0) {
    cli_alert_warning("Found {nrow(duplicate_sessions)} duplicate session IDs")
    cat("Duplicate sessions:", nrow(duplicate_sessions), "\n", file = log_file, append = TRUE)
  }
  
  # Validate categorical variables
  valid_regions <- c("LON", "BER", "PAR")
  invalid_regions <- data %>%
    filter(!region %in% valid_regions) %>%
    nrow()
  
  valid_os <- c("Android", "iOS")
  invalid_os <- data %>%
    filter(!operating_system %in% valid_os) %>%
    nrow()
  
  valid_strategies <- c("price_first", "rating_first", "time_first")
  invalid_strategies <- data %>%
    filter(!recommendation_strategy %in% valid_strategies) %>%
    nrow()
  
  valid_age_groups <- c("18-24", "25-34", "35-49", "50+")
  invalid_age_groups <- data %>%
    filter(!user_age_group %in% valid_age_groups) %>%
    nrow()
  
  valid_route_types <- c("commute", "airport", "nightlife", "city_center")
  invalid_route_types <- data %>%
    filter(!primary_route_type %in% valid_route_types) %>%
    nrow()
  
  validation_results$duplicate_sessions <- nrow(duplicate_sessions)
  validation_results$invalid_regions <- invalid_regions
  validation_results$invalid_os <- invalid_os
  validation_results$invalid_strategies <- invalid_strategies
  validation_results$invalid_age_groups <- invalid_age_groups
  validation_results$invalid_route_types <- invalid_route_types
  
  cli_alert_success("Data structure validation completed")
  return(validation_results)
}

#===============================================================================
# Data Cleaning Functions
#===============================================================================

#' Clean timestamp data with multiple format handling
#' @param data tibble with timestamp column
#' @return tibble with cleaned timestamps
clean_timestamps <- function(data) {
  cli_h2("Cleaning timestamp data")
  
  initial_count <- nrow(data)
  
  # Parse timestamps with multiple format handling
  data_cleaned <- data %>%
    mutate(
      # Handle different date formats
      timestamp_parsed = case_when(
        # Format: 2025-03-01 11:23:21
        str_detect(session_timestamp, "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$") ~
          ymd_hms(session_timestamp, tz = "Europe/Berlin"),
        
        # Format: 22/04/2025 02:20
        str_detect(session_timestamp, "^\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}$") ~
          dmy_hm(session_timestamp, tz = "Europe/Berlin"),
        
        # Format: 02-07-2025 23:10:55
        str_detect(session_timestamp, "^\\d{2}-\\d{2}-\\d{4} \\d{2}:\\d{2}:\\d{2}$") ~
          dmy_hms(session_timestamp, tz = "Europe/Berlin"),
        
        # Format: 03-02-2025 02:20:01
        str_detect(session_timestamp, "^\\d{2}-\\d{2}-\\d{4} \\d{2}:\\d{2}:\\d{2}$") ~
          dmy_hms(session_timestamp, tz = "Europe/Berlin"),
        
        TRUE ~ NA_POSIXct_
      ),
      
      # Extract date and time components
      session_date = date(timestamp_parsed),
      session_hour = hour(timestamp_parsed),
      session_weekday = wday(timestamp_parsed, label = TRUE, abbr = FALSE),
      session_month = month(timestamp_parsed, label = TRUE, abbr = FALSE),
      session_quarter = quarter(timestamp_parsed)
    )
  
  # Log parsing results
  parsed_count <- sum(!is.na(data_cleaned$timestamp_parsed))
  failed_parsing <- initial_count - parsed_count
  
  cli_alert_info("Parsed {parsed_count} timestamps successfully")
  if (failed_parsing > 0) {
    cli_alert_warning("Failed to parse {failed_parsing} timestamps")
    cat("Failed timestamp parsing:", failed_parsing, "\n", file = log_file, append = TRUE)
  }
  
  return(data_cleaned)
}

#' Clean categorical variables
#' @param data tibble to clean
#' @return tibble with cleaned categorical variables
clean_categorical_variables <- function(data) {
  cli_h2("Cleaning categorical variables")
  
  data_cleaned <- data %>%
    # Standardize region names
    mutate(
      region = case_when(
        region %in% c("LON") ~ "London",
        region %in% c("BER") ~ "Berlin", 
        region %in% c("PAR") ~ "Paris",
        TRUE ~ region
      )
    ) %>%
    
    # Fix age group formatting issues
    mutate(
      user_age_group = case_when(
        str_detect(user_age_group, "25_34") ~ "25-34",
        TRUE ~ user_age_group
      )
    ) %>%
    
    # Standardize route type names
    mutate(
      primary_route_type = case_when(
        primary_route_type == "city_center" ~ "city_center",
        primary_route_type == "nightlife" ~ "nightlife",
        primary_route_type == "commute" ~ "commute",
        primary_route_type == "airport" ~ "airport",
        TRUE ~ primary_route_type
      )
    ) %>%
    
    # Create factor variables for proper analysis
    mutate(
      region = factor(region, levels = c("London", "Berlin", "Paris")),
      operating_system = factor(operating_system, levels = c("Android", "iOS")),
      recommendation_strategy = factor(recommendation_strategy, 
                                     levels = c("price_first", "rating_first", "time_first")),
      user_age_group = factor(user_age_group, 
                             levels = c("18-24", "25-34", "35-49", "50+")),
      primary_route_type = factor(primary_route_type,
                                 levels = c("commute", "airport", "nightlife", "city_center"))
    )
  
  cli_alert_success("Categorical variables cleaned and standardized")
  return(data_cleaned)
}

#' Clean numeric variables and handle outliers
#' @param data tibble to clean
#' @return tibble with cleaned numeric variables
clean_numeric_variables <- function(data) {
  cli_h2("Cleaning numeric variables")
  
  initial_count <- nrow(data)
  
  data_cleaned <- data %>%
    # Clean weather index (should be 0-1)
    mutate(
      weather_index = case_when(
        weather_index < 0 ~ 0,
        weather_index > 1 ~ 1,
        TRUE ~ weather_index
      )
    ) %>%
    
    # Clean surge multiplier (should be >= 1)
    mutate(
      surge_multiplier = case_when(
        surge_multiplier < 1 ~ 1.0,
        surge_multiplier > 10 ~ 10.0,  # Cap extreme outliers
        TRUE ~ surge_multiplier
      )
    ) %>%
    
    # Clean fare estimates (remove negative values)
    mutate(
      fare_estimate = case_when(
        fare_estimate < 0 ~ NA_real_,
        fare_estimate > 1000 ~ NA_real_,  # Remove extreme outliers
        TRUE ~ fare_estimate
      )
    ) %>%
    
    # Clean wait time estimates (should be positive integers)
    mutate(
      wait_time_estimate_min = case_when(
        wait_time_estimate_min < 0 ~ NA_integer_,
        wait_time_estimate_min > 120 ~ 120L,  # Cap at 2 hours
        TRUE ~ wait_time_estimate_min
      )
    ) %>%
    
    # Clean driver ratings (should be 0-5)
    mutate(
      driver_rating_shown = case_when(
        driver_rating_shown < 0 ~ NA_real_,
        driver_rating_shown > 5 ~ 5.0,
        TRUE ~ driver_rating_shown
      )
    ) %>%
    
    # Clean wait time actual (should be positive)
    mutate(
      wait_time_actual_min = case_when(
        wait_time_actual_min < 0 ~ NA_real_,
        wait_time_actual_min > 180 ~ NA_real_,  # Remove extreme outliers (>3 hours)
        TRUE ~ wait_time_actual_min
      )
    ) %>%
    
    # Clean count variables (should be non-negative integers)
    mutate(
      across(c(event_peak, options_viewed_count, pct_offer_inspection, 
               promo_impressions, app_errors_count, ride_requested, 
               ride_completed, cancel_after_request),
             ~ case_when(
               .x < 0 ~ NA_integer_,
               TRUE ~ .x
             ))
    )
  
  cli_alert_success("Numeric variables cleaned and outliers handled")
  return(data_cleaned)
}

#' Handle missing values with proper logging
#' @param data tibble to process
#' @return tibble with missing value handling
handle_missing_values <- function(data) {
  cli_h2("Handling missing values")
  
  # Log missing values before handling
  missing_summary <- data %>%
    summarise_all(~ sum(is.na(.))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
    mutate(missing_pct = round(missing_count / nrow(data) * 100, 2)) %>%
    filter(missing_count > 0) %>%
    arrange(desc(missing_count))
  
  if (nrow(missing_summary) > 0) {
    cli_alert_info("Missing values found:")
    print(missing_summary)
    cat("Missing values summary:\n", file = log_file, append = TRUE)
    write.table(missing_summary, file = log_file, append = TRUE, 
                row.names = FALSE, sep = "\t")
  }
  
  # For this analysis, we'll keep missing values as they may be informative
  # Only remove rows where critical variables are missing
  data_cleaned <- data %>%
    filter(
      !is.na(user_id),
      !is.na(session_id),
      !is.na(region),
      !is.na(ride_requested)
    )
  
  removed_count <- nrow(data) - nrow(data_cleaned)
  if (removed_count > 0) {
    cli_alert_warning("Removed {removed_count} rows with missing critical variables")
    cat("Rows removed due to missing critical variables:", removed_count, "\n", 
        file = log_file, append = TRUE)
  }
  
  return(data_cleaned)
}

#===============================================================================
# Feature Engineering Functions
#===============================================================================

#' Create derived features for analysis
#' @param data tibble to process
#' @return tibble with new features
create_derived_features <- function(data) {
  cli_h2("Creating derived features")
  
  data_enhanced <- data %>%
    # Create time-based features
    mutate(
      is_weekend = ifelse(session_weekday %in% c("Saturday", "Sunday"), TRUE, FALSE),
      is_peak_hour = ifelse(session_hour %in% c(7:9, 17:19), TRUE, FALSE),
      is_night_time = ifelse(session_hour >= 22 | session_hour <= 5, TRUE, FALSE),
      time_of_day = case_when(
        session_hour >= 6 & session_hour < 12 ~ "Morning",
        session_hour >= 12 & session_hour < 18 ~ "Afternoon", 
        session_hour >= 18 & session_hour < 22 ~ "Evening",
        TRUE ~ "Night"
      )
    ) %>%
    
    # Create pricing features
    mutate(
      total_fare_estimate = fare_estimate * surge_multiplier,
      is_surge_pricing = surge_multiplier > 1.0,
      fare_per_minute = total_fare_estimate / pmax(wait_time_estimate_min, 1)
    ) %>%
    
    # Create user behavior features
    mutate(
      conversion_rate = ride_completed / pmax(ride_requested, 1),
      cancellation_rate = cancel_after_request / pmax(ride_requested, 1),
      wait_time_difference = wait_time_actual_min - wait_time_estimate_min,
      wait_time_accuracy = ifelse(
        is.na(wait_time_actual_min) | is.na(wait_time_estimate_min),
        NA_real_,
        abs(wait_time_difference) / pmax(wait_time_estimate_min, 1)
      )
    ) %>%
    
    # Create app experience features
    mutate(
      has_app_errors = app_errors_count > 0,
      high_promo_exposure = promo_impressions > quantile(promo_impressions, 0.8, na.rm = TRUE),
      options_engagement = options_viewed_count / pmax(promo_impressions, 1)
    )
  
  cli_alert_success("Derived features created successfully")
  return(data_enhanced)
}

#===============================================================================
# Main Data Processing Pipeline
#===============================================================================

#' Main data cleaning pipeline
#' @return cleaned tibble
process_data <- function() {
  cli_h1("Starting Ride-hailing Data Cleaning Pipeline")
  
  # Load raw data
  raw_data <- load_raw_data()
  
  # Validate data structure
  validation_results <- validate_data_structure(raw_data)
  
  # Apply cleaning functions
  cleaned_data <- raw_data %>%
    clean_timestamps() %>%
    clean_categorical_variables() %>%
    clean_numeric_variables() %>%
    handle_missing_values() %>%
    create_derived_features()
  
  # Final validation
  final_validation <- validate_data_structure(cleaned_data)
  
  # Log final statistics
  cli_h2("Final Data Summary")
  cli_alert_success("Final dataset: {nrow(cleaned_data)} rows, {ncol(cleaned_data)} columns")
  cli_alert_info("Date range: {min(cleaned_data$session_date, na.rm = TRUE)} to {max(cleaned_data$session_date, na.rm = TRUE)}")
  
  cat("Final dataset:", nrow(cleaned_data), "rows,", ncol(cleaned_data), "columns\n", 
      file = log_file, append = TRUE)
  cat("Date range:", as.character(min(cleaned_data$session_date, na.rm = TRUE)), 
      "to", as.character(max(cleaned_data$session_date, na.rm = TRUE)), "\n",
      file = log_file, append = TRUE)
  
  return(cleaned_data)
}

#===============================================================================
# Export Functions
#===============================================================================

#' Export cleaned data
#' @param data tibble to export
export_cleaned_data <- function(data) {
  cli_h2("Exporting cleaned data")
  
  # Export main dataset
  output_file <- "data/processed/sessions_clean.csv"
  write_csv(data, output_file)
  cli_alert_success("Exported cleaned data to {output_file}")
  
  # Export data summary
  summary_file <- "results/data_summary.txt"
  cat("=== Ride-hailing Data Summary ===\n", file = summary_file)
  cat("Generated:", as.character(Sys.time()), "\n\n")
  cat("Dataset Dimensions:", nrow(data), "rows x", ncol(data), "columns\n\n")
  
  cat("=== Variable Summary ===\n")
  summary_stats <- data %>%
    select_if(is.numeric) %>%
    summary()
  capture.output(summary_stats, file = summary_file, append = TRUE)
  
  cat("\n=== Categorical Variables ===\n", file = summary_file, append = TRUE)
  cat_vars <- data %>% select_if(is.factor) %>% names()
  for (var in cat_vars) {
    cat("\n", var, ":\n", file = summary_file, append = TRUE)
    cat_table <- table(data[[var]], useNA = "ifany")
    capture.output(cat_table, file = summary_file, append = TRUE)
  }
  
  cli_alert_success("Data summary exported to {summary_file}")
  
  return(data)
}

#===============================================================================
# Execute Pipeline
#===============================================================================

if (interactive()) {
  cli_alert_info("Running in interactive mode")
} else {
  cli_alert_info("Running as script")
}

# Execute the complete pipeline
tryCatch({
  cleaned_data <- process_data()
  final_data <- export_cleaned_data(cleaned_data)
  
  cli_h1("Data Cleaning Pipeline Completed Successfully")
  cli_alert_success("Cleaned data ready for analysis")
  
  cat("=== Pipeline Completed Successfully ===\n", file = log_file, append = TRUE)
  cat("Final timestamp:", as.character(Sys.time()), "\n", file = log_file, append = TRUE)
  
}, error = function(e) {
  cli_alert_danger("Pipeline failed: {e$message}")
  cat("=== Pipeline Failed ===\n", file = log_file, append = TRUE)
  cat("Error:", e$message, "\n", file = log_file, append = TRUE)
  cat("Timestamp:", as.character(Sys.time()), "\n", file = log_file, append = TRUE)
  stop(e)
})

# Return the final cleaned dataset
final_data
