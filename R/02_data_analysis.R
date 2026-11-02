#!/usr/bin/env Rscript
#===============================================================================
# Ride-hailing Data Analysis Script
# Author: Data Analyst
# Date: 2025
# Description: Advanced analytics for ride-sharing dataset
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
  library(corrplot)
  library(ggplot2)
  library(scales)
  library(patchwork)
})

# Set working directory
# Run from the repository root, e.g.  Rscript R/01_data_cleaning.R

#===============================================================================
# Data Loading Functions
#===============================================================================

#' Load cleaned data
#' @return tibble with cleaned data
load_cleaned_data <- function(file_path = "data/processed/sessions_clean.csv") {
  cli_h2("Loading cleaned data")
  
  if (!file_exists(file_path)) {
    cli_alert_danger("Cleaned data file not found: {file_path}")
    cli_alert_info("Please run 01_data_cleaning.R first")
    stop("Cleaned data not available")
  }
  
  cleaned_data <- read_csv(
    file_path,
    col_types = cols(
      .default = col_guess(),
      session_timestamp = col_character(),
      timestamp_parsed = col_datetime(),
      session_date = col_date(),
      region = col_character(),
      operating_system = col_character(),
      recommendation_strategy = col_character(),
      user_age_group = col_character(),
      primary_route_type = col_character(),
      session_weekday = col_character(),
      session_month = col_character(),
      time_of_day = col_character()
    )
  ) %>%
    # Convert to factors after loading
    mutate(
      region = factor(region),
      operating_system = factor(operating_system),
      recommendation_strategy = factor(recommendation_strategy),
      user_age_group = factor(user_age_group),
      primary_route_type = factor(primary_route_type),
      session_weekday = factor(session_weekday),
      session_month = factor(session_month),
      time_of_day = factor(time_of_day)
    )
  
  cli_alert_success("Loaded {nrow(cleaned_data)} rows, {ncol(cleaned_data)} columns")
  return(cleaned_data)
}

#===============================================================================
# Descriptive Analytics Functions
#===============================================================================

#' Generate comprehensive descriptive statistics
#' @param data tibble to analyze
#' @return list with descriptive statistics
generate_descriptive_stats <- function(data) {
  cli_h2("Generating descriptive statistics")
  
  stats <- list()
  
  # Basic dataset info
  stats$dataset_info <- list(
    total_rows = nrow(data),
    total_columns = ncol(data),
    date_range = paste(min(data$session_date, na.rm = TRUE), 
                      "to", max(data$session_date, na.rm = TRUE)),
    unique_users = n_distinct(data$user_id),
    unique_sessions = n_distinct(data$session_id)
  )
  
  # Regional distribution
  stats$regional_distribution <- data %>%
    count(region, name = "sessions") %>%
    mutate(percentage = round(sessions / sum(sessions) * 100, 2))
  
  # Conversion metrics
  stats$conversion_metrics <- data %>%
    summarise(
      total_sessions = n(),
      total_requests = sum(ride_requested, na.rm = TRUE),
      total_completions = sum(ride_completed, na.rm = TRUE),
      total_cancellations = sum(cancel_after_request, na.rm = TRUE),
      request_rate = round(sum(ride_requested, na.rm = TRUE) / n() * 100, 2),
      completion_rate = round(sum(ride_completed, na.rm = TRUE) / 
                             sum(ride_requested, na.rm = TRUE) * 100, 2),
      cancellation_rate = round(sum(cancel_after_request, na.rm = TRUE) / 
                               sum(ride_requested, na.rm = TRUE) * 100, 2)
    )
  
  # Pricing analysis
  stats$pricing_summary <- data %>%
    summarise(
      avg_fare_estimate = round(mean(fare_estimate, na.rm = TRUE), 2),
      median_fare_estimate = round(median(fare_estimate, na.rm = TRUE), 2),
      avg_surge_multiplier = round(mean(surge_multiplier, na.rm = TRUE), 2),
      surge_pricing_rate = round(sum(is_surge_pricing, na.rm = TRUE) / n() * 100, 2),
      avg_wait_time_estimate = round(mean(wait_time_estimate_min, na.rm = TRUE), 2),
      avg_wait_time_actual = round(mean(wait_time_actual_min, na.rm = TRUE), 2)
    )
  
  # Strategy performance
  stats$strategy_performance <- data %>%
    group_by(recommendation_strategy) %>%
    summarise(
      sessions = n(),
      request_rate = round(sum(ride_requested, na.rm = TRUE) / n() * 100, 2),
      completion_rate = round(sum(ride_completed, na.rm = TRUE) / 
                             sum(ride_requested, na.rm = TRUE) * 100, 2),
      avg_fare = round(mean(fare_estimate, na.rm = TRUE), 2),
      avg_wait_time = round(mean(wait_time_estimate_min, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  cli_alert_success("Descriptive statistics generated")
  return(stats)
}

#===============================================================================
# Visualization Functions
#===============================================================================

#' Create comprehensive visualization suite
#' @param data tibble to visualize
#' @param stats list with descriptive statistics
create_visualizations <- function(data, stats) {
  cli_h2("Creating visualizations")
  
  # Create output directory
  dir_create("results/plots")
  
  # Set theme
  theme_set(theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ))
  
  # 1. Regional Distribution
  p1 <- ggplot(stats$regional_distribution, aes(x = region, y = sessions, fill = region)) +
    geom_col(alpha = 0.8) +
    geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5) +
    labs(
      title = "Session Distribution by Region",
      subtitle = "Percentage of total sessions",
      x = "Region", y = "Number of Sessions",
      fill = "Region"
    ) +
    scale_y_continuous(labels = comma_format()) +
    theme(legend.position = "none")
  
  # 2. Strategy Performance Comparison
  p2 <- ggplot(stats$strategy_performance, aes(x = recommendation_strategy)) +
    geom_col(aes(y = completion_rate, fill = "Completion Rate"), alpha = 0.7, position = "dodge") +
    geom_col(aes(y = request_rate, fill = "Request Rate"), alpha = 0.7, position = "dodge") +
    labs(
      title = "Recommendation Strategy Performance",
      subtitle = "Request and completion rates by strategy",
      x = "Recommendation Strategy", y = "Rate (%)",
      fill = "Metric"
    ) +
    scale_y_continuous(labels = percent_format(scale = 1))
  
  # 3. Pricing Distribution
  p3 <- ggplot(data, aes(x = fare_estimate)) +
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
    labs(
      title = "Fare Estimate Distribution",
      subtitle = "Distribution of estimated fares",
      x = "Fare Estimate (Currency Units)", y = "Frequency"
    ) +
    scale_x_continuous(labels = dollar_format())
  
  # 4. Surge Pricing Analysis
  surge_data <- data %>%
    count(is_surge_pricing, name = "sessions") %>%
    mutate(percentage = sessions / sum(sessions) * 100)
  
  p4 <- ggplot(surge_data, aes(x = "", y = sessions, fill = is_surge_pricing)) +
    geom_col(width = 1) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(round(percentage, 1), "%")), 
              position = position_stack(vjust = 0.5)) +
    labs(
      title = "Surge Pricing Distribution",
      subtitle = "Proportion of sessions with surge pricing",
      fill = "Surge Pricing"
    ) +
    theme_void() +
    scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = "darkred"),
                      labels = c("FALSE" = "No Surge", "TRUE" = "Surge Pricing"))
  
  # 5. Wait Time Accuracy
  wait_time_data <- data %>%
    filter(!is.na(wait_time_estimate_min), !is.na(wait_time_actual_min)) %>%
    mutate(wait_time_difference = wait_time_actual_min - wait_time_estimate_min)
  
  p5 <- ggplot(wait_time_data, aes(x = wait_time_difference)) +
    geom_histogram(bins = 30, fill = "orange", alpha = 0.7) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Wait Time Estimation Accuracy",
      subtitle = "Difference between estimated and actual wait times",
      x = "Wait Time Difference (minutes)", y = "Frequency"
    ) +
    scale_x_continuous(breaks = seq(-20, 20, 5))
  
  # 6. Time-based Patterns
  hourly_patterns <- data %>%
    filter(!is.na(session_hour)) %>%
    group_by(session_hour) %>%
    summarise(
      sessions = n(),
      request_rate = sum(ride_requested, na.rm = TRUE) / n() * 100,
      .groups = "drop"
    )
  
  p6 <- ggplot(hourly_patterns, aes(x = session_hour, y = request_rate)) +
    geom_line(color = "steelblue", size = 1) +
    geom_point(color = "steelblue", size = 2) +
    labs(
      title = "Request Rate by Hour of Day",
      subtitle = "Percentage of sessions resulting in ride requests",
      x = "Hour of Day", y = "Request Rate (%)"
    ) +
    scale_x_continuous(breaks = seq(0, 23, 2)) +
    scale_y_continuous(labels = percent_format(scale = 1))
  
  # Save individual plots
  ggsave("results/plots/01_regional_distribution.png", p1, width = 8, height = 6, dpi = 300)
  ggsave("results/plots/02_strategy_performance.png", p2, width = 10, height = 6, dpi = 300)
  ggsave("results/plots/03_fare_distribution.png", p3, width = 8, height = 6, dpi = 300)
  ggsave("results/plots/04_surge_pricing.png", p4, width = 8, height = 6, dpi = 300)
  ggsave("results/plots/05_wait_time_accuracy.png", p5, width = 8, height = 6, dpi = 300)
  ggsave("results/plots/06_hourly_patterns.png", p6, width = 10, height = 6, dpi = 300)
  
  # Create combined dashboard
  dashboard <- (p1 | p2) / (p3 | p4) / (p5 | p6)
  ggsave("results/plots/07_analytics_dashboard.png", dashboard, width = 16, height = 12, dpi = 300)
  
  cli_alert_success("Visualizations created and saved to plots/ directory")
  
  return(list(
    regional_dist = p1,
    strategy_perf = p2,
    fare_dist = p3,
    surge_pricing = p4,
    wait_accuracy = p5,
    hourly_patterns = p6,
    dashboard = dashboard
  ))
}

#===============================================================================
# Statistical Analysis Functions
#===============================================================================

#' Perform correlation analysis
#' @param data tibble to analyze
#' @return correlation matrix and plot
analyze_correlations <- function(data) {
  cli_h2("Analyzing correlations")
  
  # Select numeric variables for correlation
  numeric_vars <- data %>%
    select_if(is.numeric) %>%
    select(-contains("id"), -contains("timestamp")) %>%
    na.omit()
  # drop constant columns (e.g. ride_requested after na.omit): their correlations are NA,
  # which makes corrplot's hclust ordering fail
  numeric_vars <- numeric_vars[, vapply(numeric_vars, function(x) sd(x) > 0, logical(1))]
  
  # Calculate correlation matrix
  cor_matrix <- cor(numeric_vars, use = "complete.obs")
  
  # Create correlation plot
  png("results/plots/08_correlation_matrix.png", width = 12, height = 10, units = "in", res = 300)
  corrplot(cor_matrix, method = "color", type = "upper", 
           order = "hclust", tl.cex = 0.8, tl.col = "black",
           title = "Correlation Matrix of Numeric Variables",
           mar = c(0,0,2,0))
  dev.off()
  
  cli_alert_success("Correlation analysis completed")
  
  return(list(
    correlation_matrix = cor_matrix,
    numeric_variables = names(numeric_vars)
  ))
}

#' Analyze A/B test results for recommendation strategies
#' @param data tibble to analyze
#' @return statistical test results
analyze_ab_test <- function(data) {
  cli_h2("Analyzing A/B test results")
  
  # Prepare data for statistical testing
  strategy_data <- data %>%
    filter(!is.na(recommendation_strategy)) %>%
    group_by(recommendation_strategy) %>%
    summarise(
      sessions = n(),
      requests = sum(ride_requested, na.rm = TRUE),
      completions = sum(ride_completed, na.rm = TRUE),
      request_rate = sum(ride_requested, na.rm = TRUE) / n(),
      completion_rate = sum(ride_completed, na.rm = TRUE) / sum(ride_requested, na.rm = TRUE),
      avg_fare = mean(fare_estimate, na.rm = TRUE),
      avg_wait_time = mean(wait_time_estimate_min, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Statistical significance testing
  # Chi-square test for conversion rates
  contingency_table <- data %>%
    select(recommendation_strategy, ride_requested) %>%
    table()
  
  chi_square_test <- chisq.test(contingency_table)
  
  # ANOVA for continuous variables
  fare_anova <- aov(fare_estimate ~ recommendation_strategy, data = data)
  wait_time_anova <- aov(wait_time_estimate_min ~ recommendation_strategy, data = data)
  
  results <- list(
    strategy_summary = strategy_data,
    chi_square_test = chi_square_test,
    fare_anova = summary(fare_anova),
    wait_time_anova = summary(wait_time_anova)
  )
  
  cli_alert_success("A/B test analysis completed")
  return(results)
}

#===============================================================================
# Business Intelligence Functions
#===============================================================================

#' Generate business insights and recommendations
#' @param data tibble to analyze
#' @param stats list with descriptive statistics
#' @param ab_results list with A/B test results
#' @return list with business insights
generate_business_insights <- function(data, stats, ab_results) {
  cli_h2("Generating business insights")
  
  insights <- list()
  
  # Top performing strategy
  best_strategy <- stats$strategy_performance %>%
    arrange(desc(completion_rate)) %>%
    slice(1)
  
  insights$top_strategy <- list(
    strategy = best_strategy$recommendation_strategy,
    completion_rate = best_strategy$completion_rate,
    sessions = best_strategy$sessions
  )
  
  # Regional performance analysis
  regional_performance <- data %>%
    group_by(region) %>%
    summarise(
      sessions = n(),
      request_rate = sum(ride_requested, na.rm = TRUE) / n() * 100,
      completion_rate = sum(ride_completed, na.rm = TRUE) / sum(ride_requested, na.rm = TRUE) * 100,
      avg_fare = mean(fare_estimate, na.rm = TRUE),
      surge_rate = sum(is_surge_pricing, na.rm = TRUE) / n() * 100,
      .groups = "drop"
    ) %>%
    arrange(desc(completion_rate))
  
  insights$regional_insights <- regional_performance
  
  # Time-based optimization opportunities
  hourly_analysis <- data %>%
    filter(!is.na(session_hour)) %>%
    group_by(session_hour) %>%
    summarise(
      sessions = n(),
      request_rate = sum(ride_requested, na.rm = TRUE) / n() * 100,
      avg_wait_time = mean(wait_time_estimate_min, na.rm = TRUE),
      surge_rate = sum(is_surge_pricing, na.rm = TRUE) / n() * 100,
      .groups = "drop"
    ) %>%
    arrange(desc(request_rate))
  
  insights$time_optimization <- hourly_analysis
  
  # User segment analysis
  user_segment_analysis <- data %>%
    group_by(user_age_group) %>%
    summarise(
      sessions = n(),
      request_rate = sum(ride_requested, na.rm = TRUE) / n() * 100,
      completion_rate = sum(ride_completed, na.rm = TRUE) / sum(ride_requested, na.rm = TRUE) * 100,
      avg_fare = mean(fare_estimate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(completion_rate))
  
  insights$user_segments <- user_segment_analysis
  
  # Key recommendations
  insights$recommendations <- list(
    strategy_optimization = glue("Consider focusing on {best_strategy$recommendation_strategy} strategy which shows {round(best_strategy$completion_rate, 1)}% completion rate"),
    regional_focus = glue("Region {regional_performance$region[1]} shows highest completion rate at {round(regional_performance$completion_rate[1], 1)}%"),
    time_optimization = glue("Peak request hours are {paste(hourly_analysis$session_hour[1:3], collapse = ', ')} with request rates above {round(mean(hourly_analysis$request_rate[1:3]), 1)}%"),
    user_targeting = glue("Age group {user_segment_analysis$user_age_group[1]} shows highest completion rate at {round(user_segment_analysis$completion_rate[1], 1)}%")
  )
  
  cli_alert_success("Business insights generated")
  return(insights)
}

#===============================================================================
# Report Generation
#===============================================================================

#' Generate comprehensive analysis report
#' @param data tibble with cleaned data
#' @param stats list with descriptive statistics
#' @param insights list with business insights
#' @param ab_results list with A/B test results
generate_report <- function(data, stats, insights, ab_results) {
  cli_h2("Generating analysis report")
  
  # Create reports directory
  dir_create("results/reports")
  
  report_file <- "results/reports/analysis_report.txt"
  
  # Write report header
  cat("=== RIDE-HAILING DATA ANALYSIS REPORT ===\n", file = report_file)
  cat("Generated:", as.character(Sys.time()), "\n\n")
  
  # Dataset overview
  cat("## DATASET OVERVIEW\n", file = report_file, append = TRUE)
  cat("Total Sessions:", stats$dataset_info$total_rows, "\n", file = report_file, append = TRUE)
  cat("Unique Users:", stats$dataset_info$unique_users, "\n", file = report_file, append = TRUE)
  cat("Date Range:", stats$dataset_info$date_range, "\n\n", file = report_file, append = TRUE)
  
  # Conversion metrics
  cat("## CONVERSION METRICS\n", file = report_file, append = TRUE)
  cat("Request Rate:", stats$conversion_metrics$request_rate, "%\n", file = report_file, append = TRUE)
  cat("Completion Rate:", stats$conversion_metrics$completion_rate, "%\n", file = report_file, append = TRUE)
  cat("Cancellation Rate:", stats$conversion_metrics$cancellation_rate, "%\n\n", file = report_file, append = TRUE)
  
  # Strategy performance
  cat("## RECOMMENDATION STRATEGY PERFORMANCE\n", file = report_file, append = TRUE)
  write.table(stats$strategy_performance, file = report_file, append = TRUE, 
              row.names = FALSE, sep = "\t")
  cat("\n", file = report_file, append = TRUE)
  
  # Business insights
  cat("## KEY INSIGHTS & RECOMMENDATIONS\n", file = report_file, append = TRUE)
  for (i in seq_along(insights$recommendations)) {
    cat(names(insights$recommendations)[i], ":", 
        insights$recommendations[[i]], "\n", file = report_file, append = TRUE)
  }
  cat("\n", file = report_file, append = TRUE)
  
  # Statistical significance
  cat("## STATISTICAL SIGNIFICANCE\n", file = report_file, append = TRUE)
  cat("Chi-square test p-value:", ab_results$chi_square_test$p.value, "\n", file = report_file, append = TRUE)
  if (ab_results$chi_square_test$p.value < 0.05) {
    cat("Result: Statistically significant differences between strategies\n", file = report_file, append = TRUE)
  } else {
    cat("Result: No statistically significant differences between strategies\n", file = report_file, append = TRUE)
  }
  
  cli_alert_success("Analysis report generated: {report_file}")
  return(report_file)
}

#===============================================================================
# Main Analysis Pipeline
#===============================================================================

#' Main data analysis pipeline
#' @return list with all analysis results
run_analysis_pipeline <- function() {
  cli_h1("Starting Ride-hailing Data Analysis Pipeline")
  
  # Load cleaned data
  data <- load_cleaned_data()
  
  # Generate descriptive statistics
  stats <- generate_descriptive_stats(data)
  
  # Create visualizations
  plots <- create_visualizations(data, stats)
  
  # Perform correlation analysis
  correlations <- analyze_correlations(data)
  
  # Analyze A/B test results
  ab_results <- analyze_ab_test(data)
  
  # Generate business insights
  insights <- generate_business_insights(data, stats, ab_results)
  
  # Generate comprehensive report
  report_file <- generate_report(data, stats, insights, ab_results)
  
  cli_h1("Data Analysis Pipeline Completed Successfully")
  cli_alert_success("All analysis components completed")
  cli_alert_info("Check plots/ directory for visualizations")
  cli_alert_info("Check reports/ directory for analysis report")
  
  return(list(
    data = data,
    statistics = stats,
    plots = plots,
    correlations = correlations,
    ab_test_results = ab_results,
    insights = insights,
    report_file = report_file
  ))
}

#===============================================================================
# Execute Analysis Pipeline
#===============================================================================

if (interactive()) {
  cli_alert_info("Running in interactive mode")
} else {
  cli_alert_info("Running as script")
}

# Execute the complete analysis pipeline
tryCatch({
  analysis_results <- run_analysis_pipeline()
  
  cli_h1("Analysis Pipeline Completed Successfully")
  cli_alert_success("All analysis components completed")
  
}, error = function(e) {
  cli_alert_danger("Analysis pipeline failed: {e$message}")
  stop(e)
})

# Return the analysis results
analysis_results
