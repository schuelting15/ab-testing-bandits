#!/usr/bin/env Rscript
#===============================================================================
# Ride-hailing Multi-Armed Bandit Analysis
# Author: Data Analyst
# Date: 2025
# Description: Comprehensive multi-armed bandit analysis for recommendation strategies
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
  library(ggplot2)
  library(scales)
  library(patchwork)
  library(broom)
  library(pwr)
})

set.seed(42)  # reproducible bandit simulations

# Set working directory
# Run from the repository root, e.g.  Rscript R/01_data_cleaning.R

#===============================================================================
# Multi-Armed Bandit Analysis Functions
#===============================================================================

#' Load cleaned data for bandit analysis
#' @return tibble with cleaned data
load_bandit_data <- function(file_path = "data/processed/sessions_clean.csv") {
  cli_h2("Loading data for bandit analysis")
  
  if (!file_exists(file_path)) {
    cli_alert_danger("Cleaned data file not found: {file_path}")
    cli_alert_info("Please run 01_data_cleaning.R first")
    stop("Cleaned data not available")
  }
  
  bandit_data <- read_csv(
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
  
  cli_alert_success("Loaded {nrow(bandit_data)} rows, {ncol(bandit_data)} columns")
  return(bandit_data)
}

#===============================================================================
# Bandit Performance Metrics
#===============================================================================

#' Calculate bandit performance metrics
#' @param data tibble with bandit data
#' @return list with performance metrics
calculate_bandit_metrics <- function(data) {
  cli_h2("Calculating bandit performance metrics")
  
  # Define arms (recommendation strategies)
  arms <- c("price_first", "rating_first", "time_first")
  
  # Calculate metrics for each arm
  arm_metrics <- data %>%
    filter(!is.na(recommendation_strategy)) %>%
    group_by(recommendation_strategy) %>%
    summarise(
      # Sample size
      n_trials = n(),
      n_requests = sum(ride_requested, na.rm = TRUE),
      n_completions = sum(ride_completed, na.rm = TRUE),
      
      # Conversion rates
      request_rate = sum(ride_requested, na.rm = TRUE) / n(),
      completion_rate = sum(ride_completed, na.rm = TRUE) / sum(ride_requested, na.rm = TRUE),
      
      # Revenue metrics
      total_revenue = sum(total_fare_estimate * ride_completed, na.rm = TRUE),
      avg_revenue_per_trial = total_revenue / n(),
      avg_revenue_per_completion = total_revenue / sum(ride_completed, na.rm = TRUE),
      
      # Wait time metrics
      avg_wait_time_estimate = mean(wait_time_estimate_min, na.rm = TRUE),
      avg_wait_time_actual = mean(wait_time_actual_min, na.rm = TRUE),
      
      # User experience metrics
      avg_driver_rating = mean(driver_rating_shown, na.rm = TRUE),
      surge_pricing_rate = sum(is_surge_pricing, na.rm = TRUE) / n(),
      
      .groups = "drop"
    ) %>%
    # Calculate confidence intervals for completion rate
    mutate(
      completion_rate_se = sqrt((completion_rate * (1 - completion_rate)) / n_requests),
      completion_rate_ci_lower = completion_rate - 1.96 * completion_rate_se,
      completion_rate_ci_upper = completion_rate + 1.96 * completion_rate_se,
      
      # Request rate confidence intervals
      request_rate_se = sqrt((request_rate * (1 - request_rate)) / n_trials),
      request_rate_ci_lower = request_rate - 1.96 * request_rate_se,
      request_rate_ci_upper = request_rate + 1.96 * request_rate_se
    )
  
  # Calculate overall performance
  overall_metrics <- data %>%
    summarise(
      total_trials = n(),
      total_requests = sum(ride_requested, na.rm = TRUE),
      total_completions = sum(ride_completed, na.rm = TRUE),
      overall_request_rate = sum(ride_requested, na.rm = TRUE) / n(),
      overall_completion_rate = sum(ride_completed, na.rm = TRUE) / sum(ride_requested, na.rm = TRUE),
      total_revenue = sum(total_fare_estimate * ride_completed, na.rm = TRUE)
    )
  
  # Calculate regret metrics
  best_arm <- arm_metrics %>%
    arrange(desc(completion_rate)) %>%
    slice(1)
  
  regret_analysis <- arm_metrics %>%
    mutate(
      best_completion_rate = best_arm$completion_rate,
      regret = best_completion_rate - completion_rate,
      regret_percentage = regret / best_completion_rate * 100,
      
      # Statistical significance test (vs best arm)
      p_value = map_dbl(recommendation_strategy, ~ {
        if (.x == best_arm$recommendation_strategy) return(1.0)
        
        # Two-proportion z-test
        arm_data <- arm_metrics %>% filter(recommendation_strategy == .x)
        best_data <- best_arm
        
        p1 <- arm_data$completion_rate
        p2 <- best_data$completion_rate
        n1 <- arm_data$n_requests
        n2 <- best_data$n_requests
        
        p_pooled <- (arm_data$n_completions + best_data$n_completions) / (n1 + n2)
        se <- sqrt(p_pooled * (1 - p_pooled) * (1/n1 + 1/n2))
        z_score <- (p1 - p2) / se
        
        return(2 * (1 - pnorm(abs(z_score))))
      })
    )
  
  results <- list(
    arm_metrics = arm_metrics,
    overall_metrics = overall_metrics,
    best_arm = best_arm,
    regret_analysis = regret_analysis
  )
  
  cli_alert_success("Bandit metrics calculated")
  return(results)
}

#===============================================================================
# Statistical Testing Functions
#===============================================================================

#' Perform statistical tests for bandit analysis
#' @param data tibble with bandit data
#' @param metrics list with calculated metrics
#' @return list with statistical test results
perform_bandit_statistical_tests <- function(data, metrics) {
  cli_h2("Performing statistical tests")
  
  # Chi-square test for conversion rates
  contingency_table <- data %>%
    select(recommendation_strategy, ride_requested) %>%
    table()
  
  chi_square_request <- chisq.test(contingency_table)
  
  # Chi-square test for completion rates (among requests)
  completion_contingency <- data %>%
    filter(ride_requested == 1) %>%
    select(recommendation_strategy, ride_completed) %>%
    table()
  
  chi_square_completion <- chisq.test(completion_contingency)
  
  # ANOVA tests for continuous variables
  fare_anova <- aov(total_fare_estimate ~ recommendation_strategy, data = data)
  wait_time_anova <- aov(wait_time_estimate_min ~ recommendation_strategy, data = data)
  rating_anova <- aov(driver_rating_shown ~ recommendation_strategy, data = data)
  
  # Pairwise comparisons (Tukey's HSD)
  fare_tukey <- TukeyHSD(fare_anova)
  wait_time_tukey <- TukeyHSD(wait_time_anova)
  rating_tukey <- TukeyHSD(rating_anova)
  
  # Power analysis
  effect_sizes <- metrics$arm_metrics %>%
    select(recommendation_strategy, completion_rate, n_requests) %>%
    pivot_wider(names_from = recommendation_strategy, values_from = completion_rate) %>%
    mutate(
      effect_price_rating = abs(price_first - rating_first),
      effect_price_time = abs(price_first - time_first),
      effect_rating_time = abs(rating_first - time_first),
      max_effect = pmax(effect_price_rating, effect_price_time, effect_rating_time, na.rm = TRUE)
    )
  
  # Calculate power for the test
  total_requests <- sum(metrics$arm_metrics$n_requests)
  max_effect <- ifelse(is.na(effect_sizes$max_effect[1]) || effect_sizes$max_effect[1] <= 0, 
                       0.1, effect_sizes$max_effect[1])
  
  power_analysis <- list(
    total_sample_size = total_requests,
    max_effect_size = max_effect,
    chi_square_power = tryCatch({
      pwr.chisq.test(w = max_effect, N = total_requests, df = 2)
    }, error = function(e) {
      list(power = NA, note = "Power calculation failed")
    })
  )
  
  results <- list(
    chi_square_request = chi_square_request,
    chi_square_completion = chi_square_completion,
    fare_anova = summary(fare_anova),
    wait_time_anova = summary(wait_time_anova),
    rating_anova = summary(rating_anova),
    fare_tukey = fare_tukey,
    wait_time_tukey = wait_time_tukey,
    rating_tukey = rating_tukey,
    power_analysis = power_analysis
  )
  
  cli_alert_success("Statistical tests completed")
  return(results)
}

#===============================================================================
# Bandit Algorithm Simulation
#===============================================================================

#' Simulate different bandit algorithms
#' @param data tibble with bandit data
#' @return list with algorithm performance
simulate_bandit_algorithms <- function(data) {
  cli_h2("Simulating bandit algorithms")
  
  # Prepare data for simulation
  simulation_data <- data %>%
    filter(!is.na(recommendation_strategy), !is.na(ride_completed)) %>%
    arrange(session_date, session_id) %>%
    select(session_id, recommendation_strategy, ride_completed, total_fare_estimate) %>%
    mutate(
      reward = ifelse(ride_completed == 1, 1, 0),  # Binary reward
      revenue_reward = ifelse(ride_completed == 1, total_fare_estimate, 0)  # Revenue reward
    )
  
  # Epsilon-Greedy Algorithm
  epsilon_greedy_results <- simulate_epsilon_greedy(simulation_data, epsilon = 0.1)
  
  # Upper Confidence Bound (UCB1)
  ucb_results <- simulate_ucb1(simulation_data)
  
  # Thompson Sampling
  thompson_results <- simulate_thompson_sampling(simulation_data)
  
  # Random (baseline)
  random_results <- simulate_random_selection(simulation_data)
  
  results <- list(
    epsilon_greedy = epsilon_greedy_results,
    ucb1 = ucb_results,
    thompson_sampling = thompson_results,
    random = random_results
  )
  
  cli_alert_success("Bandit algorithm simulation completed")
  return(results)
}

#' Simulate Epsilon-Greedy algorithm
simulate_epsilon_greedy <- function(data, epsilon = 0.1) {
  arms <- as.character(unique(data$recommendation_strategy))  # names, not factor codes
  n_arms <- length(arms)
  n_trials <- nrow(data)
  
  # Initialize
  arm_counts <- setNames(rep(0, n_arms), arms)
  arm_rewards <- setNames(rep(0, n_arms), arms)
  arm_means <- setNames(rep(0, n_arms), arms)
  
  cumulative_reward <- 0
  cumulative_regret <- 0
  
  # True means (from actual data)
  true_means <- data %>%
    group_by(recommendation_strategy) %>%
    summarise(true_mean = mean(reward), .groups = "drop") %>%
    deframe()
  
  best_arm_mean <- max(true_means)
  
  # Simulate
  for (i in 1:n_trials) {
    # Choose action
    if (runif(1) < epsilon) {
      # Explore
      chosen_arm <- sample(arms, 1)
    } else {
      # Exploit
      chosen_arm <- names(arm_means)[which.max(arm_means)]
    }
    
    # Get reward (from actual data)
    # Reward of the CHOSEN arm, drawn from an environment calibrated to the observed
    # completion rates (the logged reward belongs to whichever arm was actually shown).
    reward <- rbinom(1, 1, true_means[chosen_arm])
    
    # Update estimates
    arm_counts[chosen_arm] <- arm_counts[chosen_arm] + 1
    arm_rewards[chosen_arm] <- arm_rewards[chosen_arm] + reward
    arm_means[chosen_arm] <- arm_rewards[chosen_arm] / arm_counts[chosen_arm]
    
    # Update cumulative metrics
    cumulative_reward <- cumulative_reward + reward
    cumulative_regret <- cumulative_regret + (best_arm_mean - true_means[chosen_arm])
  }
  
  return(list(
    cumulative_reward = cumulative_reward,
    cumulative_regret = cumulative_regret,
    final_arm_means = arm_means,
    arm_counts = arm_counts,
    total_trials = n_trials
  ))
}

#' Simulate UCB1 algorithm
simulate_ucb1 <- function(data) {
  arms <- as.character(unique(data$recommendation_strategy))  # names, not factor codes
  n_arms <- length(arms)
  n_trials <- nrow(data)
  
  # Initialize
  arm_counts <- setNames(rep(0, n_arms), arms)
  arm_rewards <- setNames(rep(0, n_arms), arms)
  arm_means <- setNames(rep(0, n_arms), arms)
  
  cumulative_reward <- 0
  cumulative_regret <- 0
  
  # True means
  true_means <- data %>%
    group_by(recommendation_strategy) %>%
    summarise(true_mean = mean(reward), .groups = "drop") %>%
    deframe()
  
  best_arm_mean <- max(true_means)
  
  # Simulate
  for (i in 1:n_trials) {
    # Calculate UCB values
    ucb_values <- arm_means + sqrt(2 * log(i) / pmax(arm_counts, 1))
    
    # Choose arm with highest UCB
    chosen_arm <- names(ucb_values)[which.max(ucb_values)]
    
    # Get reward
    # Reward of the CHOSEN arm, drawn from an environment calibrated to the observed
    # completion rates (the logged reward belongs to whichever arm was actually shown).
    reward <- rbinom(1, 1, true_means[chosen_arm])
    
    # Update estimates
    arm_counts[chosen_arm] <- arm_counts[chosen_arm] + 1
    arm_rewards[chosen_arm] <- arm_rewards[chosen_arm] + reward
    arm_means[chosen_arm] <- arm_rewards[chosen_arm] / arm_counts[chosen_arm]
    
    # Update cumulative metrics
    cumulative_reward <- cumulative_reward + reward
    cumulative_regret <- cumulative_regret + (best_arm_mean - true_means[chosen_arm])
  }
  
  return(list(
    cumulative_reward = cumulative_reward,
    cumulative_regret = cumulative_regret,
    final_arm_means = arm_means,
    arm_counts = arm_counts,
    total_trials = n_trials
  ))
}

#' Simulate Thompson Sampling
simulate_thompson_sampling <- function(data) {
  arms <- as.character(unique(data$recommendation_strategy))  # names, not factor codes
  n_arms <- length(arms)
  n_trials <- nrow(data)
  
  # Initialize Beta distribution parameters
  alpha <- setNames(rep(1, n_arms), arms)  # Successes + 1
  beta <- setNames(rep(1, n_arms), arms)   # Failures + 1
  
  cumulative_reward <- 0
  cumulative_regret <- 0
  
  # True means
  true_means <- data %>%
    group_by(recommendation_strategy) %>%
    summarise(true_mean = mean(reward), .groups = "drop") %>%
    deframe()
  
  best_arm_mean <- max(true_means)
  
  # Simulate
  for (i in 1:n_trials) {
    # Sample from Beta distributions
    sampled_means <- rbeta(n_arms, alpha, beta)
    names(sampled_means) <- arms
    
    # Choose arm with highest sampled mean
    chosen_arm <- names(sampled_means)[which.max(sampled_means)]
    
    # Get reward
    # Reward of the CHOSEN arm, drawn from an environment calibrated to the observed
    # completion rates (the logged reward belongs to whichever arm was actually shown).
    reward <- rbinom(1, 1, true_means[chosen_arm])
    
    # Update Beta parameters
    if (reward == 1) {
      alpha[chosen_arm] <- alpha[chosen_arm] + 1
    } else {
      beta[chosen_arm] <- beta[chosen_arm] + 1
    }
    
    # Update cumulative metrics
    cumulative_reward <- cumulative_reward + reward
    cumulative_regret <- cumulative_regret + (best_arm_mean - true_means[chosen_arm])
  }
  
  # Final means (posterior means)
  final_means <- alpha / (alpha + beta)
  
  return(list(
    cumulative_reward = cumulative_reward,
    cumulative_regret = cumulative_regret,
    final_arm_means = final_means,
    alpha = alpha,
    beta = beta,
    total_trials = n_trials
  ))
}

#' Simulate Random Selection (baseline)
simulate_random_selection <- function(data) {
  arms <- as.character(unique(data$recommendation_strategy))  # names, not factor codes
  n_arms <- length(arms)
  n_trials <- nrow(data)
  
  # True means
  true_means <- data %>%
    group_by(recommendation_strategy) %>%
    summarise(true_mean = mean(reward), .groups = "drop") %>%
    deframe()
  
  best_arm_mean <- max(true_means)
  average_mean <- mean(true_means)
  
  cumulative_reward <- 0
  cumulative_regret <- 0
  
  # Simulate random selection
  for (i in 1:n_trials) {
    chosen_arm <- sample(arms, 1)
    # Reward of the CHOSEN arm, drawn from an environment calibrated to the observed
    # completion rates (the logged reward belongs to whichever arm was actually shown).
    reward <- rbinom(1, 1, true_means[chosen_arm])
    
    cumulative_reward <- cumulative_reward + reward
    cumulative_regret <- cumulative_regret + (best_arm_mean - true_means[chosen_arm])
  }
  
  return(list(
    cumulative_reward = cumulative_reward,
    cumulative_regret = cumulative_regret,
    total_trials = n_trials
  ))
}

#===============================================================================
# Visualization Functions
#===============================================================================

#' Create bandit analysis visualizations
#' @param metrics list with calculated metrics
#' @param statistical_tests list with test results
#' @param algorithm_results list with algorithm simulation results
create_bandit_visualizations <- function(metrics, statistical_tests, algorithm_results) {
  cli_h2("Creating bandit visualizations")
  
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
  
  # 1. Completion Rate with Confidence Intervals
  p1 <- ggplot(metrics$arm_metrics, aes(x = recommendation_strategy, y = completion_rate)) +
    geom_col(fill = "steelblue", alpha = 0.7) +
    geom_errorbar(aes(ymin = completion_rate_ci_lower, ymax = completion_rate_ci_upper),
                  width = 0.2, color = "darkred") +
    geom_text(aes(label = paste0(round(completion_rate * 100, 1), "%")), 
              vjust = -0.5, size = 4) +
    labs(
      title = "Bandit Arm Performance: Completion Rates",
      subtitle = "With 95% confidence intervals",
      x = "Recommendation Strategy (Arm)",
      y = "Completion Rate"
    ) +
    scale_y_continuous(labels = percent_format())
  
  # 2. Regret Analysis
  p2 <- ggplot(metrics$regret_analysis, aes(x = recommendation_strategy, y = regret_percentage)) +
    geom_col(fill = "orange", alpha = 0.7) +
    geom_text(aes(label = paste0(round(regret_percentage, 1), "%")), 
              vjust = -0.5, size = 4) +
    labs(
      title = "Regret Analysis",
      subtitle = "Percentage regret vs. best arm",
      x = "Recommendation Strategy (Arm)",
      y = "Regret (%)"
    ) +
    scale_y_continuous(labels = percent_format(scale = 1))
  
  # 3. Sample Size Distribution
  p3 <- ggplot(metrics$arm_metrics, aes(x = recommendation_strategy, y = n_trials)) +
    geom_col(fill = "green", alpha = 0.7) +
    geom_text(aes(label = scales::comma(n_trials)), vjust = -0.5, size = 4) +
    labs(
      title = "Sample Size Distribution",
      subtitle = "Number of trials per arm",
      x = "Recommendation Strategy (Arm)",
      y = "Number of Trials"
    ) +
    scale_y_continuous(labels = comma_format())
  
  # 4. Revenue Performance
  p4 <- ggplot(metrics$arm_metrics, aes(x = recommendation_strategy, y = avg_revenue_per_trial)) +
    geom_col(fill = "purple", alpha = 0.7) +
    geom_text(aes(label = paste0("$", round(avg_revenue_per_trial, 2))), 
              vjust = -0.5, size = 4) +
    labs(
      title = "Revenue Performance",
      subtitle = "Average revenue per trial",
      x = "Recommendation Strategy (Arm)",
      y = "Average Revenue per Trial ($)"
    ) +
    scale_y_continuous(labels = dollar_format())
  
  # 5. Algorithm Comparison
  algorithm_summary <- data.frame(
    algorithm = c("Epsilon-Greedy", "UCB1", "Thompson Sampling", "Random"),
    cumulative_reward = c(
      algorithm_results$epsilon_greedy$cumulative_reward,
      algorithm_results$ucb1$cumulative_reward,
      algorithm_results$thompson_sampling$cumulative_reward,
      algorithm_results$random$cumulative_reward
    ),
    cumulative_regret = c(
      algorithm_results$epsilon_greedy$cumulative_regret,
      algorithm_results$ucb1$cumulative_regret,
      algorithm_results$thompson_sampling$cumulative_regret,
      algorithm_results$random$cumulative_regret
    )
  )
  
  p5 <- ggplot(algorithm_summary, aes(x = algorithm, y = cumulative_reward)) +
    geom_col(fill = "darkblue", alpha = 0.7) +
    geom_text(aes(label = round(cumulative_reward)), vjust = -0.5, size = 4) +
    labs(
      title = "Algorithm Performance Comparison",
      subtitle = "Cumulative reward over all trials",
      x = "Algorithm",
      y = "Cumulative Reward"
    ) +
    scale_y_continuous(labels = comma_format())
  
  # 6. Regret Comparison
  p6 <- ggplot(algorithm_summary, aes(x = algorithm, y = cumulative_regret)) +
    geom_col(fill = "darkred", alpha = 0.7) +
    geom_text(aes(label = round(cumulative_regret, 1)), vjust = -0.5, size = 4) +
    labs(
      title = "Algorithm Regret Comparison",
      subtitle = "Cumulative regret over all trials",
      x = "Algorithm",
      y = "Cumulative Regret"
    ) +
    scale_y_continuous(labels = comma_format())
  
  # Save individual plots
  ggsave("results/plots/09_bandit_completion_rates.png", p1, width = 10, height = 6, dpi = 300)
  ggsave("results/plots/10_bandit_regret_analysis.png", p2, width = 10, height = 6, dpi = 300)
  ggsave("results/plots/11_bandit_sample_sizes.png", p3, width = 10, height = 6, dpi = 300)
  ggsave("results/plots/12_bandit_revenue_performance.png", p4, width = 10, height = 6, dpi = 300)
  ggsave("results/plots/13_algorithm_comparison.png", p5, width = 10, height = 6, dpi = 300)
  ggsave("results/plots/14_algorithm_regret.png", p6, width = 10, height = 6, dpi = 300)
  
  # Create combined dashboard
  dashboard <- (p1 | p2) / (p3 | p4) / (p5 | p6)
  ggsave("results/plots/15_bandit_dashboard.png", dashboard, width = 16, height = 12, dpi = 300)
  
  cli_alert_success("Bandit visualizations created and saved to plots/ directory")
  
  return(list(
    completion_rates = p1,
    regret_analysis = p2,
    sample_sizes = p3,
    revenue_performance = p4,
    algorithm_comparison = p5,
    algorithm_regret = p6,
    dashboard = dashboard
  ))
}

#===============================================================================
# Report Generation
#===============================================================================

#' Generate comprehensive bandit analysis report
#' @param metrics list with calculated metrics
#' @param statistical_tests list with test results
#' @param algorithm_results list with algorithm simulation results
generate_bandit_report <- function(metrics, statistical_tests, algorithm_results) {
  cli_h2("Generating bandit analysis report")
  
  # Create reports directory
  dir_create("results/reports")
  
  report_file <- "results/reports/multi_armed_bandit_report.txt"
  
  # Write report header
  cat("=== MULTI-ARMED BANDIT ANALYSIS REPORT ===\n", file = report_file)
  cat("Generated:", as.character(Sys.time()), "\n\n")
  
  # Executive Summary
  cat("## EXECUTIVE SUMMARY\n", file = report_file, append = TRUE)
  best_arm <- metrics$best_arm
  cat("Best Performing Arm:", as.character(best_arm$recommendation_strategy), "\n", 
      file = report_file, append = TRUE)
  cat("Best Completion Rate:", round(best_arm$completion_rate * 100, 2), "%\n", 
      file = report_file, append = TRUE)
  cat("Total Trials:", scales::comma(metrics$overall_metrics$total_trials), "\n", 
      file = report_file, append = TRUE)
  cat("Total Revenue:", paste0("$", round(metrics$overall_metrics$total_revenue, 2)), "\n\n", 
      file = report_file, append = TRUE)
  
  # Arm Performance
  cat("## ARM PERFORMANCE METRICS\n", file = report_file, append = TRUE)
  write.table(metrics$arm_metrics, file = report_file, append = TRUE, 
              row.names = FALSE, sep = "\t")
  cat("\n", file = report_file, append = TRUE)
  
  # Statistical Significance
  cat("## STATISTICAL SIGNIFICANCE\n", file = report_file, append = TRUE)
  cat("Chi-square test for completion rates (p-value):", 
      round(statistical_tests$chi_square_completion$p.value, 6), "\n", 
      file = report_file, append = TRUE)
  
  if (statistical_tests$chi_square_completion$p.value < 0.05) {
    cat("Result: Statistically significant differences between arms\n", 
        file = report_file, append = TRUE)
  } else {
    cat("Result: No statistically significant differences between arms\n", 
        file = report_file, append = TRUE)
  }
  
  # Regret Analysis
  cat("\n## REGRET ANALYSIS\n", file = report_file, append = TRUE)
  write.table(metrics$regret_analysis, file = report_file, append = TRUE, 
              row.names = FALSE, sep = "\t")
  cat("\n", file = report_file, append = TRUE)
  
  # Algorithm Performance
  cat("## ALGORITHM PERFORMANCE\n", file = report_file, append = TRUE)
  cat("Algorithm\tCumulative Reward\tCumulative Regret\n", file = report_file, append = TRUE)
  cat("Epsilon-Greedy\t", algorithm_results$epsilon_greedy$cumulative_reward, 
      "\t", algorithm_results$epsilon_greedy$cumulative_regret, "\n", 
      file = report_file, append = TRUE)
  cat("UCB1\t", algorithm_results$ucb1$cumulative_reward, 
      "\t", algorithm_results$ucb1$cumulative_regret, "\n", 
      file = report_file, append = TRUE)
  cat("Thompson Sampling\t", algorithm_results$thompson_sampling$cumulative_reward, 
      "\t", algorithm_results$thompson_sampling$cumulative_regret, "\n", 
      file = report_file, append = TRUE)
  cat("Random\t", algorithm_results$random$cumulative_reward, 
      "\t", algorithm_results$random$cumulative_regret, "\n", 
      file = report_file, append = TRUE)
  cat("\n", file = report_file, append = TRUE)
  
  # Recommendations
  cat("## RECOMMENDATIONS\n", file = report_file, append = TRUE)
  cat("1. Primary Recommendation: Deploy", as.character(best_arm$recommendation_strategy), 
      "as the default strategy\n", file = report_file, append = TRUE)
  cat("2. Statistical Confidence: ", 
      ifelse(statistical_tests$chi_square_completion$p.value < 0.05, 
             "High confidence in differences", "Low confidence in differences"), 
      "\n", file = report_file, append = TRUE)
  regrets <- sapply(algorithm_results, function(r) unname(r$cumulative_regret))
  cat("3. Algorithm Choice:", c(epsilon_greedy = "Epsilon-Greedy", ucb1 = "UCB1",
      thompson_sampling = "Thompson Sampling", random = "Random")[names(which.min(regrets))],
      "has the lowest cumulative regret in the simulation\n", file = report_file, append = TRUE)
  pw <- statistical_tests$power_analysis$chi_square_power$power
  cat("4. Sample Size: power of the chi-square test for the largest observed difference =",
      ifelse(is.null(pw) || is.na(pw), "n/a", round(pw, 3)), "\n", file = report_file, append = TRUE)
  
  cli_alert_success("Bandit analysis report generated: {report_file}")
  return(report_file)
}

#===============================================================================
# Main Bandit Analysis Pipeline
#===============================================================================

#' Main multi-armed bandit analysis pipeline
#' @return list with all analysis results
run_bandit_analysis_pipeline <- function() {
  cli_h1("Starting Multi-Armed Bandit Analysis Pipeline")
  
  # Load data
  data <- load_bandit_data()
  
  # Calculate bandit metrics
  metrics <- calculate_bandit_metrics(data)
  
  # Perform statistical tests
  statistical_tests <- perform_bandit_statistical_tests(data, metrics)
  
  # Simulate bandit algorithms
  algorithm_results <- simulate_bandit_algorithms(data)
  
  # Create visualizations
  plots <- create_bandit_visualizations(metrics, statistical_tests, algorithm_results)
  
  # Generate report
  report_file <- generate_bandit_report(metrics, statistical_tests, algorithm_results)
  
  cli_h1("Multi-Armed Bandit Analysis Pipeline Completed Successfully")
  cli_alert_success("All bandit analysis components completed")
  cli_alert_info("Check plots/ directory for bandit visualizations")
  cli_alert_info("Check reports/ directory for bandit analysis report")
  
  return(list(
    data = data,
    metrics = metrics,
    statistical_tests = statistical_tests,
    algorithm_results = algorithm_results,
    plots = plots,
    report_file = report_file
  ))
}

#===============================================================================
# Execute Bandit Analysis Pipeline
#===============================================================================

if (interactive()) {
  cli_alert_info("Running in interactive mode")
} else {
  cli_alert_info("Running as script")
}

# Execute the complete bandit analysis pipeline
tryCatch({
  bandit_results <- run_bandit_analysis_pipeline()
  
  cli_h1("Bandit Analysis Pipeline Completed Successfully")
  cli_alert_success("Multi-armed bandit analysis completed")
  
}, error = function(e) {
  cli_alert_danger("Bandit analysis pipeline failed: {e$message}")
  stop(e)
})

# Return the bandit analysis results
bandit_results
