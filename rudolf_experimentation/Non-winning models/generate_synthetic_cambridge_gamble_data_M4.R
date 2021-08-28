#!/usr/bin/env Rscript

# generate_synthetic_cambridge_gamble_data_M4.R
#
# Simulate the Cambridge Gamble Task.
# Uses the [Romeu2020] model.
#
# - Rudolf Cardinal, 18 Dec 2020, following Lukas Cincikas's previous code.
# - Naming conventions: CONSTANT, some_variable, someFunction.
# - References:
#   [Romeu2020] Romeu et al (2020, PMID 31735532).


# =============================================================================
# Libraries
# =============================================================================

library(data.table)
library(tidyverse)
source("https://egret.psychol.cam.ac.uk/rlib/miscfile.R")
source("https://egret.psychol.cam.ac.uk/rlib/miscstat.R")


# =============================================================================
# R settings
# =============================================================================

options(warn = 2)  # Warnings become errors.


# =============================================================================
# Directories
# =============================================================================

THIS_DIR <- miscfile$current_script_directory()
SYNTHETIC_DATA_DIR <- file.path(THIS_DIR, "synthetic_data")
dir.create(SYNTHETIC_DATA_DIR, showWarnings = FALSE)


# =============================================================================
# Constants
# =============================================================================

# Task features
N_LOCATIONS <- 10  # red:blue ratio ranges from 1:9 to 9:1
POINTS_AT_START_OF_BLOCK <- 100
BET_FRACTION_OPTIONS <- c(0.05, 0.25, 0.5, 0.75, 0.95)  # [Romeu2020]
N_BET_OPTIONS <- length(BET_FRACTION_OPTIONS)
# ... fraction (of your current points total) that you can bet on each trial
# ... if your total falls below this, you end the block.
MIN_BOXES_PER_SIDE <- 1  # at least one on each side [Romeu2020]

MAX_TRIALS_PER_BLOCK <- 9  # r[Romeu2020], Supplementary p.26
END_IF_POINTS_FALL_TO <- 1  # reach 1 point and you die; [Romeu2020], Supplementary p.26

N_BLOCKS <- 8


# =============================================================================
# Generic helper functions
# =============================================================================

getRandomIntegerFromInclusiveRange <- function(a, b, size = 1)
{
  # Returns a random integer in the range [a, b]. Or several of them.
  stopifnot(b >= a)
  possibilities <- a : b
  return(sample(possibilities, size = size, replace = TRUE))
}


# =============================================================================
# Task helper functions
# =============================================================================

isBlockAscending <- function(block_num)
{
  # block_num is 1-based
  return(block_num <= N_BLOCKS / 2)
  # 4 ascending, 4 descending; [Romeu2020], Supplementary p. 26-27.
  # In real life, this order is counterbalanced, but it doesn't matter here.
}


# =============================================================================
# Cognitive model functions
# =============================================================================

pChooseRed <- function(proportion_red_offered, alpha, red_bias)
{
  # After [Romeu2020], Supplementary Material, equation 3.
  #
  # - alpha: determines side/colour choice probability; is the
  #   "probability distortion" parameter (Supplementary Material, Eq. 2).
  #   Range 0-5.
  #   0 = random choice.
  #   1 = no distortion.
  #   5 = extreme preference.
  #
  # - red_bias: colour bias parameter (c) [Romeu2020]
  #   0 = will never choose red.
  #   0.5 = no bias.
  #   1 = will always choose red.
  stopifnot(length(proportion_red_offered) == 1 &&
              proportion_red_offered >= 0 &&
              proportion_red_offered <= 1)
  stopifnot(length(alpha) == 1 && alpha >= 0 && alpha <= 5)
  stopifnot(length(red_bias) == 1 && red_bias >= 0 && red_bias <= 1)
  r <- proportion_red_offered  # for notational consistency
  c <- red_bias  # for notational consistency; WARNING: overwrites R's "c()"
  u <- 1 - r  # proportion of offered boxes that are blue
  p_red <- c * r ^ alpha / (c * r ^ alpha + (1 - c) * u ^ alpha)
  return(p_red)
}


getStakeIndex <- function(bet_fraction)
{
  return(match(bet_fraction, BET_FRACTION_OPTIONS))
  # match(a, b) returns a vector: for every item in a, the corresponding
  # return value is the first index of a matching item in b (or NA if there
  # isn't one)
}


getStake <- function(current_points, proportion_bet, round = TRUE)
{
  # - round: round the number of points calculated for the stake?
  #   The real task rounds points, we think.
  #   The [Romeu2020] scaled model (Supplementary Material p5) uses
  #   fractional points, so can't scale.
  stake <- current_points * proportion_bet
  if (round) {
    stake <- round(stake)
  }
  return(stake)
}


utilityFunctionOriginal <- function(x, rho)
{
  
  stopifnot(rho >= 0)
  return(ifelse(
    x >= 0,
    log(1 + x),  # "rho = 1 for gains"
    log(1 + rho * x)  # "rho >= 0 is a free parameter for losses"
  ))
}

utilityFunction2 <- function(x, rho, delta)
{
  # Utility function log(1+x)
  stopifnot(rho >= 0)
  return((-1) * delta * log(1 + rho *x))
}

utilityFunction4 <- function(x, rho)
{
  # Utility function log(1+x*rho)
  stopifnot(rho >= 0)
  return(log(1 + rho * x))
}

getPenalizedExpectedUtility <- function(
  p_win,
  current_points,
  proportion_bet,
  rho,
  delta,
  bet_position,
  beta,
  n_bet_options = N_BET_OPTIONS,
  debug = TRUE)
{
  # [Romeu2020], eq. 4-6; Cumulative Model (CM), which was best.
  #
  # - p_win: the probability that the chosen option will in fact win
  #
  # - current_points: the points owned at the start of the trial
  #
  # - proportion_bet: the proportion of current_points that will be staked
  #   (or a vector of such proportions)
  #
  # - bet_position: the sequential order of the proportion (or a vector whose
  #   order matches proportion_bet)
  #
  # - beta: the cost of waiting: [Romeu2020], eq. 12
  #
  # - n_bet_options: the number of possible bets offered
  
  stopifnot(beta >= 0)
  
  scaled_capital <- current_points / 100  # as per [Romeu 2020]
  stake <- getStake(scaled_capital, proportion_bet, round = FALSE)
  x_capital_after_win <- stake  # [Romeu 2020], eq. 5, X
  utility_after_win <- utilityFunction4(x_capital_after_win, rho)
  utility_after_loss <- utilityFunction2(x_capital_after_win, rho, delta)
  
  p_loss <- 1 - p_win
  expected_utility <- p_win * utility_after_win + p_loss * utility_after_loss
  # ... [Romeu 2020], eq. 6
  
  cost_of_bet_position <- (bet_position - 1) / (n_bet_options - 1)
  # [Romeu 2020], eq. 11
  # ... range [0, 1]
  stopifnot(cost_of_bet_position >= 0 & cost_of_bet_position <= 1)
  penalty_for_waiting <- beta * cost_of_bet_position
  
  penalized_expected_utility <- expected_utility - penalty_for_waiting
  
  if (debug) {
    cat(
      "- getPenalizedExpectedUtility():\n",
      "  scaled_capital = ", scaled_capital, "\n",
      "  expected_utility = ", expected_utility, "\n",
      "  bet_position = ", bet_position, "\n",
      "  cost_of_bet_position = ", cost_of_bet_position, "\n",
      "  penalty_for_waiting = ", penalty_for_waiting, "\n",
      "  penalized_expected_utility = ", penalized_expected_utility, "\n"
    )
  }
  
  return(penalized_expected_utility)
}


# =============================================================================
# Generate dummy data
# =============================================================================

makeSingleRun <- function(alpha, red_bias, gamma, rho, beta, delta)
{
  
  stopifnot(gamma >= 0)
  
  # Vectors to store results
  trial_tracker <- NA
  block_tracker <- NA
  trial_within_block_tracker <- NA
  ascending_tracker <- NA
  
  starting_points <- NA
  n_red <- NA
  p_subject_chooses_red <- NA  # introspection
  chosen_colour_utility <- NA
  chose_red <- NA
  chosen_bet <- NA
  proportion_staked <- NA
  target_was_behind_red <- NA
  subject_won <- NA
  finishing_points <- NA
  
  # Short-term variables
  trial <- 1
  
  # Main choice loop
  for (block in 1 : N_BLOCKS) {
    ascending <- isBlockAscending(block)
    bet_position <- if (ascending) 1:N_BET_OPTIONS else N_BET_OPTIONS:1
    # ... do not use ifelse()!
    trial_within_block <- 1
    
    while (trial_within_block <= MAX_TRIALS_PER_BLOCK) {
      
      # Calculate starting points
      if (trial_within_block == 1) {
        start_points <- POINTS_AT_START_OF_BLOCK
      } else {
        start_points <- finishing_points[trial - 1]
      }
      if (start_points <= END_IF_POINTS_FALL_TO) {
        break
      }
      starting_points[trial] <- start_points
      
      # Record some trial variables
      trial_tracker[trial] <- trial
      block_tracker[trial] <- block
      trial_within_block_tracker[trial] <- trial_within_block
      ascending_tracker[trial] <- as.integer(ascending)
      
      # How many red/blue squares will the task offer?
      n_red[trial] <- getRandomIntegerFromInclusiveRange(
        a = MIN_BOXES_PER_SIDE,
        b = N_LOCATIONS - MIN_BOXES_PER_SIDE)
      proportion_red <- n_red[trial] / N_LOCATIONS
      
      # Which colour will the subject choose?
      p_subject_chooses_red[trial] <- pChooseRed(
        proportion_red_offered = proportion_red,
        alpha = alpha,
        red_bias = red_bias
      )
      chose_red[trial] <- as.integer(
        miscstat$coin(p = p_subject_chooses_red[trial]))
      
      # What was the proportion of locations for the *chosen* side?
      # This is the probability of winning.
      proportion_of_boxes_for_chosen_colour <-
        if (chose_red[trial]) proportion_red else 1 - proportion_red
      
      # What are the expected utilities of each possible bet?
      penalized_expected_utility <- getPenalizedExpectedUtility(
        p_win = proportion_of_boxes_for_chosen_colour,
        current_points = starting_points[trial],
        proportion_bet = BET_FRACTION_OPTIONS,  # ... vectorized here
        rho = rho,
        delta = delta,
        bet_position = bet_position,
        beta = beta,
        n_bet_options = N_BET_OPTIONS
      )
      
      # Translate to probabilities
      p_each_bet <- miscstat$softmax(penalized_expected_utility, b = gamma)
      # ... [Romeu2020], eq. 13
      
      # What bet will our subject choose?
      chosen_bet[trial] <- sample(
        1:N_BET_OPTIONS, size = 1, prob = p_each_bet)
      proportion_staked[trial] <- BET_FRACTION_OPTIONS[chosen_bet[trial]]
      
      # How many points are being bet?
      points_staked <- getStake(
        current_points = starting_points[trial],
        proportion_bet = proportion_staked[trial],
        round = TRUE
      )
      
      # Will they win?
      target_was_behind_red[trial] <- as.integer(miscstat$coin(p = proportion_red))
      
      # Did the subject win?
      subject_won[trial] <- as.integer(target_was_behind_red[trial] == chose_red[trial])
      
      # Update points according to the outcome
      if (subject_won[trial]) {
        # Win
        finishing_points[trial] <- starting_points[trial] + points_staked
      } else {
        # Loss
        finishing_points[trial] <- starting_points[trial] - points_staked
      }
      
      # Move to next trial (and/or block)
      trial <- trial + 1
      trial_within_block <- trial_within_block + 1
    }
  }
  
  mock_data <- data.table(
    overall_trial = trial_tracker,
    block = block_tracker,
    trial_within_block = trial_within_block_tracker,
    ascending_bets = ascending_tracker,
    
    starting_points = starting_points,
    finishing_points = finishing_points,
    n_red = n_red,
    chose_red = chose_red,
    proportion_staked = proportion_staked,
    chosen_bet_index = getStakeIndex(proportion_staked),  # 1-based
    target_was_behind_red = target_was_behind_red,
    subject_won = subject_won
  )
  return(mock_data)
}


makeMultipleSubjectsSameParameters <- function(
  n_subjects,
  group_name = "the_group",
  subject_prefix = paste0(group_name, "_subject"),
  ...)
{
  # - subject_prefix: prefix for each subject (prepended to a number from
  #   1:n_subjects)
  # - ...: parameters to makeSingleRun()
  
  cat(paste0("- Creating group: ", group_name, " ...\n"))
  all_subjects_data <- NULL
  for (s in 1:n_subjects) {
    subject_name <- paste0(subject_prefix, s)
    cat(paste0("- Simulating subject: ", subject_name, " ... "))
    subject_data <- makeSingleRun(...)
    cat("done.\n")
    subject_data[, subject_name := subject_name]
    subject_data[, group_name := group_name]
    all_subjects_data <- rbind(all_subjects_data, subject_data)
  }
  return(all_subjects_data)
}


makeWithinSubjectOneInstance <- function(
  group_name = "the_group",
  subject_postfix = "number",
  condition_name = "control",
  segment_number = "placeholder",
  ...)
{
  # - subject_prefix: prefix for each subject (prepended to a number from
  #   1:n_subjects)
  # - ...: parameters to makeSingleRun()
  
  cat(paste0("- Creating group: ", group_name, " ...\n"))
  subject_name <- paste0("subject_", subject_postfix)
  cat(paste0("- Simulating subject: ", subject_name, " ... "))
  subject_data <- makeSingleRun(...)
  cat("done.\n")
  subject_data[, subject_name := subject_name]
  subject_data[, group_name := group_name]
  subject_data[, condition_name := condition_name]
  subject_data[, segment_number := segment_number]
  return(subject_data)
}


# =============================================================================
# What data sets would we like to create today?
# =============================================================================

DEFAULT_ALPHA <- 3  # (roughly) [Romeu2020], fig. 3, controls
DEFAULT_RED_BIAS <- 0.5  # no bias; controls were ~0.48 in [Romeu2020], fig. 3
DEFAULT_GAMMA <- 12  # (roughly) [Romeu2020], fig. 3, controls
DEFAULT_RHO <- 1  # flat utility function; (roughly) [Romeu2020], fig. 3, controls
DEFAULT_BETA <- 0.15  # (roughly) [Romeu2020], fig. 3, controls
DEFAULT_DELTA <- 1

makeSpecimensWithinSubjects <- function(n_subjects_per_group = 25, seed = NULL)
{
  # These are just picked at random:
  secondary_alpha <- 2
  secondary_red_bias <- 0.6
  secondary_gamma <- 10
  secondary_rho <- 1.5
  secondary_beta <- 0.3
  secondary_delta <- 0.6
  
  #For within-subject design populations, these should be used:
  drug_primary_alpha <- 1
  drug_primary_red_bias <- 0.1
  drug_primary_gamma <- -3
  drug_primary_rho <- 0.4
  drug_primary_beta <- -0.1
  drug_primary_delta <- 0.5
  
  drug_secondary_alpha <- 1
  drug_secondary_red_bias <- 0.05
  drug_secondary_gamma <- -4
  drug_secondary_rho <- 0.4
  drug_secondary_beta <- -0.15
  drug_secondary_delta <- 0.6
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  data_holder <- NULL
  for(q in 1:n_subjects_per_group)
  {
    data_holder <- rbind(
      data_holder,
      makeWithinSubjectOneInstance(
        group_name = "group_1",
        subject_postfix = (2*q-1),
        condition_name = "condition_1",
        segment_number = q,
        alpha = DEFAULT_ALPHA,
        red_bias = DEFAULT_RED_BIAS,
        gamma = DEFAULT_GAMMA,
        rho = DEFAULT_RHO,
        beta = DEFAULT_BETA,
        delta = DEFAULT_DELTA
      ),
      makeWithinSubjectOneInstance(
        group_name = "group_1",
        subject_postfix = (2*q),
        condition_name = "condition_2",
        segment_number = q,
        alpha = (DEFAULT_ALPHA + drug_primary_alpha),
        red_bias = (DEFAULT_RED_BIAS + drug_primary_red_bias),
        gamma = (DEFAULT_GAMMA + drug_primary_gamma),
        rho = (DEFAULT_RHO + drug_primary_rho),
        beta = (DEFAULT_BETA + drug_primary_beta),
        delta = (DEFAULT_DELTA + drug_primary_delta)
      )
    )
    
  }
  
  for(q in 1:n_subjects_per_group)
  {
    data_holder <- rbind(
      data_holder,
      makeWithinSubjectOneInstance(
        group_name = "group_2",
        subject_postfix = (2*q-1+n_subjects_per_group*2),
        condition_name = "condition_1",
        segment_number = (q+n_subjects_per_group),
        alpha = secondary_alpha,
        red_bias = secondary_red_bias,
        gamma = secondary_gamma,
        rho = secondary_rho,
        beta = secondary_beta,
        delta = secondary_delta
      ),
      makeWithinSubjectOneInstance(
        group_name = "group_2",
        subject_postfix = (2*q+n_subjects_per_group*2),
        condition_name = "condition_2",
        segment_number = (q+n_subjects_per_group),
        alpha = (secondary_alpha + drug_secondary_alpha),
        red_bias = (secondary_red_bias + drug_secondary_red_bias),
        gamma = (secondary_gamma + drug_secondary_gamma),
        rho = (secondary_rho + drug_secondary_rho),
        beta = (secondary_beta + drug_secondary_beta),
        delta = (secondary_delta + drug_secondary_delta)
      )
    )
    
  }
  write.csv(
    data_holder,
    file = file.path(SYNTHETIC_DATA_DIR, "mock_data_rnc_2x2groups_M4.csv"),
    row.names = FALSE
  )
}


# =============================================================================
# Main entry point.
# =============================================================================

#makeSpecimens(seed = 1234)
makeSpecimensWithinSubjects(seed=1111)
