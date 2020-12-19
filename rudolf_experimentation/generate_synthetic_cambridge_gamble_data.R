#!/usr/bin/env Rscript

# generate_synthetic_cambridge_gamble_data.R
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


utilityFunction <- function(x, rho)
{
    # - rho: "risk aversion/seeking"; utility function power;
    #   [Romeu2020] eq. 6 but then extended; see table on p.17 of Supplementary
    #   Materials; winning model is model 12. This model is discussed on p.8.
    #
    #   <1: risk aversion (concave utility function)
    #   >1: risk seeking (convex utility function)
    stopifnot(rho >= 0)
    return(ifelse(
        x >= 0,
        log(1 + x),  # "rho = 1 for gains"
        log(1 + rho * x)  # "rho >= 0 is a free parameter for losses"
    ))
}


getPenalizedExpectedUtility <- function(
    p_win,
    current_points,
    proportion_bet,
    rho,
    bet_position,
    beta,
    n_bet_options = N_BET_OPTIONS)
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
    x_capital_after_win <- scaled_capital + stake  # [Romeu 2020], eq. 5, X
    y_capital_after_loss <- scaled_capital - stake  # [Romeu 2020], eq. 5, Y
    utility_after_win <- utilityFunction(x_capital_after_win, rho)
    utility_after_loss <- utilityFunction(y_capital_after_loss, rho)

    p_loss <- 1 - p_win
    expected_utility <- p_win * utility_after_win + p_loss * utility_after_loss
    # ... [Romeu 2020], eq. 6

    cost_of_bet_position <- (bet_position - 1) / (n_bet_options - 1)
    # [Romeu 2020], eq. 11
    # ... range [0, 1]
    stopifnot(cost_of_bet_position >= 0 & cost_of_bet_position <= 1)
    penalty_for_waiting <- beta * cost_of_bet_position
    
    return(expected_utility - penalty_for_waiting)
}


# =============================================================================
# Generate dummy data
# =============================================================================

makeSingleRun <- function(alpha, red_bias, gamma, rho, beta)
{
    # Creates simulated data for a single subject doing the task once.
    # We use model 12 of [Romeu2020], the winning model.
    #
    # - alpha: "probability distortion" parameter [Romeu2020].
    #   See pChooseRed().
    #
    # - red_bias: "colour bias" parameter, towards red; c [Romeu2020]
    #   ... but we don't call it "c" within R! Common keyword.
    #   See pChooseRed().
    #
    # - gamma: "choice consistency" parameter [Romeu2020]
    #   = softmax inverse temperature; [Romeu2020] eq. 13
    #
    # - rho: "risk aversion/seeking"; utility function power; [Romeu2020] eq. 6
    #   See utilityFunction().
    #
    # - beta: the cost of waiting: [Romeu2020], eq. 12

    stopifnot(gamma >= 0)

    # Vectors to store results
    trial_tracker <- NA
    block_tracker <- NA
    trial_within_block_tracker <- NA
    ascending_tracker <- NA

    starting_points <- NA
    n_red <- NA
    p_subject_chooses_red <- NA  # introspection
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
        bet_position <- ifelse(ascending, 1:N_BET_OPTIONS, N_BET_OPTIONS:1)
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
            proportion_of_boxes_for_chosen_colour <- ifelse(
                chose_red[trial],
                proportion_red,
                1 - proportion_red
            )

            # What are the expected utilities of each possible bet?
            penalized_expected_utility <- getPenalizedExpectedUtility(
                p_win = proportion_of_boxes_for_chosen_colour,
                current_points = starting_points[trial],
                proportion_bet = BET_FRACTION_OPTIONS,  # ... vectorized here
                rho = rho,
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
    subject_prefix = "subject_",
    group_name = "the_group",
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


# =============================================================================
# What data sets would we like to create today?
# =============================================================================

DEFAULT_ALPHA <- 3  # (roughly) [Romeu2020], fig. 3, controls
DEFAULT_RED_BIAS <- 0.5  # no bias; controls were ~0.48 in [Romeu2020], fig. 3
DEFAULT_GAMMA <- 12  # (roughly) [Romeu2020], fig. 3, controls
DEFAULT_RHO <- 1  # flat utility function; (roughly) [Romeu2020], fig. 3, controls
DEFAULT_BETA <- 0.15  # (roughly) [Romeu2020], fig. 3, controls

makeSpecimens <- function(n_subjects_per_group = 50)
{
    write.csv(
        makeMultipleSubjectsSameParameters(
            n_subjects = 1,
            alpha = DEFAULT_ALPHA,
            red_bias = DEFAULT_RED_BIAS,
            gamma = DEFAULT_GAMMA,
            rho = DEFAULT_RHO,
            beta = DEFAULT_BETA
        ),
        file = file.path(SYNTHETIC_DATA_DIR, "mock_data_rnc_1subject.csv"),
        row.names = FALSE
    )
    write.csv(
        makeMultipleSubjectsSameParameters(
            n_subjects = n_subjects_per_group,
            alpha = DEFAULT_ALPHA,
            red_bias = DEFAULT_RED_BIAS,
            gamma = DEFAULT_GAMMA,
            rho = DEFAULT_RHO,
            beta = DEFAULT_BETA
        ),
        file = file.path(SYNTHETIC_DATA_DIR, "mock_data_rnc_1group.csv"),
        row.names = FALSE
    )
    write.csv(
        rbind(
            makeMultipleSubjectsSameParameters(
                group_name = "group1",
                n_subjects = n_subjects_per_group,
                alpha = DEFAULT_ALPHA,
                red_bias = DEFAULT_RED_BIAS,
                gamma = DEFAULT_GAMMA,
                rho = DEFAULT_RHO,
                beta = DEFAULT_BETA
            ),
            makeMultipleSubjectsSameParameters(
                group_name = "group2",
                n_subjects = n_subjects_per_group,
                alpha = 2,
                red_bias = 0.6,
                gamma = 10,
                rho = 1.5,
                beta = 0.3
            )
        ),
        file = file.path(SYNTHETIC_DATA_DIR, "mock_data_rnc_2groups.csv"),
        row.names = FALSE
    )
}


# =============================================================================
# Main entry point.
# =============================================================================

makeSpecimens()
