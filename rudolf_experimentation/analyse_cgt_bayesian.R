#!/usr/bin/env Rscript

# Analyse Cambridge Gamble Task data.
# Uses the [Romeu2020] model.
#
# - Rudolf Cardinal, 18 Dec 2020.
# - Naming conventions: CONSTANT, some_variable, someFunction.
# - References:
#   [Romeu2020] Romeu et al (2020, PMID 31735532).




# =============================================================================
# Libraries
# =============================================================================

library(data.table)
library(tidyverse)
source("https://egret.psychol.cam.ac.uk/rlib/listassign.R")
source("https://egret.psychol.cam.ac.uk/rlib/miscfile.R")
source("https://egret.psychol.cam.ac.uk/rlib/miscstat.R")
source("https://egret.psychol.cam.ac.uk/rlib/stanfunc.R")


# =============================================================================
# R settings
# =============================================================================

options(warn = 2)  # Warnings become errors.

# As advised by Stan:
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# =============================================================================
# Directories, filenames
# =============================================================================

THIS_DIR <- miscfile$current_script_directory()
SYNTHETIC_DATA_DIR <- file.path(THIS_DIR, "synthetic_data")
FIT_CACHE_DIR <- file.path(THIS_DIR, "fitcache")

STAN_SINGLE_SUBJECT_FILENAME <- file.path(
    THIS_DIR, "cgt_romeu2020_model12_single_subject.stan")
STAN_SINGLE_SUBJECT_CODE <- readr::read_file(STAN_SINGLE_SUBJECT_FILENAME)


# =============================================================================
# Analyse Cambridge Gamble Task data
# =============================================================================

analyseSingleSubjectFromDataTable <- function(
    d,
    model_name,
    with_bridge_sampling = FALSE,
    chains = 8,
    iter = 2000,
    init = "0",
    seed = 1234,
    adapt_delta = 0.8,
    stepsize = 1,
    max_treedepth = 10)
{
    standata <- list(
        N_TRIALS = nrow(d),
        ascending_bets = d$ascending_bets,
        starting_points = d$starting_points,
        n_red = d$n_red,
        chose_red = d$chose_red,
        chosen_bet_index = d$chosen_bet_index
    )
    # browser()

    cat(paste("Running model ", model_name, "...\n", sep=""))
    fit_filename <- file.path(
        FIT_CACHE_DIR, paste0("fit_", model_name, ".rds"))
    bridge_filename <- file.path(
        FIT_CACHE_DIR, paste0("bridge_", model_name, ".rds"))
    code_filename <- file.path(
        FIT_CACHE_DIR, paste0("temp_code_", model_name, ".cpp"))
    diagnostic_file <- file.path(
        FIT_CACHE_DIR, paste0("diagnostics_", model_name, ".txt"))

    old_working_directory <- getwd()
    setwd(THIS_DIR)  # for Stan #include statements

    fit <- stanfunc$load_or_run_stan(
        data = standata,
        model_code = STAN_SINGLE_SUBJECT_CODE,
        fit_filename = fit_filename,
        model_name = model_name,
        save_code_filename = code_filename,
        diagnostic_file = diagnostic_file,
        chains = chains,
        iter = iter,
        init = init,
        seed = seed,
        control = list(
            adapt_delta = adapt_delta,
            stepsize = stepsize,
            max_treedepth = max_treedepth
        )
    )

    # View the model in ShinyStan
    cat("Making ShinyStan object...\n")
    shiny <- shinystan::as.shinystan(fit)
    cat("... made\n")
    # shinystan::launch_shinystan(ss)

    if (with_bridge_sampling) {
        bridge_sample <- stanfunc$load_or_run_bridge_sampler(
            stanfit = fit,
            filename = bridge_filename,
            model_code = code,
            data = standata
        )
    } else {
        bridge_sample <- NULL
    }

    setwd(old_working_directory)

    return(list(
        fit = fit,
        bridge_sample = bridge_sample,
        shiny = shiny
    ))
}


analyseSingleSubjectFromCsv <- function(filename, model_name)
{
    cat(paste0("- Loading: ", filename, "\n"))
    d <- data.table(read.csv(filename))
    return(analyseSingleSubjectFromDataTable(d, model_name))
}


# =============================================================================
# Main entry point.
# =============================================================================

results_1 <- analyseSingleSubjectFromCsv(
    filename = file.path(SYNTHETIC_DATA_DIR, "mock_data_rnc_1.csv"),
    model_name = "s1"
)
