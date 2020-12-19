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
dir.create(FIT_CACHE_DIR, showWarnings = FALSE)

# We prefer to read code in and use the "model_code" parameter for Stan, rather
# than leaving it on disk and using the "file" parameter, because Stan is quite
# keen to create .rds files of cached compiled models, which it then uses even
# if the Stan code changes. (You can disable this with 'save_dso = TRUE'. But
# it's a risk.) Either way, "#include" statements work.
STAN_SINGLE_SUBJECT_FILENAME <- file.path(
    THIS_DIR, "cgt_romeu2020_model12_single_subject.stan")
STAN_SINGLE_SUBJECT_CODE <- readr::read_file(STAN_SINGLE_SUBJECT_FILENAME)
STAN_GROUPS_FILENAME <- file.path(
    THIS_DIR, "cgt_romeu2020_model12_groups.stan")
STAN_GROUPS_CODE <- readr::read_file(STAN_GROUPS_FILENAME)


# =============================================================================
# Analyse Cambridge Gamble Task data
# =============================================================================

# -----------------------------------------------------------------------------
# Read in data in a format suitable for Stan
# -----------------------------------------------------------------------------

makeStanDataFromDataTable <- function(d, print_data = FALSE)
{
    group_names <- unique(d$group_name)
    n_groups <- length(group_names)

    subject_names <- unique(d$subject_name)
    n_subjects <- length(subject_names)

    subject_names_groups <- d[
        ,
        .(n_trials = .N),  # do something irrelevant; here, counting rows
        by = .(subject_name, group_name)
    ]
    subject_names_groups[, subject_num := match(subject_name, subject_names)]
    subject_names_groups[, group_num := match(group_name, group_names)]

    # Safety check on the subject order:
    stopifnot(subject_names_groups$subject_name == subject_names)
    stopifnot(subject_names_groups$subject_num == 1:n_subjects)

    n_trials <- nrow(d)
    stopifnot(n_trials > 1)  # General requirement

    standata <- list(
        all_subject_names = subject_names,  # not used by Stan; for safety
        all_group_names = group_names,  # not used by Stan; for safety

        N_GROUPS = n_groups,
        N_SUBJECTS = n_subjects,
        N_TRIALS = n_trials,

        # For the multi-subject version:
        group_num_by_subject = as.array(subject_names_groups$group_num),
        # ... In Stan, we declare group_num_by_subject as an array.
        #     Therefore, if it has only one value, we have to convert.
        subject_num_by_trial = match(d$subject_name, subject_names),

        ascending_bets = d$ascending_bets,
        starting_points = d$starting_points,
        n_red = d$n_red,
        chose_red = d$chose_red,
        chosen_bet_index = d$chosen_bet_index
    )
    # browser()
    if (print_data) {
        print(standata)
    }
    return(standata)
}


makeStanDataFromCsv <- function(filename)
{
    cat(paste0("- Loading: ", filename, "\n"))
    d <- data.table(read.csv(filename))
    cat("... loaded.\n")
    return(makeStanDataFromDataTable(d))
}


# -----------------------------------------------------------------------------
# Run Stan
# -----------------------------------------------------------------------------

analyseViaStan <- function(
    standata,
    model_name,
    stan_code,
    with_bridge_sampling = FALSE,
    with_diagnostics = FALSE,
    chains = 8,
    iter = 2000,
    init = "0",
    seed = 1234,
    adapt_delta = 0.8,
    stepsize = 1,
    max_treedepth = 10)
{
    cat(paste("Running model ", model_name, "...\n", sep=""))
    fit_filename <- file.path(
        FIT_CACHE_DIR, paste0("fit_", model_name, ".rds"))
    bridge_filename <- file.path(
        FIT_CACHE_DIR, paste0("bridge_", model_name, ".rds"))
    code_filename <- file.path(
        FIT_CACHE_DIR, paste0("temp_code_", model_name, ".cpp"))
    if (with_diagnostics) {
        diagnostic_file_stem <- file.path(
            FIT_CACHE_DIR, paste0("diagnostics_", model_name))
    } else {
        diagnostic_file_stem <- NULL
    }

    old_working_directory <- getwd()
    setwd(THIS_DIR)  # for Stan #include statements

    fit <- stanfunc$load_or_run_stan(
        model_code = stan_code,
        data = standata,
        fit_filename = fit_filename,
        model_name = model_name,
        save_code_filename = code_filename,
        diagnostic_file = diagnostic_file_stem,
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
        standata = standata,
        fit = fit,
        bridge_sample = bridge_sample,
        shiny = shiny
    ))
}


analyseSingleSubject <- function(standata, model_name, ...)
{
    stopifnot(standata$N_TRIALS > 1)
    return(analyseViaStan(
        standata = standata,
        model_name = model_name,
        stan_code = STAN_SINGLE_SUBJECT_CODE,
        ...
    ))
}


analyseGroups <- function(standata, model_name, ...)
{
    stopifnot(standata$N_TRIALS > 1 && standata$N_SUBJECTS > 1)
    return(analyseViaStan(
        standata = standata,
        model_name = model_name,
        stan_code = STAN_GROUPS_CODE,
        ...
    ))
}


# =============================================================================
# Test analyses.
# =============================================================================

testAnalyses <- function()
{
    # Writes to global namespace with "<<-". In general, avoid this!

    mock_data_1s <<- makeStanDataFromCsv(
        file.path(SYNTHETIC_DATA_DIR, "mock_data_rnc_1subject.csv"))
    mock_results_1s <<- analyseSingleSubject(
        standata = mock_data_1s, model_name = "cgt_mock_1_subject")

    mock_data_1g <<- makeStanDataFromCsv(
        file.path(SYNTHETIC_DATA_DIR, "mock_data_rnc_1group.csv"))
    mock_results_1g <<- analyseGroups(
        standata = mock_data_1g, model_name = "cgt_mock_1_group")

    mock_data_2g <<- makeStanDataFromCsv(
        file.path(SYNTHETIC_DATA_DIR, "mock_data_rnc_2groups.csv"))
    mock_results_2g <<- analyseGroups(
        standata = mock_data_2g, model_name = "cgt_mock_2_groups")
}


# =============================================================================
# Main entry point.
# =============================================================================

testAnalyses()
