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
library(patchwork)
library(tidyverse)
source("https://egret.psychol.cam.ac.uk/rlib/debugfunc.R")
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
OUTPUT_DIR <- file.path(THIS_DIR, "output")
dir.create(OUTPUT_DIR, showWarnings = FALSE)
OUTPUT_FILE <- file.path(OUTPUT_DIR, "output.txt")

# We prefer to read code in and use the "model_code" parameter for Stan, rather
# than leaving it on disk and using the "file" parameter, because Stan is quite
# keen to create .rds files of cached compiled models, which it then uses even
# if the Stan code changes. (You can disable this with 'save_dso = TRUE'. But
# it's a risk.) Either way, "#include" statements work.
STAN_INDEPENDENT_SUBJECTS_CODE <- readr::read_file(file.path(
    THIS_DIR, "cgt_romeu2020_model12_independent_subjects.stan"))
STAN_GROUPS_CODE <- readr::read_file(file.path(
    THIS_DIR, "cgt_romeu2020_model12_groups.stan"))


# =============================================================================
# Analyse Cambridge Gamble Task data as per [Romeu2020] figure 2.
# =============================================================================

mkRomeuFig2 <- function(d)
{
    n_locations <- 10
    working <- copy(d)
    working[, ascending := ifelse(ascending_bets, "ascending", "descending")]
    working[, red_to_blue_ratio := paste0(n_red, ":", n_locations - n_red)]
    by_subject <- (
        working %>%
        group_by(group_name, subject_name, ascending, red_to_blue_ratio) %>%
        summarize(
            average_colour_choice = mean(chose_red),
            average_bet_ratio = mean(proportion_staked),
            .groups = "drop"
            # https://stackoverflow.com/questions/62140483/how-to-interpret-dplyr-message-summarise-regrouping-output-by-x-override
        ) %>%
        as.data.table()
    )
    by_group <- (
        by_subject %>%
        group_by(group_name, ascending, red_to_blue_ratio) %>%
        summarize(
            mean_colour_choice = mean(average_colour_choice),
            sem_colour_choice = miscstat$sem(average_colour_choice),
            mean_bet_ratio = mean(average_bet_ratio),
            sem_bet_ratio = miscstat$sem(average_bet_ratio),
            .groups = "drop"
        ) %>%
        as.data.table()
    )
    theme <- theme_bw()
    fig_a <- (
        ggplot(
            by_group,
            aes(
                x = red_to_blue_ratio,
                y = mean_colour_choice,
                colour = group_name,
                group = group_name
            )
        ) +
        theme +
        scale_colour_discrete(name = "Group") +
        geom_line() +
        geom_errorbar(aes(
            ymin = mean_colour_choice - sem_colour_choice,
            ymax = mean_colour_choice + sem_colour_choice
        )) +
        geom_point() +
        facet_grid(. ~ ascending) +
        xlab("Red:blue colour ratio") +
        ylab("Colour choice (0 = blue, 1 = red)") +
        ylim(0, 1)
    )
    fig_b <- (
        ggplot(
            by_group,
            aes(
                x = red_to_blue_ratio,
                y = mean_bet_ratio,
                colour = group_name,
                group = group_name
            )
        ) +
        theme +
        scale_colour_discrete(name = "Group") +
        geom_line() +
        geom_errorbar(aes(
            ymin = mean_bet_ratio - sem_bet_ratio,
            ymax = mean_bet_ratio + sem_bet_ratio
        )) +
        geom_point() +
        facet_grid(. ~ ascending) +
        xlab("Red:blue colour ratio") +
        ylab("Bet size (proportion of points)") +
        ylim(0, 1)
    )
    f <- (fig_a / fig_b + plot_layout(guides = "collect"))
    return(f)
}


# =============================================================================
# Analyse Cambridge Gamble Task data via Bayesian modelling
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
    if (nrow(subject_names_groups) != n_subjects) {
        stop("Error: at least one subject appears in >1 group!")
    }

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


loadDataFromCsv <- function(filename)
{
    cat(paste0("- Loading: ", filename, "\n"))
    d <- data.table(read.csv(filename))
    cat("... loaded.\n")
    return(d)
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
    adapt_delta = 0.9,  # Stan default is 0.8
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


analyseIndependentSubjects <- function(standata, model_name, ...)
{
    stopifnot(standata$N_TRIALS > 1)
    return(analyseViaStan(
        standata = standata,
        model_name = model_name,
        stan_code = STAN_INDEPENDENT_SUBJECTS_CODE,
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

saveFig <- function(fig, filename_stem,
                    width_mm = 200, height_mm = 200, dpi = 600)
{
    fullpath <- file.path(OUTPUT_DIR, filename_stem)
    cat(paste0("- Saving figure: ", fullpath, " ..."))
    ggsave(
        filename = fullpath,
        plot = fig,
        width = width_mm,
        height = height_mm,
        units = "mm",
        dpi = dpi
    )
    cat(" done.\n")
}


saveOutput <- function(..., append = TRUE)
{
    debugfunc$debug_quantity(..., filename = OUTPUT_FILE, append = append,
                             print_only = TRUE)
}


analyseMockData <- function(bayesian = TRUE, figures = TRUE)
{
    # Writes to global namespace with "<<-". In general, avoid this!

    # One subject
    mock_data_1s <<- loadDataFromCsv(
        file.path(SYNTHETIC_DATA_DIR, "mock_data_rnc_1subject.csv"))
    mock_standata_1s <<- makeStanDataFromDataTable(mock_data_1s)
    if (bayesian) {
        mock_results_1s <<- analyseIndependentSubjects(
            standata = mock_standata_1s, model_name = "cgt_mock_1_subject")
        saveOutput(mock_results_1s$fit, append = FALSE)
    }

    # Two subjects
    mock_data_2s <<- loadDataFromCsv(
        file.path(SYNTHETIC_DATA_DIR, "mock_data_rnc_2subjects.csv"))
    mock_standata_2s <<- makeStanDataFromDataTable(mock_data_2s)
    if (bayesian) {
        mock_results_2s <<- analyseIndependentSubjects(
            standata = mock_standata_2s, model_name = "cgt_mock_2_subjects")
        saveOutput(mock_results_2s$fit)
    }

    # One group
    mock_data_1g <<- loadDataFromCsv(
        file.path(SYNTHETIC_DATA_DIR, "mock_data_rnc_1group.csv"))
    mock_standata_1g <<- makeStanDataFromDataTable(mock_data_1g)
    if (bayesian) {
        mock_results_1g <<- analyseGroups(
            standata = mock_standata_1g, model_name = "cgt_mock_1_group")
        saveOutput(mock_results_1g$fit)
    }
    if (figures) {
        mock_fig_1g <<- mkRomeuFig2(mock_data_1g)
        saveFig(mock_fig_1g, "mock_fig_1g.png")
    }

    # Two groups
    mock_data_2g <<- loadDataFromCsv(
        file.path(SYNTHETIC_DATA_DIR, "mock_data_rnc_2groups.csv"))
    mock_standata_2g <<- makeStanDataFromDataTable(mock_data_2g)
    if (bayesian) {
        mock_results_2g <<- analyseGroups(
            standata = mock_standata_2g, model_name = "cgt_mock_2_groups")
        saveOutput(mock_results_2g$fit)
        # Specimen group comparison
        summary_of_mock_results_2g <<- stanfunc$annotated_parameters(
            fit = mock_results_2g$fit,
            probs = c(0.025, 0.975),
            par_regex = "^group.*\\[(?:1|1,2|2)\\]$"
        )
        saveOutput(summary_of_mock_results_2g)
    }
    if (figures) {
        mock_fig_2g <<- mkRomeuFig2(mock_data_2g)
        saveFig(mock_fig_2g, "mock_fig_2g.png")
    }
}


# =============================================================================
# Main entry point.
# =============================================================================

analyseMockData()
