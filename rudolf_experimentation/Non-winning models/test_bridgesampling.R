#!/usr/bin/env Rscript
#
# RNC Dec 2017

# =============================================================================
# Command-line arguments
# =============================================================================

possible_cmd_args <- c(
    "all",
    "dummy_bridge_good", "dummy_bridge_bad",
    "unimodal_data_unimodal_model",
    "unimodal_data_bimodal_2sd_model",
    "unimodal_data_bimodal_1sd_model",
    "bimodal_1sd_data_unimodal_model",
    "bimodal_1sd_data_bimodal_2sd_model",
    "bimodal_1sd_bimodal_1sd_model",
    "bimodal_2sd_data_unimodal_model",
    "bimodal_2sd_data_bimodal_2sd_model",
    "bimodal_2sd_data_bimodal_2sd_model_single_sd",
    "demo_bad_parameterization",
    "noncentred_test1", "noncentred_test2", "noncentred_test3",
    "noncentred_test4"
)
cmdargs <- commandArgs(trailingOnly=TRUE)
cat("Command-line arguments:", paste(cmdargs, collapse=" "), "\n")
if (!all(cmdargs %in% possible_cmd_args)) {
    stop("Invalid arguments; must be one or more of: ",
         paste(possible_cmd_args, collapse=", "))
}

# =============================================================================
# Libraries
# =============================================================================

library(Rcpp)  # or Stan will fail with: could not find function "cpp_object_initializer"  -- also, load it early on osprey (version conflicts)

library(bridgesampling)
library(data.table)
library(ggplot2)
library(gridExtra)
library(parallel)
library(plyr)
library(readr)
library(rstan)
library(shinystan)

# Remote access:
RLIB_PREFIX = "http://egret.psychol.cam.ac.uk/rlib"
# Local access:
# RLIB_PREFIX = "/home/rudolf/Documents/code/rlib"

source(paste(RLIB_PREFIX, "listassign.R", sep="/"))
source(paste(RLIB_PREFIX, "miscfile.R", sep="/"))
source(paste(RLIB_PREFIX, "miscmath.R", sep="/"))
source(paste(RLIB_PREFIX, "miscstat.R", sep="/"))
source(paste(RLIB_PREFIX, "rpm.R", sep="/"))
source(paste(RLIB_PREFIX, "stanfunc.R", sep="/"))

CODE_COMMON_FUNCTIONS <- readr::read_file(paste(
    RLIB_PREFIX, "commonfunc.stan", sep="/"))

# As advised by Stan:

rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())  # or Stan won't go parallel by default

# =============================================================================
# Directories
# =============================================================================

THIS_SCRIPT_DIR <- miscfile$current_script_directory()
setwd(THIS_SCRIPT_DIR)
FIT_CACHE_DIR <- paste(THIS_SCRIPT_DIR, "fitcache", sep="/")
if (!file.exists(FIT_CACHE_DIR)) {
    dir.create(FIT_CACHE_DIR)
}
OUTPUT_DIR <- paste(THIS_SCRIPT_DIR, "output", sep="/")
if (!file.exists(OUTPUT_DIR)) {
    dir.create(OUTPUT_DIR)
}


# =============================================================================
# Seed RNG
# =============================================================================

set.seed(999)  # for consistency!

# =============================================================================
# Testing bridge_sampler
# =============================================================================

DUMMY_BRIDGE_CODE_BAD <- "

    // For this one, bridge_sampler() will provide output, but it will be wrong.
    // The sampling statements need to be changed; see DUMMY_BRIDGE_CODE_GOOD.
    // See
    // (a) The 'bridgesampling' manual, p. 15.
    // (b) The Stan manual (v2.16.0), p. 78-79, which explicitly deals with the
    //     differences between the sampling ('var ~ dist(...)')  and
    //     'target += dist_lpdf(var | ...)' notations.

    data {
        int<lower=0> N;
        real<lower=0> y[N];
    }
    parameters {
        real mu;
        real<lower=0> sigma;
    }
    model {
        mu ~ normal(0, 100);
        sigma ~ cauchy(0, 10);  // half-Cauchy (as constrained to be positive)
        y ~ normal(mu, sigma);
    }
"
DUMMY_BRIDGE_CODE_GOOD <- "
    data {
        int<lower=0> N;
        real<lower=0> y[N];
    }
    parameters {
        real mu;
        real<lower=0> sigma;
    }
    model {
        target += normal_lpdf(mu | 0, 100);
        target += cauchy_lpdf(sigma | 0, 10) -
                  cauchy_lccdf(0 | 0, 10);  // lower bound of 0
        target += normal_lpdf(y | mu, sigma);

        // y ~ normal(mu, sigma);  // the equivalent in plain Stan.
    }
"
DUMMY_BRIDGE_DATA <- list(
    N = 30,
    # generated with dput(rnorm(30, mean=27, sd=5))
    y = c(
        35.7803799074715, 21.228991468146, 33.1090329253268, 25.5535980833693,
        26.8435806451285, 38.9839460360288, 32.6958517445998, 27.5728333102714,
        22.0664328702194, 26.2038511138431, 23.9620797014717, 25.3144278312363,
        25.6846226513005, 28.7575665769027, 36.4363397288354, 30.3891545252888,
        29.0069836646559, 33.2811622831038, 21.3772352248602, 22.9219419770314,
        23.9961631923664, 30.1030071923881, 26.6327909584684, 17.0523672682594,
        28.32278628163, 26.6759679078499, 13.437687518426, 18.6363288612264,
        39.5369123095488, 19.1798085289138)
)

# =============================================================================
# A model comparison example
# =============================================================================
# Hunting for an example
#   http://mlg.eng.cam.ac.uk/zoubin/course05/lect9ms.pdf
# No clear ones found except a discrete one in Kruschke "Doing Bayesian Data
#   Analysis" p67 (coins).
# Let's pick one: unimodal vs bimodal.
#
# See also
#   http://benediktehinger.de/blog/science/how-to-use-bimodal-priors-for-bayesian-data-analysis-in-stan/
#   https://groups.google.com/forum/#!topic/stan-users/swku9w9fnKs

N <- 1000
UNIMODAL_MEAN = 40
UNIMODAL_SD = 7
BIMODAL_MEAN_1 = 30
BIMODAL_MEAN_2 = 50
BIMODAL_SD_1 = 4
BIMODAL_SD_2 = 9

DATA_UNIMODAL <- list(
    N = N,
    y = rnorm(N, mean=UNIMODAL_MEAN, sd=UNIMODAL_SD)
)

DATA_BIMODAL_A <- rnorm(N, mean=BIMODAL_MEAN_1, sd=BIMODAL_SD_1)
DATA_BIMODAL_B <- rnorm(N, mean=BIMODAL_MEAN_2, sd=BIMODAL_SD_1)
# and the mixture:
MIXTURE_FRACTION <- 0.4
SPLIT <- round(MIXTURE_FRACTION * N)
DATA_BIMODAL_A_CONTRIBUTION <- DATA_BIMODAL_A[1:SPLIT]
DATA_BIMODAL_B_CONTRIBUTION <- DATA_BIMODAL_B[(SPLIT + 1):N]
DATA_BIMODAL_ONESD <- list(
    N = N,
    y = c(
        DATA_BIMODAL_A_CONTRIBUTION,
        DATA_BIMODAL_B_CONTRIBUTION
    )
)
DATA_BIMODAL_C <- rnorm(N, mean=BIMODAL_MEAN_2, sd=BIMODAL_SD_2)
DATA_BIMODAL_C_CONTRIBUTION <- DATA_BIMODAL_C[(SPLIT + 1):N]
DATA_BIMODAL_TWOSD <- list(
    N = N,
    y = c(
        DATA_BIMODAL_A_CONTRIBUTION,
        DATA_BIMODAL_C_CONTRIBUTION
    )
)
UNIMODAL_MODEL_CODE <- "
    data {
        int<lower=0> N;
        real y[N];
    }
    parameters {
        real mu;
        real<lower=0> sigma;
    }
    model {
        // EASY TO READ, but doesn't allow bridge sampling
        // Priors:
        // mu ~ normal(0, 100);
        // sigma ~ cauchy(0, 10);

        // Data drawn from a normal distribution with mean mu, SD sigma:
        // y ~ normal(mu, sigma);

        // BRIDGE SAMPLING NOTATION:
        target += normal_lpdf(mu | 0, 100);
        target += cauchy_lpdf(sigma | 0, 10) -
                  cauchy_lccdf(0 | 0, 10);  // lower bound of 0
        target += normal_lpdf(y | mu, sigma);
    }
"
# The next two are wrong: because they don't constrain mu2 > mu1, the
# values for the two hop around (alternate). High R-hat, duff estimates.
BIMODAL_2SD_MODEL_CODE_WRONG <- "
    data {
        int<lower=0> N;
        real y[N];
    }
    parameters {
        real mu[2];
        real<lower=0> sigma[2];
        real<lower=0, upper=1> lambda;  // mixture proportion
    }
    model {
        target += normal_lpdf(mu | 0, 100);
        target += cauchy_lpdf(sigma | 0, 10) -
                  cauchy_lccdf(0 | 0, 10);  // lower bound of 0
        // Stan v2.16.0 manual p194:
        for (n in 1:N) {
            target += log_mix(lambda,
                              normal_lpdf(y[n] | mu[1], sigma[1]),
                              normal_lpdf(y[n] | mu[2], sigma[2]));
        }
    }
"
BIMODAL_1SD_MODEL_CODE_WRONG <- "
    data {
        int<lower=0> N;
        real y[N];
    }
    parameters {
        real mu[2];
        real<lower=0> sigma;
        real<lower=0, upper=1> lambda;  // mixture proportion
    }
    model {
        target += normal_lpdf(mu | 0, 100);
        target += cauchy_lpdf(sigma | 0, 10) -
                  cauchy_lccdf(0 | 0, 10);  // lower bound of 0
        for (n in 1:N) {
            target += log_mix(lambda,
                              normal_lpdf(y[n] | mu[1], sigma),
                              normal_lpdf(y[n] | mu[2], sigma));
        }
    }
"
BIMODAL_2SD_MODEL_CODE <- "
    data {
        int<lower=0> N;
        real y[N];
    }
    parameters {
        real mu1;
        real<lower=0> mu2_minus_mu1;
        real<lower=0> sigma[2];
        real<lower=0, upper=1> lambda;  // mixture proportion
    }
    model {
        target += normal_lpdf(mu1 | 0, 100);
        target += normal_lpdf(mu2_minus_mu1 | 0, 100) -
                  normal_lccdf(0 | 0, 100);  // lower bound of 0;
        target += cauchy_lpdf(sigma | 0, 10) -
                  cauchy_lccdf(0 | 0, 10);  // lower bound of 0
        // Stan v2.16.0 manual p194:
        for (n in 1:N) {
            target += log_mix(lambda,
                              normal_lpdf(y[n] | mu1, sigma[1]),
                              normal_lpdf(y[n] | mu1 + mu2_minus_mu1, sigma[2]));
        }
    }
    generated quantities {
        real mu2 = mu2_minus_mu1 + mu1;
        real sd2_minus_sd1 = sigma[2] - sigma[1];
    }
"
BIMODAL_1SD_MODEL_CODE <- "
    data {
        int<lower=0> N;
        real y[N];
    }
    parameters {
        real mu1;
        real<lower=0> mu2_minus_mu1;
        real<lower=0> sigma;
        real<lower=0, upper=1> lambda;  // mixture proportion
    }
    model {
        target += normal_lpdf(mu1 | 0, 100);
        target += normal_lpdf(mu2_minus_mu1 | 0, 100) -
                  normal_lccdf(0 | 0, 100);  // lower bound of 0;
        target += cauchy_lpdf(sigma | 0, 10) -
                  cauchy_lccdf(0 | 0, 10);  // lower bound of 0
        for (n in 1:N) {
            target += log_mix(lambda,
                              normal_lpdf(y[n] | mu1, sigma),
                              normal_lpdf(y[n] | mu1 + mu2_minus_mu1, sigma));
        }
    }
    generated quantities {
        real mu2 = mu2_minus_mu1 + mu1;
    }
"

# =============================================================================
# Test "automatic" reparameterizing
# =============================================================================

NONCENTRED_TEST1_CODE_FOR_UNIMODAL <- paste("
    functions {
", CODE_COMMON_FUNCTIONS, "
    }
    data {
        int<lower=0> N;
        real y[N];
    }
    parameters {
        real mu;
        real<lower=0> sigma;
    }
    model {
        // Notation/functions to make our bridgesampling life easier:
        sampleNormal_RRR_lp(mu, 0, 100);
        sampleCauchyLowerBound_RRR_lp(sigma, 0, 10, 0);  // lower bound of 0
        sampleNormal_ARR_lp(y, mu, sigma);
    }
", sep="")

# https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
# http://mc-stan.org/users/documentation/case-studies/divergences_and_bias.html
# ... rewritten with my convenience/bridgesampling functions
# ... yup, same answers as the canonical NONCENTRED_TEST3_CODE_FOR_SCHOOLS below
SCHOOLS_DAT <- list(J = 8,
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))
NONCENTRED_TEST2_CODE_FOR_SCHOOLS <- paste("
    functions {
", CODE_COMMON_FUNCTIONS, "
    }
    data {
        int<lower=0> J;  // number of schools
        real y[J];  // estimated treatment effects
        real<lower=0> sigma[J];  // s.e. of effect estimates
    }
    transformed data {
        real HALF_CAUCHY_MU = 0;
        real HALF_CAUCHY_SIGMA = 5;
        real HALF_PI = pi() / 2;
    }
    parameters {
        real mu;
        real theta_tilde_unit_normal[J];  // temporary for reparameterization; 'theta_tilde' in original
        real<lower=0, upper=HALF_PI> tau_uniform;  // temporary for reparameterization; new
    }
    transformed parameters {
        real<lower=0> tau;
        real theta[J];  // normal distribution of interest
        tau = getRRRReparameterizedCauchyLowerBound_lp(tau_uniform, HALF_CAUCHY_MU, HALF_CAUCHY_SIGMA, 0);
            // ... additional reparameterization of the Cauchy
        theta = getARRReparameterizedNormal_lp(theta_tilde_unit_normal, mu, tau);
            // ... reparameterized as per the example, but with our automation method
    }
    model {
        sampleNormal_RRR_lp(mu, 0, 5);  // same method as original
        sampleNormal_AAA_lp(y, theta, sigma);  // same method as original
    }
", sep="")

# This one exactly as per http://mc-stan.org/users/documentation/case-studies/divergences_and_bias.html:
# ... and it gives the same answer (within random variation) as there.
NONCENTRED_TEST3_CODE_FOR_SCHOOLS <- paste("
    data {
        int<lower=0> J;
        real y[J];
        real<lower=0> sigma[J];
    }

    parameters {
        real mu;
        real<lower=0> tau;
        real theta_tilde[J];
    }

    transformed parameters {
        real theta[J];
        for (j in 1:J) {
            theta[j] = mu + tau * theta_tilde[j];
        }
    }

    model {
        mu ~ normal(0, 5);
        tau ~ cauchy(0, 5);
        theta_tilde ~ normal(0, 1);
        y ~ normal(theta, sigma);
    }
", sep="")
# First step to check: replacing the sampling code without reparameterization
# ... works fine. Same output (except lp__, which we have adjusted for
# bridge sampling).
NONCENTRED_TEST4_CODE_FOR_SCHOOLS <- paste("
    functions {
", CODE_COMMON_FUNCTIONS, "
    }
    data {
        int<lower=0> J;
        real y[J];
        real<lower=0> sigma[J];
    }

    parameters {
        real mu;
        real<lower=0> tau;
        real theta_tilde[J];
    }

    transformed parameters {
        real theta[J];
        for (j in 1:J) {
            theta[j] = mu + tau * theta_tilde[j];
        }
    }

    model {
        sampleNormal_RRR_lp(mu, 0, 5);
        sampleCauchy_RRR_lp(tau, 0, 5);
        sampleNormal_ARR_lp(theta_tilde, 0, 1);
        sampleNormal_AAA_lp(y, theta, sigma);
    }
", sep="")
# Now back to NONCENTRED_TEST2_CODE_FOR_SCHOOLS

# Can we put things in the model as well as the transformed parameters?
# No. This fails to compile as the "tau = ..." line with:
# attempt to assign variable in wrong block. left-hand-side variable origin=transformed parameter
NONCENTRED_TEST_DUFF_CODE_FOR_SCHOOLS <- paste("
    functions {
", CODE_COMMON_FUNCTIONS, "
    }
    data {
        int<lower=0> J;  // number of schools
        real y[J];  // estimated treatment effects
        real<lower=0> sigma[J];  // s.e. of effect estimates
    }
    transformed data {
        real HALF_CAUCHY_MU = 0;
        real HALF_CAUCHY_SIGMA = 5;
        real HALF_PI = pi() / 2;
    }
    parameters {
        real mu;
        real theta_tilde_unit_normal[J];  // temporary for reparameterization; 'theta_tilde' in original
        real<lower=0, upper=HALF_PI> tau_uniform;  // temporary for reparameterization; new
    }
    transformed parameters {
        real<lower=0> tau;
        real theta[J];  // normal distribution of interest
        // +++ remove sampling code
    }
    model {
        // +++ and add it here
        tau = getRRRReparameterizedCauchyLowerBound_lp(tau_uniform, HALF_CAUCHY_MU, HALF_CAUCHY_SIGMA, 0);
            // ... additional reparameterization of the Cauchy
        theta = getARRReparameterizedNormal_lp(theta_tilde_unit_normal, mu, tau);
            // ... reparameterized as per the example, but with our automation method

        sampleNormal_RRR_lp(mu, 0, 5);  // same method as original
        sampleNormal_AAA_lp(y, theta, sigma);  // same method as original
    }
", sep="")

# =============================================================================
# Run!
# =============================================================================

run_models <- function(cmdargs)
{
    # WRITES TO GLOBAL NAMESPACE

    CHAINS <- 8
    ITER <- 2000
    INIT <- "0"
    SEED <- 1234
    ADAPT_DELTA <- 0.99

    runner <- function(model_name, code, data) {
        cat(paste("Running model ", model_name, "...\n", sep=""))
        fit_filename <- paste(
            FIT_CACHE_DIR, "/", model_name, "_fit.rds", sep="")
        bridge_filename <- paste(
            FIT_CACHE_DIR, "/", model_name, "_bridge.rds", sep="")
        code_filename <- paste(
            FIT_CACHE_DIR, "/", model_name, "_temp_code.cpp", sep="")

        fit <- stanfunc$load_or_run_stan(
            data=data,
            model_code=code,
            fit_filename=fit_filename,
            model_name=model_name,
            save_code_filename=code_filename,
            chains=CHAINS,
            iter=ITER,
            init=INIT,
            seed=SEED,
            control = list(
                adapt_delta=ADAPT_DELTA
            )
        )

        cat("Making ShinyStan object...\n")
        ss <- shinystan::as.shinystan(fit)
        cat("... made\n")

        b <- stanfunc$load_or_run_bridge_sampler(
            stanfit=fit,
            filename=bridge_filename,
            model_code=code,
            data=data
        )

        return(list(fit, ss, b))
    }

    if ("dummy_bridge_good" %in% cmdargs || "all" %in% cmdargs) {
        list[dummy_bridge_good_fit, ss_ignore, dummy_bridge_good_bridge] <- runner(
            "dummy_bridge_good", DUMMY_BRIDGE_CODE_GOOD, DUMMY_BRIDGE_DATA)
        dummy_bridge_good_fit <<- dummy_bridge_good_fit
        dummy_bridge_good_bridge <<- dummy_bridge_good_bridge
    }

    if ("dummy_bridge_bad" %in% cmdargs || "all" %in% cmdargs) {
        list[dummy_bridge_bad_fit, ss_ignore, dummy_bridge_bad_bridge] <- runner(
            "dummy_bridge_bad", DUMMY_BRIDGE_CODE_BAD, DUMMY_BRIDGE_DATA)
        dummy_bridge_bad_fit <<- dummy_bridge_bad_fit
        dummy_bridge_bad_bridge <<- dummy_bridge_bad_bridge
    }

    if ("unimodal_data_unimodal_model" %in% cmdargs || "all" %in% cmdargs) {
        list[ud_um_fit, ud_um_shiny, ud_um_bridge] <- runner(
            "unimodal_data_unimodal_model",
            UNIMODAL_MODEL_CODE, DATA_UNIMODAL)
        ud_um_fit <<- ud_um_fit  # ONE OF THE OPTIMAL MODELS
        ud_um_shiny <<- ud_um_shiny
        ud_um_bridge <<- ud_um_bridge
    }

    if ("unimodal_data_bimodal_2sd_model" %in% cmdargs || "all" %in% cmdargs) {
        list[ud_b2m_fit, ud_b2m_shiny, ud_b2m_bridge] <- runner(
            "unimodal_data_bimodal_2sd_model",
            BIMODAL_2SD_MODEL_CODE, DATA_UNIMODAL)
        ud_b2m_fit <<- ud_b2m_fit
        ud_b2m_shiny <<- ud_b2m_shiny
        ud_b2m_bridge <<- ud_b2m_bridge
    }

    if ("unimodal_data_bimodal_1sd_model" %in% cmdargs || "all" %in% cmdargs) {
        list[ud_b1m_fit, ud_b1m_shiny, ud_b1m_bridge] <- runner(
            "unimodal_data_bimodal_1sd_model",
            BIMODAL_1SD_MODEL_CODE, DATA_UNIMODAL)
        ud_b1m_fit <<- ud_b1m_fit
        ud_b1m_shiny <<- ud_b1m_shiny
        ud_b1m_bridge <<- ud_b1m_bridge
    }

    if ("bimodal_1sd_data_unimodal_model" %in% cmdargs || "all" %in% cmdargs) {
        list[b1d_um_fit, b1d_um_shiny, b1d_um_bridge] <- runner(
            "bimodal_1sd_data_unimodal_model",
            UNIMODAL_MODEL_CODE, DATA_BIMODAL_ONESD)
        b1d_um_fit <<- b1d_um_fit
        b1d_um_shiny <<- b1d_um_shiny
        b1d_um_bridge <<- b1d_um_bridge
    }

    if ("bimodal_1sd_data_bimodal_2sd_model" %in% cmdargs || "all" %in% cmdargs) {
        list[b1d_b2m_fit, b1d_b2m_shiny, b1d_b2m_bridge] <- runner(
            "bimodal_1sd_data_bimodal_2sd_model",
            BIMODAL_2SD_MODEL_CODE, DATA_BIMODAL_ONESD)
        b1d_b2m_fit <<- b1d_b2m_fit
        b1d_b2m_shiny <<- b1d_b2m_shiny
        b1d_b2m_bridge <<- b1d_b2m_bridge
    }

    if ("bimodal_1sd_bimodal_1sd_model" %in% cmdargs || "all" %in% cmdargs) {
        list[b1d_b1m_fit, b1d_b1m_shiny, b1d_b1m_bridge] <- runner(
            "bimodal_1sd_bimodal_1sd_model",
            BIMODAL_1SD_MODEL_CODE, DATA_BIMODAL_ONESD)
        b1d_b1m_fit <<- b1d_b1m_fit  # ONE OF THE OPTIMAL MODELS
        b1d_b1m_shiny <<- b1d_b1m_shiny
        b1d_b1m_bridge <<- b1d_b1m_bridge
    }

    if ("bimodal_2sd_data_unimodal_model" %in% cmdargs || "all" %in% cmdargs) {
        list[b2d_um_fit, b2d_um_shiny, b2d_um_bridge] <- runner(
            "bimodal_2sd_data_unimodal_model",
            UNIMODAL_MODEL_CODE, DATA_BIMODAL_TWOSD)
        b2d_um_fit <<- b2d_um_fit
        b2d_um_shiny <<- b2d_um_shiny
        b2d_um_bridge <<- b2d_um_bridge
    }

    if ("bimodal_2sd_data_bimodal_2sd_model" %in% cmdargs || "all" %in% cmdargs) {
        list[b2d_b2m_fit, b2d_b2m_shiny, b2d_b2m_bridge] <- runner(
            "bimodal_2sd_data_bimodal_2sd_model",
            BIMODAL_2SD_MODEL_CODE, DATA_BIMODAL_TWOSD)
        b2d_b2m_fit <<- b2d_b2m_fit  # ONE OF THE OPTIMAL MODELS
        b2d_b2m_shiny <<- b2d_b2m_shiny
        b2d_b2m_bridge <<- b2d_b2m_bridge
    }

    if ("bimodal_2sd_data_bimodal_1sd_model" %in% cmdargs || "all" %in% cmdargs) {
        list[b2d_b1m_fit, b2d_b1m_shiny, b2d_b1m_bridge] <- runner(
            "bimodal_2sd_data_bimodal_1sd_model",
            BIMODAL_1SD_MODEL_CODE, DATA_BIMODAL_TWOSD)
        b2d_b1m_fit <<- b2d_b1m_fit
        b2d_b1m_shiny <<- b2d_b1m_shiny
        b2d_b1m_bridge <<- b2d_b1m_bridge
    }

    if ("demo_bad_parameterization" %in% cmdargs || "all" %in% cmdargs) {
        # This is the one that has two means but hops between them.
        list[b1d_wrongb1m_fit, b1d_wrongb1m_shiny, b1d_wrongb1m_bridge] <- runner(
            "bimodal_1sd_bimodal_1sd_WRONG_model",
            BIMODAL_1SD_MODEL_CODE_WRONG, DATA_BIMODAL_ONESD)
        b1d_wrongb1m_fit <<- b1d_wrongb1m_fit
        b1d_wrongb1m_shiny <<- b1d_wrongb1m_shiny
        b1d_wrongb1m_bridge <<- b1d_wrongb1m_bridge
    }

    if ("noncentred_test1" %in% cmdargs || "all" %in% cmdargs) {
        list[noncentred_test1_fit, noncentred_test1_shiny, noncentred_test1_bridge] <- runner(
            "noncentred_test1",
            NONCENTRED_TEST1_CODE_FOR_UNIMODAL, DATA_UNIMODAL)
        noncentred_test1_fit <<- noncentred_test1_fit
        noncentred_test1_shiny <<- noncentred_test1_shiny
        noncentred_test1_bridge <<- noncentred_test1_bridge
    }

    if ("noncentred_test2" %in% cmdargs || "all" %in% cmdargs) {
        list[noncentred_test2_fit, noncentred_test2_shiny, noncentred_test2_bridge] <- runner(
            "noncentred_test2",
            NONCENTRED_TEST2_CODE_FOR_SCHOOLS, SCHOOLS_DAT)
        noncentred_test2_fit <<- noncentred_test2_fit
        noncentred_test2_shiny <<- noncentred_test2_shiny
        noncentred_test2_bridge <<- noncentred_test2_bridge
    }

    if ("noncentred_test3" %in% cmdargs || "all" %in% cmdargs) {
        list[noncentred_test3_fit, noncentred_test3_shiny, noncentred_test3_bridge] <- runner(
            "noncentred_test3",
            NONCENTRED_TEST3_CODE_FOR_SCHOOLS, SCHOOLS_DAT)
        noncentred_test3_fit <<- noncentred_test3_fit
        noncentred_test3_shiny <<- noncentred_test3_shiny
        # The bridge answers will be duff; ignore them (not using "target +=" notation).
    }

    if ("noncentred_test4" %in% cmdargs || "all" %in% cmdargs) {
        list[noncentred_test4_fit, noncentred_test4_shiny, noncentred_test4_bridge] <- runner(
            "noncentred_test4",
            NONCENTRED_TEST4_CODE_FOR_SCHOOLS, SCHOOLS_DAT)
        noncentred_test4_fit <<- noncentred_test4_fit
        noncentred_test4_shiny <<- noncentred_test4_shiny
        noncentred_test4_bridge <<- noncentred_test4_bridge
    }
}


human_comparison <- function()
{
    # WRITES TO GLOBAL NAMESPACE
 #   run_models("all")

    comparison_unimodal <<- stanfunc$compare_model_evidence(
        list(
            list(
                # ONE OF THE OPTIMAL MODELS
                name="Unimodal data, unimodal model",
                bridgesample=ud_um_bridge,
                stanfit=ud_um_fit
            ),
            list(
                name="Unimodal data, bimodal model with two SDs",
                bridgesample=ud_b2m_bridge,
                stanfit=ud_b2m_fit
            ),
            list(
                name="Unimodal data, bimodal model with single SD",
                bridgesample=ud_b1m_bridge,
                stanfit=ud_b1m_fit
            )
        ),
        detail=TRUE
    )
    comparison_bimodal_one_sd <<- stanfunc$compare_model_evidence(
        list(
            list(
                name="Bimodal data/one SD, unimodal model",
                bridgesample=b1d_um_bridge,
                stanfit=b1d_um_fit
            ),
            list(
                name="Bimodal data/one SD, bimodal model with two SDs",
                bridgesample=b1d_b2m_bridge,
                stanfit=b1d_b2m_fit
            ),
            list(
                # ONE OF THE OPTIMAL MODELS
                name="Bimodal data/one SD, bimodal model with single SD",
                bridgesample=b1d_b1m_bridge,
                stanfit=b1d_b1m_fit
            )
        ),
        detail=TRUE
    )
    comparison_bimodal_two_sd <<- stanfunc$compare_model_evidence(
        list(
            list(
                name="Bimodal data/two SDs, unimodal model",
                bridgesample=b2d_um_bridge,
                stanfit=b2d_um_fit
            ),
            list(
                name="Bimodal data/two SDs, bimodal model with two SDs",
                bridgesample=b2d_b2m_bridge,  # ONE OF THE OPTIMAL MODELS
                stanfit=b2d_b2m_fit
            ),
            list(
                name="Bimodal data/two SDs, bimodal model with single SD",
                bridgesample=b2d_b1m_bridge,
                stanfit=b2d_b1m_fit
            )
        ),
        detail=TRUE
    )
}


plot_unimodal <- function()
{
    hist(DATA_UNIMODAL$y, prob=TRUE)
    lines(density(DATA_UNIMODAL$y))
}


plot_bimodal_onesd <- function()
{
    hist(DATA_BIMODAL_ONESD$y, prob=TRUE)
    lines(density(DATA_BIMODAL_ONESD$y))
}


plot_bimodal_twosd <- function()
{
    hist(DATA_BIMODAL_TWOSD$y, prob=TRUE)
    lines(density(DATA_BIMODAL_TWOSD$y))
}


# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
CB_PALETTE <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


slide <- function()
{
    lines <- data.table(x = seq(15, 75, 0.1))
    lines[, unimodal := dnorm(x, UNIMODAL_MEAN, UNIMODAL_SD)]
    lines[, bimodal_one_sd :=
        MIXTURE_FRACTION * dnorm(x, BIMODAL_MEAN_1, BIMODAL_SD_1) +
        (1 - MIXTURE_FRACTION) * dnorm(x, BIMODAL_MEAN_2, BIMODAL_SD_1)
    ]
    lines[, bimodal_two_sds :=
        MIXTURE_FRACTION * dnorm(x, BIMODAL_MEAN_1, BIMODAL_SD_1) +
        (1 - MIXTURE_FRACTION) * dnorm(x, BIMODAL_MEAN_2, BIMODAL_SD_2)
    ]
    lines_long <- melt(lines,
                       id.vars=c("x"),
                       variable.name="distribution",
                       value.name="density")
    fgen <- (
        ggplot(lines_long, aes(x=x, y=density, colour=distribution)) +
        geom_line(size=1, show.legend=FALSE) +
        scale_colour_manual(values=CB_PALETTE) +
        theme_bw() +
        ggtitle("Generating distributions")
    )

    data <- data.table(
        sample_num = 1:N,
        unimodal = DATA_UNIMODAL$y,
        bimodal_one_sd = DATA_BIMODAL_ONESD$y,
        bimodal_two_sds = DATA_BIMODAL_TWOSD$y
    )
    data_long <- melt(data,
                      id.vars = c("sample_num"),
                      variable.name = "distribution",
                      value.name = "x")
    fdata <- (
        ggplot(data_long, aes(x = x, group = distribution)) +
        # geom_density(aes(colour = distribution), size=1) +
        geom_histogram(aes(# y = ..density..,
                           fill = distribution),
                       position = "dodge",
                       binwidth = 2,
                       show.legend = FALSE) +
        facet_grid(. ~ distribution) +
        scale_colour_manual(values=CB_PALETTE) +
        scale_fill_manual(values=CB_PALETTE) +
        theme_bw() +
        ggtitle(paste("Specimen data (n = ", N, ")", sep=""))
    )
    # f1 <- ggplot(d1, aes(d1$y)) + geom_histogram()

    fig <- gridExtra::arrangeGrob(fgen, fdata, nrow=1, widths=c(0.3, 0.7))
    ggsave(paste(OUTPUT_DIR, "model_selection.pdf", sep="/"),
           fig, width=250, height=80, units="mm")

    generic_normal_dist_data <- data.table(x = seq(-4, 4, 0.01))
    generic_normal_dist_data[, y := dnorm(x, 0, 1)]
    generic_normal_fig <- (
        ggplot(generic_normal_dist_data, aes(x, y)) +
        geom_line() +
        theme(
            panel.border = element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
        )
    )
    ggsave(paste(OUTPUT_DIR, "normal_distribution.pdf", sep="/"),
           generic_normal_fig, width=10, height=10, units="mm")

}


run_models("all")
