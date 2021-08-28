#!/usr/bin/env Rscript
#
# RNC Dec 2017
#
# All this does is check that Stan works.

library(rstan)
library(shinystan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# http://www.openbugs.net/Examples/Blockers.html
# https://github.com/stan-dev/example-models/blob/master/bugs_examples/vol1/blocker/

CODE <- "
    data {
        int<lower=0> N;
        int<lower=0> nt[N];
        int<lower=0> rt[N];
        int<lower=0> nc[N];
        int<lower=0> rc[N];
    }
    parameters {
        real d;
        real<lower=0> sigmasq_delta;
        vector[N] mu;
        vector[N] delta;
        real delta_new;
    }
    transformed parameters {
        real<lower=0> sigma_delta;
        sigma_delta = sqrt(sigmasq_delta);
    }
    model {
        rt ~ binomial_logit(nt, mu + delta);
        rc ~ binomial_logit(nc, mu);
        delta  ~ student_t(4, d, sigma_delta);
        mu ~ normal(0, sqrt(1E5));
        d ~ normal(0, 1E3);
        sigmasq_delta ~ inv_gamma(1E-3, 1E-3);
        // FIXME: sample in generated quantities in later version
        delta_new ~ student_t(4, d, sigma_delta);
    }
"

#INIT <- list(
#    d = 0,
#    delta = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
#    sigmasq_delta = 1,
#    mu = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
#    delta = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
#    delta_new = 0
#)

DATA <- list(
    rt = c(3, 7, 5, 102, 28, 4, 98, 60, 25, 138, 64, 45, 9, 57, 25, 33,
           28, 8, 6, 32, 27, 22),
    nt = c(38, 114, 69, 1533, 355, 59, 945, 632, 278, 1916, 873, 263,
           291, 858, 154, 207, 251, 151, 174, 209, 391, 680),
    rc = c(3, 14, 11, 127, 27, 6, 152, 48, 37, 188, 52, 47, 16, 45, 31,
           38, 12, 6, 3, 40, 43, 39),
    nc = c(39, 116, 93, 1520, 365, 52, 939, 471, 282, 1921, 583, 266,
           293, 883, 147, 213, 122, 154, 134, 218, 364, 674),
    N = 22
)

THIS_SCRIPT_DIR <- dirname(sys.frame(1)$ofile)
setwd(THIS_SCRIPT_DIR)
FIT_CACHE_DIR <- paste(THIS_SCRIPT_DIR, "fitcache", sep="/")
FIT_FILENAME <- paste(FIT_CACHE_DIR, "dummy_fit.rds", sep="/")

if (file.exists(FIT_FILENAME)) {

    fit <- readRDS(file=FIT_FILENAME)

} else {

    cat(paste("--- Running Stan, starting at", Sys.time(), "...\n"))
    fit <- stan(
        model_name = "dummy_model_openbugs_blockers",  # model_name NOT WORKING; check fit@model_name
        model_code = CODE,
        data = DATA,
        chains = 8,
        iter = 2000,
        init = "0",
        seed = 1234
    )
    cat(paste("... Finished Stan run at", Sys.time(), "\n"))
    saveRDS(fit, file=FIT_FILENAME)

}

ss <- as.shinystan(fit, model_name = "dummy_model_openbugs_blockers")  # WORKS
launch_shinystan(ss)

