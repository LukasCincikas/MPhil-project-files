// cgt_romeu2020_model12_single_subject.stan

functions {
    // #include syntax is finicky:
    // ... https://github.com/stan-dev/rstantools/issues/62
    // ... no indent, no quotes, relative file path, leading slash

#include /commonfunc.stan
    // ... https://egret.psychol.cam.ac.uk/rlib/commonfunc.stan

#include /include_functions.stan
}

data {
#include /include_data.stan
}

transformed data {
#include /include_transformed_data.stan
}

parameters {
    // ========================================================================
    // The parameters that Stan will estimate.
    // ========================================================================

    vector[N_SUBJECTS] standard_normal_alpha;
    vector[N_SUBJECTS] standard_normal_red_bias;
    vector[N_SUBJECTS] standard_normal_gamma;
    vector[N_SUBJECTS] standard_normal_rho;
    vector[N_SUBJECTS] standard_normal_beta;
}

transformed parameters {
    // ========================================================================
    // The actual parameters we care about for the model.
    // ========================================================================

    // Following [Remeu2020], supplementary p14, parameter (means) follow
    // normal(0, 1) distributions in the "unconstrained" space, but are then
    // transformed.

    // Bounded parameters: inverse probit-transformed to (0, 1) then scaled:
    // - the probit function is the quantile function for the standard normal
    //   distribution [https://en.wikipedia.org/wiki/Probit]
    // - which is Phi() in Stan
    //   [https://mc-stan.org/docs/2_21/stan-users-guide/logistic-probit-regression-section.html]
    vector<lower=0, upper=ALPHA_UPPER_LIMIT>[N_SUBJECTS] subject_alpha =
        Phi(standard_normal_alpha) * ALPHA_UPPER_LIMIT;
    vector<lower=0, upper=1>[N_SUBJECTS] subject_red_bias =
        Phi(standard_normal_red_bias);

    // Unbounded parameters: exponentially transformed to (0, +inf):
    vector<lower=0>[N_SUBJECTS] subject_gamma = exp(standard_normal_gamma);
    vector<lower=0>[N_SUBJECTS] subject_rho = exp(standard_normal_rho);
    vector<lower=0>[N_SUBJECTS] subject_beta = exp(standard_normal_beta);
}

model {
    // ========================================================================
    // Declare variables
    // ========================================================================

#include /include_model_declarations.stan

    // ========================================================================
    // Priors
    // ========================================================================

    // Following [Remeu2020], supplementary p14, parameter (means) follow
    // normal(0, 1) distributions in the "unconstrained" space:
    // ... sampleNormal_VRR_lp(vector y, real mu, real sigma)
    // ... equivalent to: target += normal_lpdf(y | mu, sigma);
    sampleNormal_VRR_lp(standard_normal_alpha,    0, 1);
    sampleNormal_VRR_lp(standard_normal_red_bias, 0, 1);
    sampleNormal_VRR_lp(standard_normal_gamma,    0, 1);
    sampleNormal_VRR_lp(standard_normal_rho,      0, 1);
    sampleNormal_VRR_lp(standard_normal_beta,     0, 1);

    // ========================================================================
    // Cognitive model
    // ========================================================================

#include /include_model_core_romeu2020_m12.stan

}
