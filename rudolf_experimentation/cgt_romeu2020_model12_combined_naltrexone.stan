// cgt_romeu2020_model12_groups_naltrexone.stan
// Used for the winning model using the 2 additional parameters,
// For 2 conditions x 3 groups data structure

functions {
    // #include syntax is finicky:
    // ... https://github.com/stan-dev/rstantools/issues/62
    // ... no indent, no quotes, relative file path, leading slash

#include /commonfunc.stan
    // ... https://egret.psychol.cam.ac.uk/rlib/commonfunc.stan

#include /include_functions.stan
}

data {
#include /include_data_loss.stan
}

transformed data {
#include /include_transformed_data.stan
}

parameters {
    // ========================================================================
    // The parameters that Stan will estimate.
    // ========================================================================
    // - We'll use "nspace1_" to indicate the "unit normal" space in which we
    //   will ask Stan to sample parameter values (unconstrained).
    // - We'll use "nspace2_" to indicate another "unit normal" space in which
    //   we will ask Stan to sample intersubject deviations. These will then
    //   be scaled (by our sampled SD) and merged into "nspace1_".

    // ------------------------------------------------------------------------
    // Fixed effects
    // ------------------------------------------------------------------------

    // Group means, in N(0,1) space:
    vector[N_GROUPS*N_CONDITIONS] nspace1_group_mean_alpha;
    vector[N_GROUPS*N_CONDITIONS] nspace1_group_mean_red_bias;
    vector[N_GROUPS*N_CONDITIONS] nspace1_group_mean_gamma;
    vector[N_GROUPS*N_CONDITIONS] nspace1_group_mean_rho;
    vector[N_GROUPS*N_CONDITIONS] nspace1_group_mean_beta;
    vector[N_GROUPS*N_CONDITIONS] nspace1_group_mean_theta;
    vector[N_GROUPS*N_CONDITIONS] nspace1_group_mean_zeta;

    // ------------------------------------------------------------------------
    // Random effects
    // ------------------------------------------------------------------------

    // Group (intersubject) SDs, in half-normal(0, 0.2) space, suitable for
    // combining with the "nspace1_" group means above:
    vector<lower=0>[N_GROUPS] nspace1_group_sd_alpha;
    vector<lower=0>[N_GROUPS] nspace1_group_sd_red_bias;
    vector<lower=0>[N_GROUPS] nspace1_group_sd_gamma;
    vector<lower=0>[N_GROUPS] nspace1_group_sd_rho;
    vector<lower=0>[N_GROUPS] nspace1_group_sd_beta;
    vector<lower=0>[N_GROUPS] nspace1_group_sd_theta;
    vector<lower=0>[N_GROUPS] nspace1_group_sd_zeta;
    
    // Subject effects (deviations from their group mean), in N(0,1) space:
    vector[N_SEGMENTS] nspace2_subject_effect_alpha;
    vector[N_SEGMENTS] nspace2_subject_effect_red_bias;
    vector[N_SEGMENTS] nspace2_subject_effect_gamma;
    vector[N_SEGMENTS] nspace2_subject_effect_rho;
    vector[N_SEGMENTS] nspace2_subject_effect_beta;
    vector[N_SEGMENTS] nspace2_subject_effect_theta;
    vector[N_SEGMENTS] nspace2_subject_effect_zeta;
    
}

transformed parameters {
    // ========================================================================
    // The actual parameters we care about for the model.
    // ========================================================================

    // These are in "proper" task parameter space:
    vector<lower=0, upper=ALPHA_UPPER_LIMIT_ALT>[N_SUBJECTS] subject_alpha;
    vector<lower=0, upper=1>[N_SUBJECTS] subject_red_bias;
    vector<lower=0>[N_SUBJECTS] subject_gamma;
    vector<lower=0>[N_SUBJECTS] subject_rho;
    vector<lower=0>[N_SUBJECTS] subject_beta;
    vector[N_SUBJECTS] subject_theta;
    vector[N_SUBJECTS] subject_zeta;

    // Following [Remeu2020], supplementary p14, parameter (means) follow
    // normal(0, 1) distributions in the "unconstrained" space, and their SDs
    // follow half-normal(0, 0.2) distributions, but they are then transformed.

    // Bounded parameters: inverse probit-transformed to (0, 1) then scaled:
    // - the probit function is the quantile function for the standard normal
    //   distribution [https://en.wikipedia.org/wiki/Probit]
    // - which is Phi() in Stan
    //   [https://mc-stan.org/docs/2_21/stan-users-guide/logistic-probit-regression-section.html]

    for (s in 1:N_SUBJECTS) {
        int g = group_num_by_subject[s];
        int c = condition_num_by_subject[s];
        int sgm = segment_num_by_subject[s];
        int p; //Marker for the number of the groupXcondition vector
        real nspace1_subject_effect_alpha;  // deviation from group mean
        real nspace1_subject_alpha;  // final per-subject value, in unit normal space
        real nspace1_subject_effect_red_bias;
        real nspace1_subject_red_bias;
        real nspace1_subject_effect_gamma;
        real nspace1_subject_gamma;
        real nspace1_subject_effect_rho;
        real nspace1_subject_rho;
        real nspace1_subject_effect_beta;
        real nspace1_subject_beta;
        real nspace1_subject_effect_theta;
        real nspace1_subject_theta;
        real nspace1_subject_effect_zeta;
        real nspace1_subject_zeta;

        // Steps for each of these:
        // (1) Transformed the sampled "subject effect", in N(0, 1) space
        //     ("nspace2_"), to N(0, intersubject_sd) space, suitable for
        //     adding to the group means (in "nspace1_"). This is simple;
        //     we just multiply by the SD.  
        // (2) Within "Stan parameter space", calculate
        //     subject value = group mean + subject-specific effect [from 1]
        // (3) Convert from "Stan parameter space" to "task parameter space".
        // We use temporary variables so this is clearer.
        
        //Because the groupXcondition ends up being a vector, I need to determine the group number here
        if (g == 1) {
            if(c == 1) {
                p = 1; 
            } else {
                p = 2;
            }
        } else if (g == 2){
            if (c == 1) {
                p = 3;
            } else {
                p = 4;
            }
        } else {
            if (c == 1) {
                p = 5;
            } else {
                p = 6;
            }
        }

        // Bounded parameters: inverse probit-transformed to (0, 1) then scaled:
        nspace1_subject_effect_alpha =
            nspace2_subject_effect_alpha[sgm] * nspace1_group_sd_alpha[g];
        nspace1_subject_alpha =
            nspace1_group_mean_alpha[p] + nspace1_subject_effect_alpha;
        subject_alpha[s] = Phi(nspace1_subject_alpha) * ALPHA_UPPER_LIMIT_ALT;

        nspace1_subject_effect_red_bias =
            nspace2_subject_effect_red_bias[sgm] * nspace1_group_sd_red_bias[g];
        nspace1_subject_red_bias =
            nspace1_group_mean_red_bias[p] + nspace1_subject_effect_red_bias;
        subject_red_bias[s] = Phi(nspace1_subject_red_bias);

        nspace1_subject_effect_gamma =
            nspace2_subject_effect_gamma[sgm] * nspace1_group_sd_gamma[g];
        nspace1_subject_gamma =
            nspace1_group_mean_gamma[p] + nspace1_subject_effect_gamma;
        subject_gamma[s] = exp(nspace1_subject_gamma);

        // Unbounded parameters: exponentially transformed to (0, +inf):
        nspace1_subject_effect_rho =
            nspace2_subject_effect_rho[sgm] * nspace1_group_sd_rho[g];
        nspace1_subject_rho =
            nspace1_group_mean_rho[p] + nspace1_subject_effect_rho;
        subject_rho[s] = exp(nspace1_subject_rho);

        nspace1_subject_effect_beta =
            nspace2_subject_effect_beta[sgm] * nspace1_group_sd_beta[g];
        nspace1_subject_beta =
            nspace1_group_mean_beta[p] + nspace1_subject_effect_beta;
        subject_beta[s] = exp(nspace1_subject_beta);
        
        nspace1_subject_effect_theta =
            nspace2_subject_effect_theta[sgm] * nspace1_group_sd_theta[g];
        nspace1_subject_theta =
            nspace1_group_mean_theta[p] + nspace1_subject_effect_theta;
        subject_theta[s] = nspace1_subject_theta;
        
        nspace1_subject_effect_zeta =
            nspace2_subject_effect_zeta[sgm] * nspace1_group_sd_zeta[g];
        nspace1_subject_zeta =
            nspace1_group_mean_zeta[p] + nspace1_subject_effect_zeta;
        subject_zeta[s] = nspace1_subject_zeta;

        // We could have used the "target +=" notation (as for bridge sampling)
        // to sample here, even though Stan doesn't usually allow sampling in
        // "transformed parameters".
        // See e.g.
        // https://discourse.mc-stan.org/t/transformed-parameters-with-target/7256
        // However, for clarity, let's not. Everything will be sampled in 
        // "model". 
    }
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
    sampleNormal_VRR_lp(nspace1_group_mean_alpha,    0, 1);
    sampleNormal_VRR_lp(nspace1_group_mean_red_bias, 0, 1);
    sampleNormal_VRR_lp(nspace1_group_mean_gamma,    0, 1);
    sampleNormal_VRR_lp(nspace1_group_mean_rho,      0, 1);
    sampleNormal_VRR_lp(nspace1_group_mean_beta,     0, 1);
    sampleNormal_VRR_lp(nspace1_group_mean_theta,    0, 1);
    sampleNormal_VRR_lp(nspace1_group_mean_zeta,    0, 1);

    // Group (intersubject) SDs follow half-normal(0, 0.2) distributions in
    // this space:
    // ... sampleNormalLowerBound_VRR_lp(vector y, real mu, real sigma, real lower)
    sampleNormalLowerBound_VRR_lp(nspace1_group_sd_alpha,    0, PRIOR_SD_FOR_GROUP_STANDARD_NORMAL, 0);
    sampleNormalLowerBound_VRR_lp(nspace1_group_sd_red_bias, 0, PRIOR_SD_FOR_GROUP_STANDARD_NORMAL, 0);
    sampleNormalLowerBound_VRR_lp(nspace1_group_sd_gamma,    0, PRIOR_SD_FOR_GROUP_STANDARD_NORMAL, 0);
    sampleNormalLowerBound_VRR_lp(nspace1_group_sd_rho,      0, PRIOR_SD_FOR_GROUP_STANDARD_NORMAL, 0);
    sampleNormalLowerBound_VRR_lp(nspace1_group_sd_beta,     0, PRIOR_SD_FOR_GROUP_STANDARD_NORMAL, 0);
    sampleNormalLowerBound_VRR_lp(nspace1_group_sd_theta,    0, PRIOR_SD_FOR_GROUP_STANDARD_NORMAL, 0);
    sampleNormalLowerBound_VRR_lp(nspace1_group_sd_zeta,    0, PRIOR_SD_FOR_GROUP_STANDARD_NORMAL, 0);
    
    // Subject effects start off being sampled in a unit normal space:
    sampleNormal_VRR_lp(nspace2_subject_effect_alpha,    0, 1);
    sampleNormal_VRR_lp(nspace2_subject_effect_red_bias, 0, 1);
    sampleNormal_VRR_lp(nspace2_subject_effect_gamma,    0, 1);
    sampleNormal_VRR_lp(nspace2_subject_effect_rho,      0, 1);
    sampleNormal_VRR_lp(nspace2_subject_effect_beta,     0, 1);
    sampleNormal_VRR_lp(nspace2_subject_effect_theta,    0, 1);
    sampleNormal_VRR_lp(nspace2_subject_effect_zeta,    0, 1);

    // ========================================================================
    // Cognitive model
    // ========================================================================

#include /include_model_core_romeu2020_m12_combined.stan

}

generated quantities {
    // ========================================================================
    // Declare
    // ========================================================================

    // ------------------------------------------------------------------------
    // Group means in "task parameter space":
    // ------------------------------------------------------------------------

    vector<lower=0, upper=ALPHA_UPPER_LIMIT_ALT>[N_GROUPS*N_CONDITIONS] group_mean_alpha;
    vector<lower=0, upper=1>[N_GROUPS*N_CONDITIONS] group_mean_red_bias;
    vector<lower=0>[N_GROUPS*N_CONDITIONS] group_mean_gamma;
    vector<lower=0>[N_GROUPS*N_CONDITIONS] group_mean_rho;
    vector<lower=0>[N_GROUPS*N_CONDITIONS] group_mean_beta;
    vector[N_GROUPS*N_CONDITIONS] group_mean_theta;
    vector[N_GROUPS*N_CONDITIONS] group_mean_zeta;

    // ------------------------------------------------------------------------
    // Group differences in means, pairwise, in "task parameter space":
    // ------------------------------------------------------------------------
    // All are indexed such that diff[g1, g2] == value[g1] - value[g2].

    matrix[N_GROUPS*N_CONDITIONS, N_GROUPS*N_CONDITIONS] group_mean_diff_alpha;
    matrix[N_GROUPS*N_CONDITIONS, N_GROUPS*N_CONDITIONS] group_mean_diff_red_bias;
    matrix[N_GROUPS*N_CONDITIONS, N_GROUPS*N_CONDITIONS] group_mean_diff_gamma;
    matrix[N_GROUPS*N_CONDITIONS, N_GROUPS*N_CONDITIONS] group_mean_diff_rho;
    matrix[N_GROUPS*N_CONDITIONS, N_GROUPS*N_CONDITIONS] group_mean_diff_beta;
    matrix[N_GROUPS*N_CONDITIONS, N_GROUPS*N_CONDITIONS] group_mean_diff_theta;
    matrix[N_GROUPS*N_CONDITIONS, N_GROUPS*N_CONDITIONS] group_mean_diff_zeta;

    // ------------------------------------------------------------------------
    // Group differences in intersubject SD, pairwise, in N(0, 0.2) space
    // ------------------------------------------------------------------------
    // All are indexed such that diff[g1, g2] == value[g1] - value[g2].

    matrix[N_GROUPS, N_GROUPS] group_sd_diff_nspace1_alpha;
    matrix[N_GROUPS, N_GROUPS] group_sd_diff_nspace1_red_bias;
    matrix[N_GROUPS, N_GROUPS] group_sd_diff_nspace1_gamma;
    matrix[N_GROUPS, N_GROUPS] group_sd_diff_nspace1_rho;
    matrix[N_GROUPS, N_GROUPS] group_sd_diff_nspace1_beta;
    matrix[N_GROUPS, N_GROUPS] group_sd_diff_nspace1_theta;
    matrix[N_GROUPS, N_GROUPS] group_sd_diff_nspace1_zeta;
    
    // ------------------------------------------------------------------------
    // Main effect of GROUP
    // ------------------------------------------------------------------------

    real overall_group_effect_alpha_control_alcohol;
    real overall_group_effect_red_bias_control_alcohol;
    real overall_group_effect_gamma_control_alcohol;
    real overall_group_effect_rho_control_alcohol;
    real overall_group_effect_beta_control_alcohol;
    real overall_group_effect_theta_control_alcohol;
    real overall_group_effect_zeta_control_alcohol;
    
    real overall_group_effect_alpha_control_poly;
    real overall_group_effect_red_bias_control_poly;
    real overall_group_effect_gamma_control_poly;
    real overall_group_effect_rho_control_poly;
    real overall_group_effect_beta_control_poly;
    real overall_group_effect_theta_control_poly;
    real overall_group_effect_zeta_control_poly;
    
    real overall_group_effect_alpha_alcohol_poly;
    real overall_group_effect_red_bias_alcohol_poly;
    real overall_group_effect_gamma_alcohol_poly;
    real overall_group_effect_rho_alcohol_poly;
    real overall_group_effect_beta_alcohol_poly;
    real overall_group_effect_theta_alcohol_poly;
    real overall_group_effect_zeta_alcohol_poly;
    
    real overall_group_effect_alpha_control_substance;
    real overall_group_effect_red_bias_control_substance;
    real overall_group_effect_gamma_control_substance;
    real overall_group_effect_rho_control_substance;
    real overall_group_effect_beta_control_substance;
    real overall_group_effect_theta_control_substance;
    real overall_group_effect_zeta_control_substance;
    
    real placebo_group_effect_alpha_control_alcohol;
    real placebo_group_effect_red_bias_control_alcohol;
    real placebo_group_effect_gamma_control_alcohol;
    real placebo_group_effect_rho_control_alcohol;
    real placebo_group_effect_beta_control_alcohol;
    real placebo_group_effect_theta_control_alcohol;
    real placebo_group_effect_zeta_control_alcohol;
    
    real placebo_group_effect_alpha_control_poly;
    real placebo_group_effect_red_bias_control_poly;
    real placebo_group_effect_gamma_control_poly;
    real placebo_group_effect_rho_control_poly;
    real placebo_group_effect_beta_control_poly;
    real placebo_group_effect_theta_control_poly;
    real placebo_group_effect_zeta_control_poly;
    
    real placebo_group_effect_alpha_alcohol_poly;
    real placebo_group_effect_red_bias_alcohol_poly;
    real placebo_group_effect_gamma_alcohol_poly;
    real placebo_group_effect_rho_alcohol_poly;
    real placebo_group_effect_beta_alcohol_poly;
    real placebo_group_effect_theta_alcohol_poly;
    real placebo_group_effect_zeta_alcohol_poly;
    
    real placebo_group_effect_alpha_control_substance;
    real placebo_group_effect_red_bias_control_substance;
    real placebo_group_effect_gamma_control_substance;
    real placebo_group_effect_rho_control_substance;
    real placebo_group_effect_beta_control_substance;
    real placebo_group_effect_theta_control_substance;
    real placebo_group_effect_zeta_control_substance;
    
    real naltrexone_group_effect_alpha_control_alcohol;
    real naltrexone_group_effect_red_bias_control_alcohol;
    real naltrexone_group_effect_gamma_control_alcohol;
    real naltrexone_group_effect_rho_control_alcohol;
    real naltrexone_group_effect_beta_control_alcohol;
    real naltrexone_group_effect_theta_control_alcohol;
    real naltrexone_group_effect_zeta_control_alcohol;
    
    real naltrexone_group_effect_alpha_control_poly;
    real naltrexone_group_effect_red_bias_control_poly;
    real naltrexone_group_effect_gamma_control_poly;
    real naltrexone_group_effect_rho_control_poly;
    real naltrexone_group_effect_beta_control_poly;
    real naltrexone_group_effect_theta_control_poly;
    real naltrexone_group_effect_zeta_control_poly;
    
    real naltrexone_group_effect_alpha_alcohol_poly;
    real naltrexone_group_effect_red_bias_alcohol_poly;
    real naltrexone_group_effect_gamma_alcohol_poly;
    real naltrexone_group_effect_rho_alcohol_poly;
    real naltrexone_group_effect_beta_alcohol_poly;
    real naltrexone_group_effect_theta_alcohol_poly;
    real naltrexone_group_effect_zeta_alcohol_poly;
    
    real naltrexone_group_effect_alpha_control_substance;
    real naltrexone_group_effect_red_bias_control_substance;
    real naltrexone_group_effect_gamma_control_substance;
    real naltrexone_group_effect_rho_control_substance;
    real naltrexone_group_effect_beta_control_substance;
    real naltrexone_group_effect_theta_control_substance;
    real naltrexone_group_effect_zeta_control_substance;
    
    // ------------------------------------------------------------------------
    // Main effect of CONDITION
    // ------------------------------------------------------------------------

    real overall_condition_effect_alpha;
    real overall_condition_effect_red_bias;
    real overall_condition_effect_gamma;
    real overall_condition_effect_rho;
    real overall_condition_effect_beta;
    real overall_condition_effect_theta;
    real overall_condition_effect_zeta;
    
    real overall_condition_control_alpha;
    real overall_condition_control_red_bias;
    real overall_condition_control_gamma;
    real overall_condition_control_rho;
    real overall_condition_control_beta;
    real overall_condition_control_theta;
    real overall_condition_control_zeta;
    
    real overall_condition_alcohol_alpha;
    real overall_condition_alcohol_red_bias;
    real overall_condition_alcohol_gamma;
    real overall_condition_alcohol_rho;
    real overall_condition_alcohol_beta;
    real overall_condition_alcohol_theta;
    real overall_condition_alcohol_zeta;
    
    real overall_condition_poly_alpha;
    real overall_condition_poly_red_bias;
    real overall_condition_poly_gamma;
    real overall_condition_poly_rho;
    real overall_condition_poly_beta;
    real overall_condition_poly_theta;
    real overall_condition_poly_zeta;
    
    real overall_condition_substance_alpha;
    real overall_condition_substance_red_bias;
    real overall_condition_substance_gamma;
    real overall_condition_substance_rho;
    real overall_condition_substance_beta;
    real overall_condition_substance_theta;
    real overall_condition_substance_zeta;

    // ========================================================================
    // Calculate
    // ========================================================================
    
    // Group means, vectorized
    group_mean_alpha = Phi(nspace1_group_mean_alpha) * ALPHA_UPPER_LIMIT_ALT;
    group_mean_red_bias = Phi(nspace1_group_mean_red_bias);
    group_mean_gamma = exp(nspace1_group_mean_gamma);
    group_mean_rho = exp(nspace1_group_mean_rho);
    group_mean_beta = exp(nspace1_group_mean_beta);
    group_mean_theta = nspace1_group_mean_theta;
    group_mean_zeta = nspace1_group_mean_zeta;
    
    {
        // In our "generated quantities" block, because we don't know how many
        // groups we have (the code is generic), we use a matrix of group
        // differences. Therefore, we will have to provide some value for
        // "zero" for the diagonal. But Stan will then give a warning that the
        // largest value of Rhat is NaN, even if everything else is converging
        // well. A solution is to return a tiny random number (therefore
        // non-constant across runs). The user just needs to ignore these.
        // See https://discourse.mc-stan.org/t/largest-rhat-is-na-when-one-parameter-is-fixed-to-a-constant/15488/6
        // ... but fixed so that the lower/upper bounds are sensible.
        real tiny_random_number = uniform_rng(-1e-16, 1e-16);

        // Group differences of each groupXcondition set
        for (g1 in 1:(N_GROUPS*N_CONDITIONS)) {
            for (g2 in 1:(N_GROUPS*N_CONDITIONS)) {
    
                if (g1 == g2) {
                    // See explanation in tiny_random_number.
                    group_mean_diff_alpha[g1, g2]    = tiny_random_number;
                    group_mean_diff_red_bias[g1, g2] = tiny_random_number;
                    group_mean_diff_gamma[g1, g2]    = tiny_random_number;
                    group_mean_diff_rho[g1, g2]      = tiny_random_number;
                    group_mean_diff_beta[g1, g2]     = tiny_random_number;
                    group_mean_diff_theta[g1, g2]    = tiny_random_number;
                    group_mean_diff_zeta[g1, g2]     = tiny_random_number;
    
                } else {
                    // We'll do this in a pretty unthinking way; more efficiency
                    // would be possible. Specifically, every value is calculated
                    // twice (e.g. g1 == 3, g2 == 4; then g1 == 4, g2 == 3). But
                    // it makes for shorter and more maintainable code. Also, line
                    // length style is ignored to improve our ability to spot
                    // typos.
                    group_mean_diff_alpha[g1, g2]    = group_mean_alpha[g1]    - group_mean_alpha[g2];
                    group_mean_diff_red_bias[g1, g2] = group_mean_red_bias[g1] - group_mean_red_bias[g2];
                    group_mean_diff_gamma[g1, g2]    = group_mean_gamma[g1]    - group_mean_gamma[g2];
                    group_mean_diff_rho[g1, g2]      = group_mean_rho[g1]      - group_mean_rho[g2];
                    group_mean_diff_beta[g1, g2]     = group_mean_beta[g1]     - group_mean_beta[g2];
                    group_mean_diff_theta[g1, g2]    = group_mean_theta[g1]    - group_mean_theta[g2];
                    group_mean_diff_zeta[g1, g2]     = group_mean_zeta[g1]     - group_mean_zeta[g2];
                }
            }
        }
        
        for (g1 in 1:(N_GROUPS)) {
            for (g2 in 1:(N_GROUPS)) {
    
                if (g1 == g2) {
                    // See explanation in tiny_random_number.
                    group_sd_diff_nspace1_alpha[g1, g2]    = tiny_random_number;
                    group_sd_diff_nspace1_red_bias[g1, g2] = tiny_random_number;
                    group_sd_diff_nspace1_gamma[g1, g2]    = tiny_random_number;
                    group_sd_diff_nspace1_rho[g1, g2]      = tiny_random_number;
                    group_sd_diff_nspace1_beta[g1, g2]     = tiny_random_number;
                    group_sd_diff_nspace1_theta[g1, g2]    = tiny_random_number;
                    group_sd_diff_nspace1_zeta[g1, g2]     = tiny_random_number;
    
                } else {
                    // We'll do this in a pretty unthinking way; more efficiency
                    // would be possible. Specifically, every value is calculated
                    // twice (e.g. g1 == 3, g2 == 4; then g1 == 4, g2 == 3). But
                    // it makes for shorter and more maintainable code. Also, line
                    // length style is ignored to improve our ability to spot
                    // typos.
    
                    group_sd_diff_nspace1_alpha[g1, g2]    = nspace1_group_sd_alpha[g1]    - nspace1_group_sd_alpha[g2];
                    group_sd_diff_nspace1_red_bias[g1, g2] = nspace1_group_sd_red_bias[g1] - nspace1_group_sd_red_bias[g2];
                    group_sd_diff_nspace1_gamma[g1, g2]    = nspace1_group_sd_gamma[g1]    - nspace1_group_sd_gamma[g2];
                    group_sd_diff_nspace1_rho[g1, g2]      = nspace1_group_sd_rho[g1]      - nspace1_group_sd_rho[g2];
                    group_sd_diff_nspace1_beta[g1, g2]     = nspace1_group_sd_beta[g1]     - nspace1_group_sd_beta[g2];
                    group_sd_diff_nspace1_theta[g1, g2]    = nspace1_group_sd_theta[g1]    - nspace1_group_sd_theta[g2];
                    group_sd_diff_nspace1_zeta[g1, g2]     = nspace1_group_sd_zeta[g1]     - nspace1_group_sd_zeta[g2];
                }
            }
        }
        
        //Overall GROUP effects
        overall_group_effect_alpha_control_alcohol = (group_mean_alpha[1] + group_mean_alpha[2]) / 2 - (group_mean_alpha[3] + group_mean_alpha[4]) / 2;
        overall_group_effect_red_bias_control_alcohol = (group_mean_red_bias[1] + group_mean_red_bias[2]) / 2 - (group_mean_red_bias[3] + group_mean_red_bias[4]) / 2;
        overall_group_effect_gamma_control_alcohol = (group_mean_gamma[1] + group_mean_gamma[2]) / 2 - (group_mean_gamma[3] + group_mean_gamma[4]) / 2;
        overall_group_effect_rho_control_alcohol = (group_mean_rho[1] + group_mean_rho[2]) / 2 - (group_mean_rho[3] + group_mean_rho[4]) / 2;
        overall_group_effect_beta_control_alcohol = (group_mean_beta[1] + group_mean_beta[2]) / 2 - (group_mean_beta[3] + group_mean_beta[4]) / 2;
        overall_group_effect_theta_control_alcohol = (group_mean_theta[1] + group_mean_theta[2]) / 2 - (group_mean_theta[3] + group_mean_theta[4]) / 2;
        overall_group_effect_zeta_control_alcohol = (group_mean_zeta[1] + group_mean_zeta[2]) / 2 - (group_mean_zeta[3] + group_mean_zeta[4]) / 2;
        
        overall_group_effect_alpha_control_poly = (group_mean_alpha[1] + group_mean_alpha[2]) / 2 - (group_mean_alpha[5] + group_mean_alpha[6]) / 2;
        overall_group_effect_red_bias_control_poly = (group_mean_red_bias[1] + group_mean_red_bias[2]) / 2 - (group_mean_red_bias[5] + group_mean_red_bias[6]) / 2;
        overall_group_effect_gamma_control_poly = (group_mean_gamma[1] + group_mean_gamma[2]) / 2 - (group_mean_gamma[5] + group_mean_gamma[6]) / 2;
        overall_group_effect_rho_control_poly = (group_mean_rho[1] + group_mean_rho[2]) / 2 - (group_mean_rho[5] + group_mean_rho[6]) / 2;
        overall_group_effect_beta_control_poly = (group_mean_beta[1] + group_mean_beta[2]) / 2 - (group_mean_beta[5] + group_mean_beta[6]) / 2;
        overall_group_effect_theta_control_poly = (group_mean_theta[1] + group_mean_theta[2]) / 2 - (group_mean_theta[5] + group_mean_theta[6]) / 2;
        overall_group_effect_zeta_control_poly = (group_mean_zeta[1] + group_mean_zeta[2]) / 2 - (group_mean_zeta[5] + group_mean_zeta[6]) / 2;
        
        overall_group_effect_alpha_alcohol_poly = (group_mean_alpha[3] + group_mean_alpha[4]) / 2 - (group_mean_alpha[5] + group_mean_alpha[6]) / 2;
        overall_group_effect_red_bias_alcohol_poly = (group_mean_red_bias[3] + group_mean_red_bias[4]) / 2 - (group_mean_red_bias[5] + group_mean_red_bias[6]) / 2;
        overall_group_effect_gamma_alcohol_poly = (group_mean_gamma[3] + group_mean_gamma[4]) / 2 - (group_mean_gamma[5] + group_mean_gamma[6]) / 2;
        overall_group_effect_rho_alcohol_poly = (group_mean_rho[3] + group_mean_rho[4]) / 2 - (group_mean_rho[5] + group_mean_rho[6]) / 2;
        overall_group_effect_beta_alcohol_poly = (group_mean_beta[3] + group_mean_beta[4]) / 2 - (group_mean_beta[5] + group_mean_beta[6]) / 2;
        overall_group_effect_theta_alcohol_poly = (group_mean_theta[3] + group_mean_theta[4]) / 2 - (group_mean_theta[5] + group_mean_theta[6]) / 2;
        overall_group_effect_zeta_alcohol_poly = (group_mean_zeta[3] + group_mean_zeta[4]) / 2 - (group_mean_zeta[5] + group_mean_zeta[6]) / 2;

        overall_group_effect_alpha_control_substance = (group_mean_alpha[1] + group_mean_alpha[2]) / 2 - ((group_mean_alpha[3] + group_mean_alpha[4] + group_mean_alpha[5] + group_mean_alpha[6]) / 4);
        overall_group_effect_red_bias_control_substance = (group_mean_red_bias[1] + group_mean_red_bias[2]) / 2 - ((group_mean_red_bias[3] + group_mean_red_bias[4] + group_mean_red_bias[5] + group_mean_red_bias[6]) / 4);
        overall_group_effect_gamma_control_substance = (group_mean_gamma[1] + group_mean_gamma[2]) / 2 - ((group_mean_gamma[3] + group_mean_gamma[4] + group_mean_gamma[5] + group_mean_gamma[6]) / 4);
        overall_group_effect_rho_control_substance = (group_mean_rho[1] + group_mean_rho[2]) / 2 - ((group_mean_rho[3] + group_mean_rho[4] + group_mean_rho[5] + group_mean_rho[6]) / 4);
        overall_group_effect_beta_control_substance = (group_mean_beta[1] + group_mean_beta[2]) / 2 - ((group_mean_beta[3] + group_mean_beta[4] + group_mean_beta[5] + group_mean_beta[6]) / 4);
        overall_group_effect_theta_control_substance = (group_mean_theta[1] + group_mean_theta[2]) / 2 - ((group_mean_theta[3] + group_mean_theta[4] + group_mean_theta[5] + group_mean_theta[6]) / 4);
        overall_group_effect_zeta_control_substance = (group_mean_zeta[1] + group_mean_zeta[2]) / 2 - ((group_mean_zeta[3] + group_mean_zeta[4] + group_mean_zeta[5] + group_mean_zeta[6]) / 4);
        
        placebo_group_effect_alpha_control_alcohol = group_mean_alpha[1] - group_mean_alpha[3];
        placebo_group_effect_red_bias_control_alcohol = group_mean_red_bias[1] - group_mean_red_bias[3];
        placebo_group_effect_gamma_control_alcohol = group_mean_gamma[1] - group_mean_gamma[3];
        placebo_group_effect_rho_control_alcohol = group_mean_rho[1] - group_mean_rho[3];
        placebo_group_effect_beta_control_alcohol = group_mean_beta[1] - group_mean_beta[3];
        placebo_group_effect_theta_control_alcohol = group_mean_theta[1] - group_mean_theta[3];
        placebo_group_effect_zeta_control_alcohol = group_mean_zeta[1] - group_mean_zeta[3];
        
        placebo_group_effect_alpha_control_poly = group_mean_alpha[1] - group_mean_alpha[5];
        placebo_group_effect_red_bias_control_poly = group_mean_red_bias[1] - group_mean_red_bias[5];
        placebo_group_effect_gamma_control_poly = group_mean_gamma[1] - group_mean_gamma[5];
        placebo_group_effect_rho_control_poly = group_mean_rho[1] - group_mean_rho[5];
        placebo_group_effect_beta_control_poly = group_mean_beta[1] - group_mean_beta[5];
        placebo_group_effect_theta_control_poly = group_mean_theta[1] - group_mean_theta[5];
        placebo_group_effect_zeta_control_poly = group_mean_zeta[1] - group_mean_zeta[5];
        
        placebo_group_effect_alpha_alcohol_poly = group_mean_alpha[3] - group_mean_alpha[5];
        placebo_group_effect_red_bias_alcohol_poly = group_mean_red_bias[3] - group_mean_red_bias[5];
        placebo_group_effect_gamma_alcohol_poly = group_mean_gamma[3] - group_mean_gamma[5];
        placebo_group_effect_rho_alcohol_poly = group_mean_rho[3] - group_mean_rho[5];
        placebo_group_effect_beta_alcohol_poly = group_mean_beta[3] - group_mean_beta[5];
        placebo_group_effect_theta_alcohol_poly = group_mean_theta[3] - group_mean_theta[5];
        placebo_group_effect_zeta_alcohol_poly = group_mean_zeta[3] - group_mean_zeta[5];
        
        placebo_group_effect_alpha_control_substance = group_mean_alpha[1] - ((group_mean_alpha[3] + group_mean_alpha[5]) / 2);
        placebo_group_effect_red_bias_control_substance = group_mean_red_bias[1] - ((group_mean_red_bias[3] + group_mean_red_bias[5]) / 2);
        placebo_group_effect_gamma_control_substance = group_mean_gamma[1] - ((group_mean_gamma[3] + group_mean_gamma[5]) / 2);
        placebo_group_effect_rho_control_substance = group_mean_rho[1] - ((group_mean_rho[3] + group_mean_rho[5]) / 2);
        placebo_group_effect_beta_control_substance = group_mean_beta[1] - ((group_mean_beta[3] + group_mean_beta[5]) / 2);
        placebo_group_effect_theta_control_substance = group_mean_theta[1] - ((group_mean_theta[3] + group_mean_theta[5]) / 2);
        placebo_group_effect_zeta_control_substance = group_mean_zeta[1] - ((group_mean_zeta[3] + group_mean_zeta[5]) / 2);
        
        naltrexone_group_effect_alpha_control_alcohol = group_mean_alpha[2] - group_mean_alpha[4];
        naltrexone_group_effect_red_bias_control_alcohol = group_mean_red_bias[2] - group_mean_red_bias[4];
        naltrexone_group_effect_gamma_control_alcohol = group_mean_gamma[2] - group_mean_gamma[4];
        naltrexone_group_effect_rho_control_alcohol = group_mean_rho[2] - group_mean_rho[4];
        naltrexone_group_effect_beta_control_alcohol = group_mean_beta[2] - group_mean_beta[4];
        naltrexone_group_effect_theta_control_alcohol = group_mean_theta[2] - group_mean_theta[4];
        naltrexone_group_effect_zeta_control_alcohol = group_mean_zeta[2] - group_mean_zeta[4];
        
        naltrexone_group_effect_alpha_control_poly = group_mean_alpha[2] - group_mean_alpha[6];
        naltrexone_group_effect_red_bias_control_poly = group_mean_red_bias[2] - group_mean_red_bias[6];
        naltrexone_group_effect_gamma_control_poly = group_mean_gamma[2] - group_mean_gamma[6];
        naltrexone_group_effect_rho_control_poly = group_mean_rho[2] - group_mean_rho[6];
        naltrexone_group_effect_beta_control_poly = group_mean_beta[2] - group_mean_beta[6];
        naltrexone_group_effect_theta_control_poly = group_mean_theta[2] - group_mean_theta[6];
        naltrexone_group_effect_zeta_control_poly = group_mean_zeta[2] - group_mean_zeta[6];
        
        naltrexone_group_effect_alpha_alcohol_poly = group_mean_alpha[4] - group_mean_alpha[6];
        naltrexone_group_effect_red_bias_alcohol_poly = group_mean_red_bias[4] - group_mean_red_bias[6];
        naltrexone_group_effect_gamma_alcohol_poly = group_mean_gamma[4] - group_mean_gamma[6];
        naltrexone_group_effect_rho_alcohol_poly = group_mean_rho[4] - group_mean_rho[6];
        naltrexone_group_effect_beta_alcohol_poly = group_mean_beta[4] - group_mean_beta[6];
        naltrexone_group_effect_theta_alcohol_poly = group_mean_theta[4] - group_mean_theta[6];
        naltrexone_group_effect_zeta_alcohol_poly = group_mean_zeta[4] - group_mean_zeta[6];
        
        naltrexone_group_effect_alpha_control_substance = group_mean_alpha[2] - ((group_mean_alpha[4] + group_mean_alpha[6]) / 2);
        naltrexone_group_effect_red_bias_control_substance = group_mean_red_bias[2] - ((group_mean_red_bias[4] + group_mean_red_bias[6]) / 2);
        naltrexone_group_effect_gamma_control_substance = group_mean_gamma[2] - ((group_mean_gamma[4] + group_mean_gamma[6]) / 2);
        naltrexone_group_effect_rho_control_substance = group_mean_rho[2] - ((group_mean_rho[4] + group_mean_rho[6]) / 2);
        naltrexone_group_effect_beta_control_substance = group_mean_beta[2] - ((group_mean_beta[4] + group_mean_beta[6]) / 2);
        naltrexone_group_effect_theta_control_substance = group_mean_theta[2] - ((group_mean_theta[4] + group_mean_theta[6]) / 2);
        naltrexone_group_effect_zeta_control_substance = group_mean_zeta[2] - ((group_mean_zeta[4] + group_mean_zeta[6]) / 2);
        
        //Overal CONDITION effects
        overall_condition_effect_alpha = (group_mean_alpha[1] + group_mean_alpha[3] + group_mean_alpha[5]) / 3 - (group_mean_alpha[2] + group_mean_alpha[4] + group_mean_alpha[6]) / 3;
        overall_condition_effect_red_bias = (group_mean_red_bias[1] + group_mean_red_bias[3] + group_mean_red_bias[5]) / 3 - (group_mean_red_bias[2] + group_mean_red_bias[4] + group_mean_red_bias[6]) / 3;
        overall_condition_effect_gamma = (group_mean_gamma[1] + group_mean_gamma[3] + group_mean_gamma[5]) / 3 - (group_mean_gamma[2] + group_mean_gamma[4] + group_mean_gamma[6]) / 3;
        overall_condition_effect_rho = (group_mean_rho[1] + group_mean_rho[3] + group_mean_rho[5]) / 3 - (group_mean_rho[2] + group_mean_rho[4] + group_mean_rho[6]) / 3;
        overall_condition_effect_beta = (group_mean_beta[1] + group_mean_beta[3] + group_mean_beta[5]) / 3 - (group_mean_beta[2] + group_mean_beta[4] + group_mean_beta[6]) / 3;
        overall_condition_effect_theta = (group_mean_theta[1] + group_mean_theta[3] + group_mean_theta[5]) / 3 - (group_mean_theta[2] + group_mean_theta[4] + group_mean_theta[6]) / 3;
        overall_condition_effect_zeta = (group_mean_zeta[1] + group_mean_zeta[3] + group_mean_zeta[5]) / 3 - (group_mean_zeta[2] + group_mean_zeta[4] + group_mean_zeta[6]) / 3;
        
        overall_condition_control_alpha = group_mean_alpha[1] - group_mean_alpha[2];
        overall_condition_control_red_bias = group_mean_red_bias[1] - group_mean_red_bias[2];
        overall_condition_control_gamma = group_mean_gamma[1] - group_mean_gamma[2];
        overall_condition_control_rho = group_mean_rho[1] - group_mean_rho[2];
        overall_condition_control_beta = group_mean_beta[1] - group_mean_beta[2];
        overall_condition_control_theta = group_mean_theta[1] - group_mean_theta[2];
        overall_condition_control_zeta = group_mean_zeta[1] - group_mean_zeta[2];
        
        overall_condition_alcohol_alpha = group_mean_alpha[3] - group_mean_alpha[4];
        overall_condition_alcohol_red_bias = group_mean_red_bias[3] - group_mean_red_bias[4];
        overall_condition_alcohol_gamma = group_mean_gamma[3] - group_mean_gamma[4];
        overall_condition_alcohol_rho = group_mean_rho[3] - group_mean_rho[4];
        overall_condition_alcohol_beta = group_mean_beta[3] - group_mean_beta[4];
        overall_condition_alcohol_theta = group_mean_theta[3] - group_mean_theta[4];
        overall_condition_alcohol_zeta = group_mean_zeta[3] - group_mean_zeta[4];
        
        overall_condition_poly_alpha = group_mean_alpha[5] - group_mean_alpha[6];
        overall_condition_poly_red_bias = group_mean_red_bias[5] - group_mean_red_bias[6];
        overall_condition_poly_gamma = group_mean_gamma[5] - group_mean_gamma[6];
        overall_condition_poly_rho = group_mean_rho[5] - group_mean_rho[6];
        overall_condition_poly_beta = group_mean_beta[5] - group_mean_beta[6];
        overall_condition_poly_theta = group_mean_theta[5] - group_mean_theta[6];
        overall_condition_poly_zeta = group_mean_zeta[5] - group_mean_zeta[6];
 
        overall_condition_substance_alpha = (group_mean_alpha[5] + group_mean_alpha[3]) / 2 - (group_mean_alpha[6] + group_mean_alpha[4]) / 2;
        overall_condition_substance_red_bias = (group_mean_red_bias[5] + group_mean_red_bias[3]) / 2 - (group_mean_red_bias[6] + group_mean_red_bias[4]) / 2;
        overall_condition_substance_gamma = (group_mean_gamma[5] + group_mean_gamma[3]) / 2 - (group_mean_gamma[6] + group_mean_gamma[4]) / 2;
        overall_condition_substance_rho = (group_mean_rho[5] + group_mean_rho[3]) / 2 - (group_mean_rho[6] + group_mean_rho[4]) / 2;
        overall_condition_substance_beta = (group_mean_beta[5] + group_mean_beta[3]) / 2 - (group_mean_beta[6] + group_mean_beta[4]) / 2;
        overall_condition_substance_theta = (group_mean_theta[5] + group_mean_theta[3]) / 2 - (group_mean_theta[6] + group_mean_theta[4]) / 2;
        overall_condition_substance_zeta = (group_mean_zeta[5] + group_mean_zeta[3]) / 2 - (group_mean_zeta[6] + group_mean_zeta[4]) / 2;

    }
}
