    // ------------------------------------------------------------------------
    // Subject/group handling
    // ------------------------------------------------------------------------

    int s = -1;  // current subject number, from 1:N_SUBJECTS

    // Working copies of the per-subject parameters:
    real alpha;
    real red_bias;
    real gamma;
    real rho;
    real beta;

    // ------------------------------------------------------------------------
    // Core model variables
    // ------------------------------------------------------------------------

    // Working variables for the task.
    real proportion_red;
    real proportion_of_boxes_for_chosen_colour;

    // Working variables for the cognitive model.
    real p_subject_chooses_red;
    vector[N_BET_OPTIONS] penalized_expected_utility;
    vector[N_BET_OPTIONS] p_each_bet;
