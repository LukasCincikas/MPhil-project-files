    // ------------------------------------------------------------------------
    // Common model variables
    // ------------------------------------------------------------------------

    // Working variables for the task.
    real proportion_red;
    real proportion_of_boxes_for_chosen_colour;

    // Working variables for the cognitive model.
    real p_subject_chooses_red;
    vector[N_BET_OPTIONS] penalized_expected_utility;
    vector[N_BET_OPTIONS] p_each_bet;
