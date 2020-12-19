    // ========================================================================
    // Core data for all models
    // ========================================================================

    int N_GROUPS;
    int N_SUBJECTS;  // subject_num goes from 1:N_SUBJECTS
    int N_TRIALS;  // TOTAL number of trials

    int<lower=1, upper=N_GROUPS> group_num_by_subject[N_SUBJECTS];
    int<lower=1, upper=N_SUBJECTS> subject_num_by_trial[N_TRIALS];
    // ... you might think one could include them in all models, and ignore
    // them for a single-subject model. However, if you're not careful, then
    // with a single subject, you get a single number rather than an array, as
    // Stan sees it, and you get this error:
    //      Exception: mismatch in number dimensions declared and found in
    //      context; processing stage=data initialization;
    //      variable name=group_num_by_subject; dims declared=(1); dims found=()
    // The solution is to do this in R:
    //      y = array(1.0, dim = 1)
    // ... see ?rstan::stan.

    int<lower=0, upper=1> ascending_bets[N_TRIALS];
    int<lower=0> starting_points[N_TRIALS];
    int<lower=1, upper=9> n_red[N_TRIALS];  // (N_LOCATIONS - 1) is not yet defined
    int<lower=0, upper=1> chose_red[N_TRIALS];
    int<lower=1, upper=5> chosen_bet_index[N_TRIALS];  // N_BET_OPTIONS is not yet defined
