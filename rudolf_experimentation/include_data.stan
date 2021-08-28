    // ========================================================================
    //  OLD Core data for all models
    // ========================================================================

    int N_GROUPS;  // ignored for independent-subjects model
    int N_SUBJECTS;  // subject_num goes from 1:N_SUBJECTS
    int N_TRIALS;  // TOTAL number of trials
    int N_CONDITIONS;
    int N_SEGMENTS;

    int<lower=1, upper=N_GROUPS> group_num_by_subject[N_SUBJECTS];
    // ... ignored for independent-subjects model
    //
    // NOTE: if you're not careful in R, then with a single subject, Stan will
    // receive an integer rather than an array, and you will get this error:
    //      Exception: mismatch in number dimensions declared and found in
    //      context; processing stage=data initialization;
    //      variable name=group_num_by_subject; dims declared=(1); dims found=()
    // The solution is to use "as.array()" in R.

    int<lower=1, upper=N_SUBJECTS> subject_num_by_trial[N_TRIALS];
    int<lower=1, upper=N_CONDITIONS> condition_num_by_subject[N_SUBJECTS];
    int<lower=1, upper=N_SEGMENTS> segment_num_by_subject[N_SUBJECTS];

    int<lower=0, upper=1> ascending_bets[N_TRIALS];
    int<lower=0> starting_points[N_TRIALS];
    int<lower=1, upper=9> n_red[N_TRIALS];  // magic number as (N_LOCATIONS - 1) is not yet defined
    int<lower=0, upper=1> chose_red[N_TRIALS];
    int<lower=1, upper=5> chosen_bet_index[N_TRIALS];  // magic number as N_BET_OPTIONS is not yet defined
