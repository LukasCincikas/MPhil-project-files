    // ========================================================================
    // Data
    // ========================================================================

    // Constants:
    int N_TRIALS;  // TOTAL number of trials

    // Arrays:
    int<lower=0, upper=1> ascending_bets[N_TRIALS];
    int<lower=0> starting_points[N_TRIALS];
    int<lower=1, upper=9> n_red[N_TRIALS];  // (N_LOCATIONS - 1) not yet defined
    int<lower=0, upper=1> chose_red[N_TRIALS];
    int<lower=1, upper=5> chosen_bet_index[N_TRIALS];  // N_BET_OPTIONS not yet defined
