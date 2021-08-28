    // ========================================================================
    // Task constants
    // ========================================================================
    real N_LOCATIONS = 10;  // real to force floating-point division
    int N_BET_OPTIONS = 5;
    vector[N_BET_OPTIONS] BET_FRACTION_OPTIONS = [0.05, 0.25, 0.5, 0.75, 0.95]';
    vector[N_BET_OPTIONS] BET_POSITION_ASCENDING = [1, 2, 3, 4, 5]';
    vector[N_BET_OPTIONS] BET_POSITION_DESCENDING = [5, 4, 3, 2, 1]';
    // ... note ' for transpose; the [a, b...] notation defines a row_vector,
    // not a vector.

    // ========================================================================
    // Modelling constants
    // ========================================================================
    real ALPHA_UPPER_LIMIT = 5;  // range [0,5]: [Romeu2020]
    real ALPHA_UPPER_LIMIT_ALT = 10; // Modified range for updated models
    real PRIOR_SD_FOR_GROUP_STANDARD_NORMAL = 0.2;  // [Romeu2020], suppl. p.14
