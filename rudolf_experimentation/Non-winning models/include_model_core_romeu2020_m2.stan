    // Cognitive model

    for (t in 1:N_TRIALS) {

        if (subject_num_by_trial[t] != s) {
            // Reset for new subject
            s = subject_num_by_trial[t];
            // This subject's parameters:
            alpha = subject_alpha[s];
            red_bias = subject_red_bias[s];
            gamma = subject_gamma[s];
            rho = subject_rho[s];
            beta = subject_beta[s];
            delta = subject_delta[s];
        }

        // --------------------------------------------------------------------
        // (a) Predict which colour was chosen.
        // --------------------------------------------------------------------

        proportion_red = n_red[t] / N_LOCATIONS;

        p_subject_chooses_red = pChooseRed(proportion_red, alpha, red_bias);

        // --------------------------------------------------------------------
        // (b) Predict how much was bet.
        // --------------------------------------------------------------------
        // Our prediction for proportion_staked (and the actual value of
        // proportion_staked) is conditional upon the choice of colour, and so
        // we're allowed to know which colour the subject actually chose.

        proportion_of_boxes_for_chosen_colour = chose_red[t]
            ? proportion_red
            : 1 - proportion_red;

        penalized_expected_utility = getPenalizedExpectedUtility2(
            proportion_of_boxes_for_chosen_colour,
            starting_points[t],
            BET_FRACTION_OPTIONS,
            rho,
            delta,
            ascending_bets[t]
                ? BET_POSITION_ASCENDING
                : BET_POSITION_DESCENDING,
            beta,
            N_BET_OPTIONS
        );

        p_each_bet = softmax(penalized_expected_utility * gamma);

        // --------------------------------------------------------------------
        // Fit model to the world.
        // --------------------------------------------------------------------
        // We could vectorize this for p_subject_chooses_red, but not easily
        // for the bet size, so simpler to do both per trial.

        // (a) Chosen colour
        //     - bernoulli_lpmf(y | theta), where
        //       - y is in {0, 1}
        //       - theta is a probability
        target += bernoulli_lpmf(chose_red[t] | p_subject_chooses_red);

        // (b) Bet size
        //     - categorical_lpmf(y | theta), where
        //       - y is an integer in {1, ..., K}
        //       - theta is a K-simplex of the probabilities of the K outcomes
        target += categorical_lpmf(chosen_bet_index[t] | p_each_bet);

    }
