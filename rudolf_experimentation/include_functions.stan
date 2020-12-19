    // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // Functions
    // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    // See generate_synthetic_cambridge_gamble_data.R

    real pChooseRed(real r, real alpha, real c)
    {
        // r: proportion_red_offered
        // c: red_bias
        real u = 1 - r;
        return c * r ^ alpha / (c * r ^ alpha + (1 - c) * u ^ alpha);
    }

    real getStake(real current_points, real proportion_bet)
    {
        // No rounding (not needed).
        return current_points * proportion_bet;
    }

    vector getStakeVector(real current_points, vector proportion_bet)
    {
        // No rounding (not needed).
        return current_points * proportion_bet;
    }

    vector utilityFunction(vector x, real rho)
    {
        int n = num_elements(x);
        vector[n] utility;
        real v;
        for (i in 1:n) {
            v = x[i];
            utility[i] = v >= 0 ? log(1 + v) : log(1 + rho * v);
        }
        return utility;
    }

    vector getPenalizedExpectedUtility(
            real p_win,
            real current_points,
            vector proportion_bet,
            real rho,
            vector bet_position,
            real beta,
            int n_bet_options)
    {
        int n = num_elements(proportion_bet);
        // ... the number of bets being considered
        // ... in practice, is always n_bet_options!
        real scaled_capital = current_points / 100;
        vector[n] stake = getStakeVector(scaled_capital, proportion_bet);
        vector[n] x_capital_after_win = scaled_capital + stake;
        vector[n] y_capital_after_loss = scaled_capital - stake;
        vector[n] utility_after_win =
            utilityFunction(x_capital_after_win, rho);
        vector[n] utility_after_loss =
            utilityFunction(y_capital_after_loss, rho);

        real p_loss = 1 - p_win;
        vector[n] expected_utility =
            p_win * utility_after_win + p_loss * utility_after_loss;

        vector[n] cost_of_bet_position =
            (bet_position - 1) / (n_bet_options - 1);
        vector[n] penalty_for_waiting = beta * cost_of_bet_position;

        return expected_utility - penalty_for_waiting;
    }
