    vector getPenalizedExpectedUtility11(
            real colour_utility,
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
            utilityFunction3(x_capital_after_win, rho);
        vector[n] utility_after_loss =
            utilityFunction4(y_capital_after_loss, rho);

        real other_colour_utility = 1 - colour_utility;
        vector[n] expected_utility =
            colour_utility * utility_after_win + other_colour_utility * utility_after_loss;

        vector[n] cost_of_bet_position =
            (bet_position - 1) / (n_bet_options - 1);
        vector[n] penalty_for_waiting = beta * cost_of_bet_position;

        return expected_utility - penalty_for_waiting;
    }


    vector getPenalizedExpectedUtility10(
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
            utilityFunction4(x_capital_after_win, rho);
        vector[n] utility_after_loss =
            utilityFunction3(y_capital_after_loss, rho);

        real p_loss = 1 - p_win;
        vector[n] expected_utility =
            p_win * utility_after_win + p_loss * utility_after_loss;

        vector[n] cost_of_bet_position =
            (bet_position - 1) / (n_bet_options - 1);
        vector[n] penalty_for_waiting = beta * cost_of_bet_position;

        return expected_utility - penalty_for_waiting;
    }

    vector getPenalizedExpectedUtility9(
            real colour_utility,
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
            utilityFunction4(x_capital_after_win, rho);
        vector[n] utility_after_loss =
            utilityFunction3(y_capital_after_loss, rho);

        real other_colour_utility = 1 - colour_utility;
        vector[n] expected_utility =
            colour_utility * utility_after_win + other_colour_utility * utility_after_loss;

        vector[n] cost_of_bet_position =
            (bet_position - 1) / (n_bet_options - 1);
        vector[n] penalty_for_waiting = beta * cost_of_bet_position;

        return expected_utility - penalty_for_waiting;
    }

    vector getPenalizedExpectedUtility4(
            real p_win,
            real current_points,
            vector proportion_bet,
            real rho,
            real delta,
            vector bet_position,
            real beta,
            int n_bet_options)
    {
        int n = num_elements(proportion_bet);
        // ... the number of bets being considered
        // ... in practice, is always n_bet_options!
        real scaled_capital = current_points / 100;
        vector[n] stake = getStakeVector(scaled_capital, proportion_bet);
        vector[n] x_capital_after_win = stake;
        vector[n] utility_after_win =
            utilityFunction4(x_capital_after_win, rho);
        vector[n] utility_after_loss =
            utilityFunction2(x_capital_after_win, rho, delta);

        real p_loss = 1 - p_win;
        vector[n] expected_utility =
            p_win * utility_after_win + p_loss * utility_after_loss;

        vector[n] cost_of_bet_position =
            (bet_position - 1) / (n_bet_options - 1);
        vector[n] penalty_for_waiting = beta * cost_of_bet_position;

        return expected_utility - penalty_for_waiting;
    }

    vector getPenalizedExpectedUtility3(
            real colour_utility,
            real current_points,
            vector proportion_bet,
            real rho,
            real delta,
            vector bet_position,
            real beta,
            int n_bet_options)
    {
        int n = num_elements(proportion_bet);
        // ... the number of bets being considered
        // ... in practice, is always n_bet_options!
        real scaled_capital = current_points / 100;
        vector[n] stake = getStakeVector(scaled_capital, proportion_bet);
        vector[n] x_capital_after_win = stake;
        vector[n] utility_after_win =
            utilityFunction4(x_capital_after_win, rho);
        vector[n] utility_after_loss =
            utilityFunction2(x_capital_after_win, rho, delta);

        real other_colour_utility = 1 - colour_utility;
        vector[n] expected_utility =
            colour_utility * utility_after_win + other_colour_utility * utility_after_loss;

        vector[n] cost_of_bet_position =
            (bet_position - 1) / (n_bet_options - 1);
        vector[n] penalty_for_waiting = beta * cost_of_bet_position;

        return expected_utility - penalty_for_waiting;
    }

    vector getPenalizedExpectedUtility2(
            real p_win,
            real current_points,
            vector proportion_bet,
            real rho,
            real delta,
            vector bet_position,
            real beta,
            int n_bet_options)
    {
        int n = num_elements(proportion_bet);
        // ... the number of bets being considered
        // ... in practice, is always n_bet_options!
        real scaled_capital = current_points / 100;
        vector[n] stake = getStakeVector(scaled_capital, proportion_bet);
        vector[n] x_capital_after_win = stake;
        vector[n] utility_after_win =
            utilityFunction0(x_capital_after_win, rho);
        vector[n] utility_after_loss =
            utilityFunction1(x_capital_after_win, rho, delta);

        real p_loss = 1 - p_win;
        vector[n] expected_utility =
            p_win * utility_after_win + p_loss * utility_after_loss;

        vector[n] cost_of_bet_position =
            (bet_position - 1) / (n_bet_options - 1);
        vector[n] penalty_for_waiting = beta * cost_of_bet_position;

        return expected_utility - penalty_for_waiting;
    }

    vector utilityFunctionOriginal(vector x, real rho) //The original utility function, left for reference
    {
        int n = num_elements(x);
        vector[n] utility;
        real v;
        for (i in 1:n) {
            v = x[i];
            utility[i] = v >= 0
                ? log(1 + v)
                : log(1 + rho * v);
        }
        return utility;
    }

    vector utilityFunction0(vector x, real rho)
    {
        int n = num_elements(x);
        vector[n] utility;
        real v;
        real Factor;
        for (i in 1:n) {
            v = x[i];
            Factor = pow(v, rho);
            utility[i] = Factor;
        }
        return utility;
    }

    vector utilityFunction1(vector x, real rho, real delta)
    {
        int n = num_elements(x);
        vector[n] utility;
        real v;
        real Factor;
        for (i in 1:n) {
            v = x[i];
            Factor = pow(v, rho);
            utility[i] = (-1) * delta * Factor;
        }
        return utility;
    }


    vector utilityFunction2(vector x, real rho, real delta)
    {
        int n = num_elements(x);
        vector[n] utility;
        real v;
        for (i in 1:n) {
            v = x[i];
            utility[i] = (-1) * delta * log(1 + v * rho);
        }
        return utility;
    }
    
    