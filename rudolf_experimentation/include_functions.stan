    // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // Functions
    // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    // For detailed thinking, see generate_synthetic_cambridge_gamble_data.R
    // Some functions not directly needed for the winning model have been moved to obsolete_functions.stan in the 'Non-winning models' folder

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

    real pChooseRed(real r, real alpha, real c)
    {
        // r: proportion_red_offered
        // c: red_bias
        real u = 1 - r;
        return c * r ^ alpha / (c * r ^ alpha + (1 - c) * u ^ alpha);
    }

    real pChooseRedLeftWon(real r, real alpha, real c, real phi)
    {
        // r: proportion_red_offered
        // c: red_bias
        real u = 1 - r;
        return c ^ phi * r ^ alpha / (c ^ phi * r ^ alpha + (1 - c ^ phi) * u ^ alpha);
    }
    
    real pChooseRedLeftLost(real r, real alpha, real c, real phi)
    {
        // r: proportion_red_offered
        // c: red_bias
        real u = 1 - r;
        return c ^ (1 / phi) * r ^ alpha / (c ^ (1 /phi) * r ^ alpha + (1 - c ^ (1 / phi)) * u ^ alpha);
    }

    real pChooseRedAlt(real r, real alpha)
    {
        // r: proportion_red_offered
        real u = 1 - r;
        return  r ^ alpha / (r ^ alpha + u ^ alpha);
    }
    
    real pChooseRedNoAlpha(real r, real c)
    {
        // r: proportion_red_offered
        // c: red_bias
        real u = 1 - r;
        return c * r / (c * r + (1 - c) * u);
    }
    
        real pChooseRedLeftWonNoAlpha(real r, real c, real phi)
    {
        // r: proportion_red_offered
        // c: red_bias
        real u = 1 - r;
        return c ^ phi * r / (c ^ phi * r + (1 - c ^ phi) * u);
    }
    
    real pChooseRedLeftLostNoAlpha(real r, real c, real phi)
    {
        // r: proportion_red_offered
        // c: red_bias
        real u = 1 - r;
        return c ^ (1 / phi) * r / (c ^ (1 / phi) * r + (1 - c ^ (1 / phi)) * u);
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

    vector utilityFunction3(vector x, real rho)
    {
        int n = num_elements(x);
        vector[n] utility;
        real v;
        for (i in 1:n) {
            v = x[i];
            utility[i] = log(1 + v);
        }
        return utility;
    }
    
    vector utilityFunction4(vector x, real rho)
    {
        int n = num_elements(x);
        vector[n] utility;
        real v;
        for (i in 1:n) {
            v = x[i];
            utility[i] = log(1 + v * rho);
        }
        return utility;
    }
    
    vector utilityFunctionLoss1(vector x, real rho, real points_ratio)
    {
        int n = num_elements(x);
        vector[n] utility;
        real v;
        for (i in 1:n) {
            v = x[i];
            utility[i] = log(1 + v * (points_ratio ^ rho));
        }
        return utility;
    }
    
    vector utilityFunctionLoss2(vector x, real rho, real points_ratio, real theta)
    {
        int n = num_elements(x);
        vector[n] utility;
        real v;
        for (i in 1:n) {
            v = x[i];
            utility[i] = log(1 + v * theta * (points_ratio ^ rho));
        }
        return utility;
    }

    //WORKING ON THIS
    vector riskAdjustment(real theta, vector utility_after_win, vector utility_after_loss, real p_win)
    {
        int n = num_elements(utility_after_win);
        vector[n] expected_utility;
        real p_loss = 1 - p_win;
        for(i in 1:n) {
            expected_utility[i] =
                p_win ^ theta * utility_after_win[i] + p_loss ^ theta * utility_after_loss[i];
        }
        return expected_utility;
    }

        vector getPenalizedExpectedUtilityCombined2(
            real p_win,
            real current_points,
            vector proportion_bet,
            real rho,
            real points_ratio,
            vector bet_position,
            real beta,
            real theta,
            real zeta,
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

        real p_loss = 1 - p_win;
        vector[n] expected_utility =
            p_win * zeta * utility_after_win * points_ratio ^ theta + p_loss * utility_after_loss;

        vector[n] cost_of_bet_position =
            (bet_position - 1) / (n_bet_options - 1);
        vector[n] penalty_for_waiting = beta * cost_of_bet_position;

        return expected_utility - penalty_for_waiting;
    }

        vector getPenalizedExpectedUtilityCombined(
            real p_win,
            real current_points,
            vector proportion_bet,
            real rho,
            real points_ratio,
            vector bet_position,
            real beta,
            real theta,
            real zeta,
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

        real p_loss = 1 - p_win;
        vector[n] expected_utility =
            p_win ^ zeta * utility_after_win * points_ratio ^ theta + p_loss * utility_after_loss;

        vector[n] cost_of_bet_position =
            (bet_position - 1) / (n_bet_options - 1);
        vector[n] penalty_for_waiting = beta * cost_of_bet_position;

        return expected_utility - penalty_for_waiting;
    }

    vector getPenalizedExpectedUtility12(
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
            utilityFunction3(x_capital_after_win, rho);
        vector[n] utility_after_loss =
            utilityFunction4(y_capital_after_loss, rho);

        real p_loss = 1 - p_win;
        vector[n] expected_utility =
            p_win * utility_after_win + p_loss * utility_after_loss;

        vector[n] cost_of_bet_position =
            (bet_position - 1) / (n_bet_options - 1);
        vector[n] penalty_for_waiting = beta * cost_of_bet_position;

        return expected_utility - penalty_for_waiting;
    }
    
        vector getPenalizedExpectedUtilityLoss3E(
            real p_win,
            real current_points,
            vector proportion_bet,
            real rho,
            real points_ratio,
            real points_ratio_loss,
            vector bet_position,
            real beta,
            real theta,
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

        real p_loss = 1 - p_win;
        vector[n] expected_utility =
            p_win * utility_after_win * points_ratio ^ theta + p_loss * utility_after_loss * points_ratio_loss ^ theta;

        vector[n] cost_of_bet_position =
            (bet_position - 1) / (n_bet_options - 1);
        vector[n] penalty_for_waiting = beta * cost_of_bet_position;

        return expected_utility - penalty_for_waiting;
    }
    
        vector getPenalizedExpectedUtilityLoss3D(
            real p_win,
            real current_points,
            vector proportion_bet,
            real rho,
            real points_ratio,
            vector bet_position,
            real beta,
            real theta,
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

        real p_loss = 1 - p_win;
        vector[n] expected_utility =
            p_win * utility_after_win * points_ratio ^ theta + p_loss * utility_after_loss * points_ratio ^ theta;

        vector[n] cost_of_bet_position =
            (bet_position - 1) / (n_bet_options - 1);
        vector[n] penalty_for_waiting = beta * cost_of_bet_position;

        return expected_utility - penalty_for_waiting;
    }
    
        vector getPenalizedExpectedUtilityLoss3C(
            real p_win,
            real current_points,
            vector proportion_bet,
            real rho,
            real points_ratio,
            vector bet_position,
            real beta,
            real theta,
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

        real p_loss = 1 - p_win;
        vector[n] expected_utility =
            p_win * utility_after_win * points_ratio ^ theta + p_loss * utility_after_loss;

        vector[n] cost_of_bet_position =
            (bet_position - 1) / (n_bet_options - 1);
        vector[n] penalty_for_waiting = beta * cost_of_bet_position;

        return expected_utility - penalty_for_waiting;
    }
    
        vector getPenalizedExpectedUtilityLoss3B(
            real p_win,
            real current_points,
            vector proportion_bet,
            real rho,
            real points_ratio,
            vector bet_position,
            real beta,
            real theta,
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

        real p_loss = 1 - p_win;
        vector[n] expected_utility =
            p_win * utility_after_win * points_ratio ^ theta + p_loss * utility_after_loss;

        vector[n] cost_of_bet_position =
            (bet_position - 1) / (n_bet_options - 1);
        vector[n] penalty_for_waiting = beta * cost_of_bet_position;

        return expected_utility - penalty_for_waiting;
    }
    
        vector getPenalizedExpectedUtilityLoss3(
            real p_win,
            real current_points,
            vector proportion_bet,
            real rho,
            real points_ratio,
            vector bet_position,
            real beta,
            real theta,
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

        real p_loss = 1 - p_win;
        vector[n] expected_utility =
            p_win * utility_after_win + p_loss * utility_after_loss * points_ratio ^ theta;

        vector[n] cost_of_bet_position =
            (bet_position - 1) / (n_bet_options - 1);
        vector[n] penalty_for_waiting = beta * cost_of_bet_position;

        return expected_utility - penalty_for_waiting;
    }
    
    vector getPenalizedExpectedUtilityLoss2(
            real p_win,
            real current_points,
            vector proportion_bet,
            real rho,
            real points_ratio,
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

        real p_loss = 1 - p_win;
        vector[n] expected_utility =
            p_win * utility_after_win + p_loss * utility_after_loss * points_ratio;

        vector[n] cost_of_bet_position =
            (bet_position - 1) / (n_bet_options - 1);
        vector[n] penalty_for_waiting = beta * cost_of_bet_position;

        return expected_utility - penalty_for_waiting;
    }
    
    vector getPenalizedExpectedUtilityLoss(
            real p_win,
            real current_points,
            vector proportion_bet,
            real rho,
            real points_ratio,
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
            utilityFunctionLoss1(y_capital_after_loss, rho, points_ratio);

        real p_loss = 1 - p_win;
        vector[n] expected_utility =
            p_win * utility_after_win + p_loss * utility_after_loss;

        vector[n] cost_of_bet_position =
            (bet_position - 1) / (n_bet_options - 1);
        vector[n] penalty_for_waiting = beta * cost_of_bet_position;

        return expected_utility - penalty_for_waiting;
    }
    
        vector getPenalizedExpectedUtilityExponential(
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
            utilityFunction3(x_capital_after_win, rho);
        vector[n] utility_after_loss =
            utilityFunction4(y_capital_after_loss, rho);

        real p_loss = 1 - p_win;
        vector[n] expected_utility =
            p_win * utility_after_win + p_loss * utility_after_loss;
            
        vector[n] penalty_for_waiting =
            exp(-1*beta*(bet_position - 1));
            
        vector[n] return_value;
        
        for (i in 1:n) {
            return_value[i] = expected_utility[i] * penalty_for_waiting[i];
        }

        return (return_value);
    }
    
        vector getPenalizedExpectedUtilityHyperbolic(
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
            utilityFunction3(x_capital_after_win, rho);
        vector[n] utility_after_loss =
            utilityFunction4(y_capital_after_loss, rho);

        real p_loss = 1 - p_win;
        vector[n] expected_utility =
            p_win * utility_after_win + p_loss * utility_after_loss;

        vector[n] penalty_for_waiting =
            1 + beta * (bet_position - 1);
        
        vector[n] return_value;
        
        for(i in 1:n) {
            return_value[i] = expected_utility[i] / penalty_for_waiting[i];
        }

        return return_value;
    }

        vector getPenalizedExpectedUtilityHyperboloid(
            real p_win,
            real current_points,
            vector proportion_bet,
            real rho,
            vector bet_position,
            real beta,
            real sens,
            int n_bet_options)
    {
        int n = num_elements(proportion_bet);
        vector[n] penalty_for_waiting;
        vector[n] return_value;
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

        real p_loss = 1 - p_win;
        vector[n] expected_utility =
            p_win * utility_after_win + p_loss * utility_after_loss;
            
        for(b in 1:n) {
            penalty_for_waiting[b] = pow((1 + beta * (bet_position[b] - 1)), sens);
        }
        
        for(i in 1:n) {
            return_value[i] = expected_utility[i] / penalty_for_waiting[i];
        }

        return return_value;
    }

    vector getPenalizedExpectedUtilityRiskAdjustment1A(
            real p_win,
            real current_points,
            vector proportion_bet,
            real rho,
            vector bet_position,
            real beta,
            real theta,
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

        real p_loss = 1 - p_win;
        real p_win_alt = p_win * theta;
        vector[n] expected_utility =
            p_win_alt * utility_after_win + p_loss * utility_after_loss;

        vector[n] cost_of_bet_position =
            (bet_position - 1) / (n_bet_options - 1);
        vector[n] penalty_for_waiting = beta * cost_of_bet_position;

        return expected_utility - penalty_for_waiting;
    }
    
    vector getPenalizedExpectedUtilityRiskAdjustment1B(
            real p_win,
            real current_points,
            vector proportion_bet,
            real rho,
            vector bet_position,
            real beta,
            real theta,
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

        real p_loss = 1 - p_win;
        real p_win_alt = p_win * theta;
        vector[n] expected_utility =
            p_win_alt * utility_after_win + p_loss * utility_after_loss;

        vector[n] cost_of_bet_position =
            (bet_position - 1) / (n_bet_options - 1);
        vector[n] penalty_for_waiting = beta * cost_of_bet_position;

        return expected_utility - penalty_for_waiting;
    }

    vector getPenalizedExpectedUtilityRiskAdjustment2A(
            real p_win,
            real current_points,
            vector proportion_bet,
            real rho,
            vector bet_position,
            real beta,
            real theta,
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

        real p_loss = 1 - p_win;
        real p_win_alt = p_win ^ theta;
        vector[n] expected_utility =
            p_win_alt * utility_after_win + p_loss * utility_after_loss;

        vector[n] cost_of_bet_position =
            (bet_position - 1) / (n_bet_options - 1);
        vector[n] penalty_for_waiting = beta * cost_of_bet_position;

        return expected_utility - penalty_for_waiting;
    }
    
    vector getPenalizedExpectedUtilityRiskAdjustment3A(
            real p_win,
            real current_points,
            vector proportion_bet,
            real rho,
            vector bet_position,
            real beta,
            real theta,
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

        real p_loss = (1 - p_win) ^ theta;
        real p_win_alt = p_win ^ theta;
        vector[n] expected_utility =
            p_win_alt * utility_after_win + p_loss * utility_after_loss;

        vector[n] cost_of_bet_position =
            (bet_position - 1) / (n_bet_options - 1);
        vector[n] penalty_for_waiting = beta * cost_of_bet_position;

        return expected_utility - penalty_for_waiting;
    }
    
    vector getPenalizedExpectedUtilityRiskAdjustment4(
            real p_win,
            real current_points,
            vector proportion_bet,
            real rho,
            vector bet_position,
            real beta,
            real theta,
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

        real p_loss = 1 - p_win;
        vector[n] expected_utility =
            p_win ^ theta * utility_after_win + p_loss * utility_after_loss;

        vector[n] cost_of_bet_position =
            (bet_position - 1) / (n_bet_options - 1);
        vector[n] penalty_for_waiting = beta * cost_of_bet_position;

        return expected_utility - penalty_for_waiting;
    }
    
        vector getPenalizedExpectedUtilityRiskAdjustment5(
            real p_win,
            real current_points,
            vector proportion_bet,
            real rho,
            vector bet_position,
            real beta,
            real theta,
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

        real p_loss = 1 - p_win;
        vector[n] expected_utility =
            p_win * utility_after_win + p_loss ^ theta * utility_after_loss;

        vector[n] cost_of_bet_position =
            (bet_position - 1) / (n_bet_options - 1);
        vector[n] penalty_for_waiting = beta * cost_of_bet_position;

        return expected_utility - penalty_for_waiting;
    }

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
