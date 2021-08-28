#Try to follow Rudolf's footsteps. Use 5 parameters of the winning model from Romeu, later add additional alterations.
setwd("C:/Users/Lukas Cincikas/Documents/R/MPhil project files/MPhil generative models")

#-----------------------------------------------------------------------------------------------------------------------------------------
# Constants
N_BLOCKS <- 4
STARTING_POINTS <- 100
BANKRUPTCY <- 1
MAX_BLOCK_TRIALS <- 9
NUMBER_OF_BOXES <- 10
POSSIBLE_BETS <- c(0.05, 0.25, 0.5, 0.75, 0.95)
N_BET_OPTIONS <- 5
#-----------------------------------------------------------------------------------------------------------------------------------------
# Helper functions



#-----------------------------------------------------------------------------------------------------------------------------------------
# Cognitive model functions

pChooseRed <- function(red_proportion, alpha, colour_bias)
{
  colour <- colour_bias
  r <- red_proportion
  u <- (1 - r)
  p_red <- colour * r ^ alpha / (colour * r ^ alpha + (1 - colour) * u ^ alpha)
  return(p_red)
}

NumericalBet <- function(current_points, proportion_bet, round = TRUE)
{
  stake <- current_points * proportion_bet
  if (round) {
    stake <- round(stake)
  }
  return(stake)
}

UtilityFunction <- function(rho, x)
{
  utility <- log(1 + rho * x)
  return(utility)
}

ExpectedUtility <- function(current_points, bet_space, p_win, rho)
{
  points_bet_options <- NumericalBet(current_points, bet_space, round = FALSE)
  win_points <- current_points + points_bet_options
  lose_points <- current_points - points_bet_options
  win_utility <- utilityFunction(rho, win_points)
  lose_utility <- utilityFunction(rho, lose_points)
  p_lose <- 1 - p_win
  EU <- p_win * win_utility + p_lose * lose_utility
  return(EU)
}

FinalUtility <- function(bet_position, beta, EU)
{
  bet_position_cost <- (bet_position - 1) / (n_bet_options - 1)
  impulsivity <- beta * bet_position_cost
  z <- EU - impulsivity
  return(z)
}
#-----------------------------------------------------------------------------------------------------------------------------------------
# Main function

tot_trial <- NA
current_points <- NA
block_trial <- NA
choice1 <- NA
red_proportion <- NA
red_wins <- NA
chosen_proportion <- NA

Simulation <- function()
{
  tot_trial <- 1 #This will keep track of things for saving as arrays
  for(block in 1:N_BLOCKS)
  {
    
    current_points[tot_trial] <- STARTING_POINTS
    block_trial <- 1
    
    while(current_points[tot_trial] > BANKRUPTCY && block_trial <= MAX_BLOCK_TRIALS)
    {
      #Determines number of red squares in a trial
      red_proportion[tot_trial] <- floor(runif(1,min=1,max=NUMBER_OF_BOXES)) / NUMBER_OF_BOXES 
      
      #Determines probability of subject choosing red
      p_red <- pChooseRed(red_proportion, alpha, colour_bias)
      
      #Determines subject's colour choice, 1=red, 0=blue
      choice1[tot_trial] <- rbinom(1,size=1,prob=p_red)
      
      #Determines the actual winning colour, 1=red, 0=blue
      red_wins[tot_trial] <- rbinom(1,size=1,prob=red_proportion)
      
      #Records whether subject won his colour choice or not
      if(choice1[tot_trial] == red_wins[tot_trial])
      {
        subject_wins[tot_trial] <- 1
      } else
      {
        subject_wins[tot_trial] <- 0
      }
      
      #Sets the proportion of the chosen colour
      if(choice1[tot_trial] == 1)
      {
        chosen_proportion <- red_proportion[tot_trial]
      } else
      {
        chosen_proportion <- 1 - red_proportion[tot_trial]
      }
      
      bet_position <- if (ascending) 1:N_BET_OPTIONS else N_BET_OPTIONS:1 #NEED TO ESTABLISH ASCENDING/DESCENDING STUFF!!!!!
      
      #Evaluates the utilities of the possible options
      EU <- ExpectedUtility(current_points[tot_trial], POSSIBLE_BETS, chosen_proportion, rho)
      z <- FinalUtility(bet_position, beta, EU)
    }
  }
}

#-----------------------------------------------------------------------------------------------------------------------------------------
# Setting up data tables


