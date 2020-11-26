#Need to adjust to give subject group data for Stan
library(rstan)
library(shinystan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

setwd("C:/Users/Lukas Cincikas/Documents/R/MPhil project files/MPhil analytical models")

trial_initial_points <- vector()
trial_final_points <- vector()
obj_left_number <- vector()
left_won <- vector()
Choice1 <- vector()
Choice2 <- vector()
Bet_chosen <- vector()
subject_wins <- vector()
trial_number <- vector()
subject_group <- vector()

file_set <- c("mock_data1.csv", "mock_data2.csv", "mock_data3.csv", "mock_data4.csv", "mock_data5.csv","mock_data6.csv","mock_data7.csv","mock_data8.csv",
              "mock_data9.csv","mock_data10.csv","mock_data11.csv","mock_data12.csv","mock_data13.csv","mock_data14.csv","mock_data15.csv","mock_data16.csv",
              "mock_data17.csv","mock_data18.csv","mock_data19.csv","mock_data20.csv") #this is the list of data files to be cycled through
subjects <- 20 #I guess just set this manually!
for (i in 1:subjects) { #creates linear arrays of all the needed data
  PreData <- read.csv(file_set[i])
  
  trial_initial_points <- c(trial_initial_points,PreData$trial.initial.points) #extracting data from .csv
  trial_final_points <- c(trial_final_points,PreData$trial.final.points)
  obj_left_number <- c(obj_left_number,PreData$n.left.colour.boxes)
  left_won <- c(left_won,PreData$left.won)
  Choice1 <- c(Choice1,PreData$left.colour.chosen)
  Choice2 <- c(Choice2,PreData$stake.index)
  Bet_chosen <- c(Bet_chosen,PreData$percentage.staked)
  subject_wins <- c(subject_wins,PreData$subject.won)
  
  trial_number[i] <- length(PreData$left.colour.chosen) #list of trials per subject
}
total_trials <- length(Choice1)

Bet_chosen <- Bet_chosen/100 #easier to use when calculating later on
#left_won <- ifelse(left_won == "yes", 1, 0) #binary is more simple   !!!!! was the source of a bug - real data has 'yes', mock uses binary already
#subject_wins <- ifelse(subject_wins == "yes", 1, 0)
#Choice1 <- ifelse(Choice1=="yes",1,0)
obj_left_number <- obj_left_number/10

subject_group <- c(1,2,1,2,1,2,1,2,1,2,3,4,3,4,3,4,3,4,3,4) # SET SUBJECT GROUPS MANUALLY HERE. Needs to be paired i.e. subject-placebo, then subject-drug.

#This makes a list of data for the actually stan fitting
stan_data <- list(total_data_length = total_trials,
                  subjects = subjects,
                  trials = trial_number,
                  n_left = obj_left_number, 
                  Choice1 = Choice1, 
                  Choice2 = Choice2, 
                  init_points = trial_initial_points,
                  subject_wins = subject_wins,
                  subject_group = subject_group)

stan_model1 <- "implementing hierarchy 2.stan"

fit <- stan(file = stan_model1,
            data = stan_data,
            warmup = 1000,
            iter = 6000,
            chains = 4,
            cores = 4,
            thin = 1)