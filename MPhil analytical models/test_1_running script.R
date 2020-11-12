#R script for preprocessing and running test stan model
library(rstan)
library(shinystan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

setwd("C:/Users/Lukas Cincikas/Documents/R/MPhil analytical models")

PreData <- read.csv("mock_data.csv")

trial_initial_points <- PreData$trial.initial.points #extracting data from .csv
trial_final_points <- PreData$trial.final.points
obj_left_number <- PreData$n.left.colour.boxes
left_won <- PreData$left.won
Choice1 <- PreData$left.colour.chosen
Choice2 <- PreData$stake.index
Bet_chosen <- PreData$percentage.staked
subject_wins <- PreData$subject.won

trial_number <- length(Choice1)

Bet_chosen <- Bet_chosen/100 #easier to use when calculating later on
#left_won <- ifelse(left_won == "yes", 1, 0) #binary is more simple   !!!!! was the source of a bug - real data has 'yes', mock uses binary already
#subject_wins <- ifelse(subject_wins == "yes", 1, 0)
#Choice1 <- ifelse(Choice1=="yes",1,0)
obj_left_number <- obj_left_number/10

#This makes a list of data for ths actually stan fitting
stan_data <- list(trials = trial_number, n_left = obj_left_number, Choice1 = Choice1, Choice2 = Choice2, init_points = trial_initial_points, subject_wins = subject_wins)

stan_model1 <- "test_2.stan"

fit <- stan(file = stan_model1,
            data = stan_data,
            warmup = 500,
            iter = 2000,
            chains = 4,
            cores = 4,
            thin = 1)