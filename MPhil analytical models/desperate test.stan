//Rework with 1 1
functions {
  
}

data {
  int total_data_length; //total data length to initialize other important variables. Otherwise no use.
  int subjects;  //total number of subjects used
  int trials[subjects]; // number of trials in each subjects data
  real n_left[total_data_length]; //proportion of left colour boxes
  int Choice1[total_data_length]; //if left chosen =1, otherwise =0
  int Choice2[total_data_length]; //bet chosen (1-5)
  int init_points[total_data_length]; //points at the start of a trial
  int subject_wins[total_data_length]; //whether subjects won the trial (Irrelevant as of now?)
  int subject_group[subjects]; //allocates each subject to one of 4 groups
}

parameters {
//  real<lower=0> 1_SD;
//  real 1_subject[subjects/2];
//  real 1_group_t[4];
  real<lower=0> gamma_SD;
  real gamma_subject[subjects/2];
  real gamma_group_t[4];
}

transformed parameters {
//  real<lower=0> 1_group[4];
  real<lower=0> gamma_group[4];
  
  for (u in 1:4) {
//    1_group[u] = exp(1_group_t[u]);
    gamma_group[u] = exp(gamma_group_t[u]);
  }
}

model {
  int sum_trials = 0; //this will keep track of the points in the arrays in the main loop
  vector[5] EU;
  vector[5] Pbet;
  real Pside;
//  real 1;
  real gamma;
//  1_group_t ~ normal(0,1);
  gamma_group_t ~ normal(0,1);
//  1_SD ~ normal(0,1);
  gamma_SD ~ normal(0,1);
//  1_subject ~ normal(0,1_SD);
  gamma_subject ~ normal(0,gamma_SD);
  
  for (s in 1:subjects){   //this loop combines the relevant group and subject parameters for calc in trial loop
//  int person = (1+s)/2;
//  int g = subject_group[s];
//  1 = 1_group[g] + 1_subject[person]; //makes it look nicer
    if (subject_group[s] == 1) { //1=control placebo
 //     1 = 1_group[1] + 1_subject[(1+s)/2];
      gamma = gamma_group[1] + gamma_subject[(1+s)/2];
    } else if (subject_group[s] == 2) { //2=control drug
//      1 = 1_group[2] + 1_subject[(1+s)/2];
      gamma = gamma_group[2] + gamma_subject[(1+s)/2];
    } else if (subject_group[s] == 3) { //3= patient placebo
//      1 = 1_group[3] + 1_subject[(1+s)/2];
      gamma = gamma_group[3] + gamma_subject[(1+s)/2];
    } else {                            //4 = patient drug
//      1 = 1_group[4] + 1_subject[(1+s)/2];
      gamma = gamma_group[4] + gamma_subject[(1+s)/2];
    }
    
    for (i in 1:trials[s]) { // main model loop
//      Choice1[sum_trials+i] ~ bernoulli(n_left[sum_trials+i] ^ 1 / ((1 - n_left[sum_trials+i]) ^ 1 + n_left[sum_trials+i] ^ 1)); //basically softmax, but for binomial
      
      if (Choice1[sum_trials+i] == 1){ //Pside is adjusted to be the side that was actually chosen
        Pside = n_left[sum_trials+i] ^ 1 / ((1 - n_left[sum_trials+i]) ^ 1 + n_left[sum_trials+i] ^ 1);
      } else {
        Pside = (1 - n_left[sum_trials+i]) ^ 1 / ((1 - n_left[sum_trials+i]) ^ 1 + n_left[sum_trials+i] ^ 1);
      } //below: calculates expected utility for each bet choice (uses a cumulative points model for calculating)
      EU[1] = Pside * (init_points[sum_trials+i] + init_points[sum_trials+i]*0.05) + (1 - Pside) * (init_points[sum_trials+i] - init_points[sum_trials+i]*0.05);
              
      EU[2] = Pside * (init_points[sum_trials+i] + init_points[sum_trials+i]*0.25) + (1 - Pside) * (init_points[sum_trials+i] - init_points[sum_trials+i]*0.25);
              
      EU[3] = Pside * (init_points[sum_trials+i] + init_points[sum_trials+i]*0.50) + (1 - Pside) * (init_points[sum_trials+i] - init_points[sum_trials+i]*0.50);
              
      EU[4] = Pside * (init_points[sum_trials+i] + init_points[sum_trials+i]*0.75) + (1 - Pside) * (init_points[sum_trials+i] - init_points[sum_trials+i]*0.75);
              
      EU[5] = Pside * (init_points[sum_trials+i] + init_points[sum_trials+i]*0.95) + (1 - Pside) * (init_points[sum_trials+i] - init_points[sum_trials+i]*0.95);
      
      Pbet = softmax(EU*gamma);
      Choice2[sum_trials+i] ~ categorical_logit(Pbet); 
    }
    sum_trials = sum_trials + trials[s]; //keep track of array point
  }
}

generated quantities {
  
}