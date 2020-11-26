//Updating model to include multiple subjects
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
  real alpha_mu[4];
  real<lower=0> alpha_sigma[4];
  real alpha_t[subjects];
  real gamma_mu[4];
  real<lower=0> gamma_sigma[4];
  real gamma_t[subjects];
}

transformed parameters {
  real alpha[subjects];
  real gamma[subjects];
  
  for(t in 1:subjects) {
    alpha[t] = exp(alpha_t[t]);
    gamma[t] = exp(gamma_t[t]);
  }
}

model {
  int sum_trials = 0; //this will keep track of the points in the arrays in the main loop
  vector[5] EU;
  vector[5] Pbet;
  real Pside;
  alpha_mu ~ normal(0,1);
  alpha_sigma ~ normal(0,1);
  gamma_mu ~ normal(0,1);
  gamma_sigma ~ normal(0,1);
  
  for (s in 1:subjects){
    if (subject_group[s] == 1) { //1=control placebo
      alpha_t[s] ~ normal(alpha_mu[1],alpha_sigma[1]); //this block segment selects the correct group parameters to sample from
      gamma_t[s] ~ normal(gamma_mu[1],gamma_sigma[1]);
    } else if (subject_group[s] == 2) { //2=control drug
      alpha_t[s] ~ normal(alpha_mu[2],alpha_sigma[2]);
      gamma_t[s] ~ normal(gamma_mu[2],gamma_sigma[2]);
    } else if (subject_group[s] == 3) { //3= patient placebo
      alpha_t[s] ~ normal(alpha_mu[3],alpha_sigma[3]);
      gamma_t[s] ~ normal(gamma_mu[3],gamma_sigma[3]);
    } else {                            //4 = patient drug
      alpha_t[s] ~ normal(alpha_mu[4],alpha_sigma[4]);
      gamma_t[s] ~ normal(gamma_mu[4],gamma_sigma[4]);
    }
    
    for (i in 1:trials[s]) { // main model loop
      Choice1[sum_trials+i] ~ bernoulli(n_left[sum_trials+i] ^ alpha[s] / ((1 - n_left[sum_trials+i]) ^ alpha[s] + n_left[sum_trials+i] ^ alpha[s])); //basically softmax, but for binomial
      
      if (Choice1[sum_trials+i] == 1){ //Pside is adjusted to be the side that was actually chosen
        Pside = n_left[sum_trials+i] ^ alpha[s] / ((1 - n_left[sum_trials+i]) ^ alpha[s] + n_left[sum_trials+i] ^ alpha[s]);
      } else {
        Pside = (1 - n_left[sum_trials+i]) ^ alpha[s] / ((1 - n_left[sum_trials+i]) ^ alpha[s] + n_left[sum_trials+i] ^ alpha[s]);
      } //below: calculates expected utility for each bet choice (uses a cumulative points model for calculating)
      EU[1] = Pside * (init_points[sum_trials+i] + init_points[sum_trials+i]*0.05) + (1 - Pside) * (init_points[sum_trials+i] - init_points[sum_trials+i]*0.05);
              
      EU[2] = Pside * (init_points[sum_trials+i] + init_points[sum_trials+i]*0.25) + (1 - Pside) * (init_points[sum_trials+i] - init_points[sum_trials+i]*0.25);
              
      EU[3] = Pside * (init_points[sum_trials+i] + init_points[sum_trials+i]*0.50) + (1 - Pside) * (init_points[sum_trials+i] - init_points[sum_trials+i]*0.50);
              
      EU[4] = Pside * (init_points[sum_trials+i] + init_points[sum_trials+i]*0.75) + (1 - Pside) * (init_points[sum_trials+i] - init_points[sum_trials+i]*0.75);
              
      EU[5] = Pside * (init_points[sum_trials+i] + init_points[sum_trials+i]*0.95) + (1 - Pside) * (init_points[sum_trials+i] - init_points[sum_trials+i]*0.95);
      
      Pbet = softmax(EU * gamma[s]);
      Choice2[sum_trials+i] ~ categorical(Pbet); 
    }
    sum_trials = sum_trials + trials[s]; //keep track of array point
  }
}

generated quantities {
  /*to derive the drug effect properly, I would need to compare individual subject parameters in drug and placebo conditions.
  To do that, I need to make loops to match the correct array points in the parameters, and subtract them.
  But how do I group together all the new distributions foreach individual parameter into a single effect parameter? */
  
  
}

