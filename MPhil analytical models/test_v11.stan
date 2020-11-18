//Updating model to include multiple subjects
data {
  int total_data_length; //total data length to initialize other important variables. Otherwise no use.
  int subjects;  //total number of subjects used
  int trials[subjects]; // number of trials in each subjects data
  real n_left[total_data_length]; //proportion of left colour boxes
  int Choice1[total_data_length]; //if left chosen =1, otherwise =0
  int Choice2[total_data_length]; //bet chosen (1-5)
  int init_points[total_data_length]; //points at the start of a trial
  int subject_wins[total_data_length]; //whether subjects won the trial
}

parameters {
  real<lower=0> alpha[subjects];
  real<lower=0> gamma[subjects];
}

model {
  int sum_trials = 0; //this will keep track of the points in the arrays in the main loop
  vector[5] EU;
  vector[5] Pbet;
  real Pside;
  
  for (s in 1:subjects){
    alpha[s] ~ gamma(2,2);
    gamma[s] ~ gamma(2,2);
    
    for (i in 1:trials[s]) {
      Choice1[sum_trials+i] ~ bernoulli(n_left[sum_trials+i] ^ alpha[s] / ((1 - n_left[sum_trials+i]) ^ alpha[s] + n_left[sum_trials+i] ^ alpha[s])); //basically softmax, but for binomial
      
      if (Choice1[sum_trials+i] == 1){ //Pside is adjusted to be the side that was actually chosen
        Pside = n_left[sum_trials+i] ^ alpha[s] / ((1 - n_left[sum_trials+i]) ^ alpha[s] + n_left[sum_trials+i] ^ alpha[s]);
      } else {
        Pside = (1 - n_left[sum_trials+i]) ^ alpha[s] / ((1 - n_left[sum_trials+i]) ^ alpha[s] + n_left[sum_trials+i] ^ alpha[s]);
      } //below: calculates expected utility for each bet choice
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

