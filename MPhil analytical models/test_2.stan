//First test model for stan using ALPHA, GAMMA and a CUMULATIVE utility
data {
  int trials;
  real n_left[trials];
  int Choice1[trials];
  int Choice2[trials];
  int init_points[trials];
  int subject_wins[trials];
}

transformed data {
  
}

parameters {
  real<lower=0> alpha;
  real<lower=0> gamma;
}

transformed parameters {

}

model {
  vector[5] EU;
  vector[5] Pbet;
  real Pside;
  alpha ~ gamma(2,2);
  gamma ~ gamma(2,2);
  
  for(i in 1:trials) {
    Choice1[i] ~ bernoulli(n_left[i]^alpha / ((1-n_left[i])^alpha + n_left[i]^alpha));
    if(Choice1[i]==1){
      Pside = n_left[i]^alpha / ((1-n_left[i])^alpha + n_left[i]^alpha);
    } else {
      Pside = (1-n_left[i])^alpha / ((1-n_left[i])^alpha + n_left[i]^alpha);
    }
    EU[1] = Pside*(init_points[i] + init_points[i]*0.05) + (1 - Pside) * (init_points[i] - init_points[i]*0.05);
    EU[2] = Pside*(init_points[i] + init_points[i]*0.25) + (1 - Pside) * (init_points[i] - init_points[i]*0.25);
    EU[3] = Pside*(init_points[i] + init_points[i]*0.50) + (1 - Pside) * (init_points[i] - init_points[i]*0.50);
    EU[4] = Pside*(init_points[i] + init_points[i]*0.75) + (1 - Pside) * (init_points[i] - init_points[i]*0.75);
    EU[5] = Pside*(init_points[i] + init_points[i]*0.95) + (1 - Pside) * (init_points[i] - init_points[i]*0.95);
    
    Pbet = softmax(EU*gamma);
    Choice2[i] ~ categorical(Pbet); 
  }
}

