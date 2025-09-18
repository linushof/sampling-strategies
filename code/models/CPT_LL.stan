data {
  int<lower=1> N;                       // Number of observations
  int<lower=0, upper=1> choice[N];      // Binary choices
  real<lower=0> o1_low[N]; 
  real<lower=0, upper=1> o1_sp_low[N];
  real<lower=0> o1_high[N];
  real<lower=0, upper=1> o1_sp_high[N];
  real<lower=0> o2_low[N];
  real<lower=0, upper=1> o2_sp_low[N];
  real<lower=0> o2_high[N];
  real<lower=0, upper=1> o2_sp_high[N];
}

parameters {
  real<lower=0, upper=1> alpha_pre;
  real<lower=0, upper=1> gamma_pre;
  real<lower=0, upper=1> delta_pre;
  real<lower=0, upper=1> phi_pre;
}

transformed parameters {
  real alpha = 2*alpha_pre;
  real gamma = 10*gamma_pre;
  real delta = 5*delta_pre;
  real phi = 5*phi_pre;
}


model {
  
  // Likelihood
  for (i in 1:N) {
    
    // value function
    real v_o1_low = pow(o1_low[i], alpha);
    real v_o1_high = pow(o1_high[i], alpha);
    
    real v_o2_low = pow(o2_low[i], alpha);
    real v_o2_high = pow(o2_high[i], alpha);
    
    // weighting function
    real pi_o1_high = (delta * pow(o1_sp_high[i], gamma)) / 
                       (delta * pow(o1_sp_high[i], gamma) + pow(o1_sp_low[i], gamma));
    real pi_o1_low = 1 - pi_o1_high;
    
    real pi_o2_high = (delta * pow(o2_sp_high[i], gamma)) / 
                       (delta * pow(o2_sp_high[i], gamma) + pow(o2_sp_low[i], gamma));
    real pi_o2_low = 1 - pi_o2_high;

    real V_o1 = pi_o1_low * v_o1_low + pi_o1_high * v_o1_high;
    real V_o2 = pi_o2_low * v_o2_low + pi_o2_high * v_o2_high;

    real V_o1_re = pow(V_o1, 1.0 / alpha);
    real V_o2_re = pow(V_o2, 1.0 / alpha);

    real binval = inv_logit(phi * (V_o1_re - V_o2_re));

    target += bernoulli_lpmf(choice[i] | binval);
  }
}

generated quantities {
  vector[N] log_lik;

  for (i in 1:N) {
    // value function
    real v_o1_low = pow(o1_low[i], alpha);
    real v_o1_high = pow(o1_high[i], alpha);

    real v_o2_low = pow(o2_low[i], alpha);
    real v_o2_high = pow(o2_high[i], alpha);

    // weighting function
    real pi_o1_high = (delta * pow(o1_sp_high[i], gamma)) / 
                       (delta * pow(o1_sp_high[i], gamma) + pow(o1_sp_low[i], gamma));
    real pi_o1_low = 1 - pi_o1_high;

    real pi_o2_high = (delta * pow(o2_sp_high[i], gamma)) / 
                       (delta * pow(o2_sp_high[i], gamma) + pow(o2_sp_low[i], gamma));
    real pi_o2_low = 1 - pi_o2_high;

    real V_o1 = pi_o1_low * v_o1_low + pi_o1_high * v_o1_high;
    real V_o2 = pi_o2_low * v_o2_low + pi_o2_high * v_o2_high;

    real V_o1_re = pow(V_o1, 1.0 / alpha);
    real V_o2_re = pow(V_o2, 1.0 / alpha);

    real binval = inv_logit(phi * (V_o1_re - V_o2_re));

    // pointwise log-likelihood for WAIC/LOO
    log_lik[i] = bernoulli_lpmf(choice[i] | binval);
  }
}