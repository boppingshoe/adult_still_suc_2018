
  data {
    int<lower=0> n_obs;
    int<lower=0> n_mis;
    int<lower=0> N;
    int<lower=0, upper=1> det[N];
    real temp[N];
    real dis[N];
    real<lower=0> vel_obs[n_obs];
    real ftt_obs[n_obs];
    vector<lower=0, upper=1>[N] trans;
    int<lower=1, upper=8> yr[N];
  }
  
  parameters {
    real b_0;
    real b_temp;
    real b_dis;
    real b_ftt;
    real b_trans;

    vector[8] a_yr;
    real<lower=0,upper=5> sigma_yr;
    real<lower=0> mu_v;
    real<lower=0> sigma_v;
    
    real<lower=0> vel_mis[n_mis];
  }
  
  transformed parameters {
    vector<lower=0, upper=1>[N] phi;
    real ftt_mis[n_mis];
    for (i in 1:n_obs)
      phi[i]= inv_logit(b_0+ b_temp*temp[i]+ b_dis*dis[i]+
        b_ftt*ftt_obs[i]+ b_trans*trans[i]+ a_yr[yr[i]]);
    for (j in (n_obs+1):N){
      ftt_mis[j-n_obs]= (225/vel_mis[j-n_obs]- 8.37)/ 7.7;
      phi[j]= inv_logit(b_0+ b_temp*temp[j]+ b_dis*dis[j]+
        b_ftt*ftt_mis[j-n_obs]+ b_trans*trans[j]+ a_yr[yr[j]]);
    }
  }
  
  model {
  // GLM
    b_0~ student_t(1, 0, 10);
    b_temp~ student_t(1, 0, 2.5);
    b_dis~ student_t(1, 0, 2.5);
    b_ftt~ student_t(1, 0, 2.5);
    b_trans~ student_t(1, 0, 2.5);
    for(j in 1:8)
      a_yr[j]~ normal(0, sigma_yr);
    sigma_yr~ student_t(1, 0, 2.25);
    
    det~ bernoulli(phi);
    
    // FTT
    mu_v~ student_t(1, 0, 10);
    sigma_v~ student_t(1, 0, 2.25);
    vel_obs~ normal(mu_v, sigma_v);
    vel_mis~ normal(mu_v, sigma_v);
  }
  
  generated quantities {
    real<lower=0, upper=1> phi_rep[N];
    int<lower=0, upper=1> det_rep[N];
    real r_obs[N];
    real r_rep[N];
    real t_obs;
    real t_rep;
    
    for (i in 1:n_obs)
      phi_rep[i]= inv_logit(b_0+ b_temp*temp[i]+ b_dis*dis[i]+
        b_ftt*ftt_obs[i]+ b_trans*trans[i]+ a_yr[yr[i]]);
    for (j in (n_obs+1):N)
      phi_rep[j]= inv_logit(b_0+ b_temp*temp[j]+ b_dis*dis[j]+
        b_ftt*ftt_mis[j-n_obs]+ b_trans*trans[j]+ a_yr[yr[j]]);
    // posterior predictive
    for (n in 1:N){
      det_rep[n]= bernoulli_rng(phi_rep[n]);
      r_obs[n]= det[n]- phi[n];
      r_rep[n]= det_rep[n]- phi_rep[n];
    }
    t_obs= mean(r_obs);
    t_rep= mean(r_rep);
  }


