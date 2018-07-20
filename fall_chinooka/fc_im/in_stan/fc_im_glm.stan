
  data {
    int<lower=0> n_obs;
    int<lower=0> n_mis;
    int<lower=0> N;
    int<lower=0, upper=1> det[N];
    real juld[N];
    real juld2[N];
    real temp[N];
    real<lower=0> vel_obs[n_obs];
    real ftt_obs[n_obs];
    vector<lower=0, upper=1>[N] trans;
  }
  
  parameters{
    real b_0;
    real b_juld;
    real b_juld2;
    real b_temp;
    real b_ftt;
    real b_txf;
    real b_trans;
    
    real<lower=0> mu_v;
    real<lower=0> sigma_v;
    
    real<lower=0> vel_mis[n_mis];
  }
  
  transformed parameters{
    vector<lower=0, upper=1>[N] phi;
    real ftt_mis[n_mis];
    for (i in 1:n_obs)
      phi[i]= inv_logit(b_0+ b_juld*juld[i]+ b_juld2*juld2[i]+
        b_temp*temp[i]+ b_ftt*ftt_obs[i]+ b_txf*temp[i]*ftt_obs[i]+
        b_trans*trans[i]);
    for (j in (n_obs+1):N) {
      ftt_mis[j-n_obs]= (225/vel_mis[j-n_obs]- 7.88)/ 4.29;
      phi[j]= inv_logit(b_0+ b_juld*juld[j]+ b_juld2*juld2[j]+
        b_temp*temp[j]+ b_ftt*ftt_mis[j-n_obs]+ b_txf*temp[j]*ftt_mis[j-n_obs]+
        b_trans*trans[j]);
    }
  }
  
  model{
    // GLM
    b_0~ student_t(1, 0, 10);
    b_juld~ student_t(1, 0, 2.5);
    b_juld2~ student_t(1, 0, 2.5);
    b_temp~ student_t(1, 0, 2.5);
    b_ftt~ student_t(1, 0, 2.5);
    b_txf~ student_t(1, 0, 2.5);
    b_trans~ student_t(1, 0, 2.5);

    det~ bernoulli(phi);
    
    // FTT
    mu_v~ student_t(1, 0, 10);
    sigma_v~ student_t(1, 0, 2.25);
    vel_obs~ normal(mu_v, sigma_v);
    vel_mis~ normal(mu_v, sigma_v);
  }
  
  generated quantities{
    real<lower=0, upper=1> phi_rep[N];
    int<lower=0, upper=1> det_rep[N];
    real r_obs[N];
    real r_rep[N];
    real t_obs;
    real t_rep;
    
    for (i in 1:n_obs)
      phi_rep[i]= inv_logit(b_0+ b_juld*juld[i]+ b_juld2*juld2[i]+
        b_temp*temp[i]+ b_ftt*ftt_obs[i]+ b_txf*temp[i]*ftt_obs[i]+
        b_trans*trans[i]);
    for (j in (n_obs+1):N)
      phi_rep[j]= inv_logit(b_0+ b_juld*juld[j]+ b_juld2*juld2[j]+
        b_temp*temp[j]+ b_ftt*ftt_mis[j-n_obs]+ b_txf*temp[j]*ftt_mis[j-n_obs]+
        b_trans*trans[j]);
    // posterior predictive
    for (n in 1:N){
      det_rep[n]= bernoulli_rng(phi_rep[n]);
      r_obs[n]= det[n]- phi[n];
      r_rep[n]= det_rep[n]- phi_rep[n];
    }
    t_obs= mean(r_obs);
    t_rep= mean(r_rep);
  }

