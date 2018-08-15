
  data {
    int<lower=0> n_obs;
    int<lower=0> n_mis;
    int<lower=0> N;
      int<lower=0, upper=1> stray[N];
    int<lower=0, upper=1> det[N];
    vector<lower=0, upper=1>[N] summer;
    real temp[N];
    real temp2[N];
    real<lower=0> vel_obs[n_obs];
    real ftt_obs[n_obs];
    real dis[N];
    vector<lower=0, upper=1>[N] trans;
    int<lower=1, upper=15> yr[N];
  }
  
  parameters{
    real b_0;
    real b_run;
    real b_temp;
    real b_temp2;
    real b_ftt;
    real b_dis;
    real b_trans;
    
    vector[15] a_yr;
    real<lower=0,upper=5> sigma_yr;
    real<lower=0> mu_v[2];
    real<lower=0> sigma_v[2];
    
    real<lower=0> vel_mis[n_mis];
  }
  
  transformed parameters{
    vector<lower=0, upper=1>[N] phi;
    real ftt_mis[n_mis];
    for (i in 1:n_obs)
      phi[i]= inv_logit(b_0+ b_run*summer[i]+ b_temp*temp[i]+
        b_temp2*temp2[i]+ b_ftt*ftt_obs[i]+ b_dis*dis[i]+
        b_trans*trans[i]+ a_yr[yr[i]]);
    for (j in (n_obs+1):N) {
      ftt_mis[j-n_obs]= 225/ vel_mis[j-n_obs];
      phi[j]= inv_logit(b_0+ b_run*summer[j]+ b_temp*temp[j]+
        b_temp2*temp2[j]+ b_ftt*ftt_mis[j-n_obs]+ b_dis*dis[j]+
        b_trans*trans[j]+ a_yr[yr[j]]);
    }
  }
  
  model{
  // GLM
    b_0~ student_t(1, 0, 10);
    b_run~ student_t(1, 0, 2.5);
    b_temp~ student_t(1, 0, 2.5);
    b_temp2~ student_t(1, 0, 2.5);
    b_ftt~ student_t(1, 0, 2.5);
    b_dis~ student_t(1, 0, 2.5);
    b_trans~ student_t(1, 0, 2.5);
    for(j in 1:15)
      a_yr[j]~ normal(0, sigma_yr);
    sigma_yr~ student_t(1, 0, 2.25);
    
    det~ bernoulli(phi);
    
    // FTT
    mu_v~ student_t(1, 0, 10);
    sigma_v~ student_t(1, 0, 2.25);
    for (i in 1:n_obs){
      if (stray[i] == 0)
        target += normal_lpdf(vel_obs[i] | mu_v[1], sigma_v[1]);
      else
        target += normal_lpdf(vel_obs[i] | mu_v[2], sigma_v[2]);
    }
    for (j in (n_obs+1):N){
      if (stray[j] == 0)
        target += normal_lpdf(vel_mis[j-n_obs] | mu_v[1], sigma_v[1]);
      else
        target += normal_lpdf(vel_mis[j-n_obs] | mu_v[2], sigma_v[2]);
    }
  }
  
  generated quantities{
    real<lower=0, upper=1> phi_rep[N];
    int<lower=0, upper=1> det_rep[N];
    real t_rep[N];
    real tm_rep;
    real ir_rep[N];
    real tr_rep[N];
    real ir_obs[N];
    real tr_obs[N];
    real trm_rep;
    real trm_obs;
    real sp_rep[N];
    real su_rep[N];
    real sp_obs[N];
    real su_obs[N];
    real spm_rep;
    real spm_obs;
  
    for (i in 1:n_obs)
      phi_rep[i]= inv_logit(b_0+ b_run*summer[i]+ b_temp*temp[i]+
          b_temp2*temp2[i]+ b_ftt*ftt_obs[i]+ b_dis*dis[i]+
          b_trans*trans[i]+ a_yr[yr[i]]);
    for (j in (n_obs+1):N)
      phi_rep[j]= inv_logit(b_0+ b_run*summer[j]+ b_temp*temp[j]+
          b_temp2*temp2[j]+ b_ftt*ftt_mis[j-n_obs]+ b_dis*dis[j]+
          b_trans*trans[j]+ a_yr[yr[j]]);
    // posterior predictive
    for (n in 1:N){
      det_rep[n]= bernoulli_rng(phi_rep[n]);
      t_rep[n]= det_rep[n]* temp[n];
        // mig history
      ir_rep[n]= (trans[n]-1)* (-1)* (det_rep[n]- phi_rep[n]);
      tr_rep[n]= trans[n]* (det_rep[n]- phi_rep[n]);
      ir_obs[n]= (trans[n]-1)* (-1)* (det[n]- phi[n]);
      tr_obs[n]= trans[n]* (det[n]- phi[n]);
        // run type
      sp_rep[n]= (summer[n]-1)* (-1)* (det_rep[n]- phi_rep[n]);
      su_rep[n]= summer[n]* (det_rep[n]- phi_rep[n]);
      sp_obs[n]= (summer[n]-1)* (-1)* (det[n]- phi[n]);
      su_obs[n]= summer[n]* (det[n]- phi[n]);
    }
    tm_rep= mean(t_rep);
    trm_rep= sum(ir_rep)/(sum(trans-1)*(-1))- sum(tr_rep)/sum(trans);
    trm_obs= sum(ir_obs)/(sum(trans-1)*(-1))- sum(tr_obs)/sum(trans);
    spm_rep= sum(sp_rep)/(sum(summer-1)*(-1))- sum(su_rep)/sum(summer);
    spm_obs= sum(sp_obs)/(sum(summer-1)*(-1))- sum(su_obs)/sum(summer);
  }
  

