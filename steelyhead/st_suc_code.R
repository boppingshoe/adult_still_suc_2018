
# load/format data
rm(list=ls())
wd<- 'G:/STAFF/Bobby/css/adult_still_suc_2018/'
source(file=paste0(wd, "steelyhead/st_suc_util.R")) # where the functions are
sts<- load_dat_st(wd)
#

## low snake survivals vs. temperature
# with GLM ----
cat('
model{
  
  for (i in 1:n_ind){
    # GLM
    det[i]~ dbern(phi[i])
    logit(phi[i])<- b_0+ b_juld*juld[i]+ b_juld2*juld2[i]+
      b_temp*temp[i]+ b_ftt*ftt[i]+ b_txf*temp[i]*ftt[i]+
      b_trans*trans[i]+ a_yr[yr[i]]
    r_obs[i]<- det[i]- phi[i]
    det_rep[i]~ dbern(phi[i])
    logit(phi_rep[i])<- b_0+ b_juld*juld[i]+ b_juld2*juld2[i]+
      b_temp*temp[i]+ b_ftt*ftt[i]+ b_txf*temp[i]*ftt[i]+
      b_trans*trans[i]+ a_yr[yr[i]]
    r_rep[i]<- det_rep[i]- phi_rep[i]

    # FTT
    ftt[i]<- (225/vel[i]- 23.04)/ 35.04 # scaled ftt 
    vel[i]~ dnorm(mu_v, tau_v)T(0,)
  }

  # priors
  # GLM
  b_0~ dt(0, 0.1, 1)
  b_juld~ dt(0, 0.4, 1)
  b_juld2~ dt(0, 0.4, 1)
  b_temp~ dt(0, 0.4, 1)
  b_trans~ dt(0, 0.4, 1)
  b_ftt~ dt(0, 0.4, 1)
  for (j in 1:14){
    a_yr[j]~ dnorm(0, tau_yr)
  }
  sigma_yr~ dt(0, 0.444, 1)T(0,5)
  tau_yr<- pow(sigma_yr, -2)
  b_txf~ dt(0, 0.4, 1)

  # FTT
  mu_v~ dt(0, 0.1, 1)T(0,)
  sigma_v~ dt(0, 0.444, 1)T(0,)
  tau_v<- pow(sigma_v, -2)

  # posterior predictive
  t_obs<- sum(r_obs)/n_ind
  t_rep<- sum(r_rep)/n_ind

}', fill=TRUE, file = paste0(wd,'steelyhead/st_im/st_im_glm.txt'))
#####
require(jagsUI)

im_data<- prep_dat(sts)
str(im_data)
#
# run JAGS ----
parameters <- c('b_0','b_juld','b_juld2','b_temp','b_ftt','b_txf','b_trans','a_yr','sigma_yr','mu_v','sigma_v','t_obs','t_rep')
inits<- function() {list(b_0=runif(1,-1,1), b_juld=runif(1,-1,1), b_juld2=runif(1,-1,1), b_temp=runif(1,-1,1), b_ftt=runif(1,-1,1), b_txf=runif(1,-1,1), b_trans=runif(1,-1,1), sigma_yr=runif(1,0,2), mu_v=runif(1,1,8), sigma_v=runif(1,0,2) )}

# nc<- 4   ;   ni<- 100   ;   nb<- 0   ;   nt<- 1 # test run
nc<- 4   ;   ni<- 1000   ;   nb<- 500   ;   nt<- 1 # test run2
# nc<- 4   ;   ni<- 40000   ;   nb<-20000   ;   nt<- 2
# nc<- 4   ;   ni<- 80000   ;   nb<-50000   ;   nt<- 3

imglm_out<- jags(im_data, inits, parameters, "steelyhead/st_im/st_im_glm.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)
# im_out<- autojags(im_data, inits, parameters, "steelyhead/st_im/st_im_glm.txt", n.thin=nt, n.chains=nc, n.burnin=2000, iter.increment=5000, max.iter=42000, parallel=TRUE)
imglm_out<- update(imglm_out, parameters.to.save=parameters, n.thin=5, n.chains=4, n.iter=50000, parallel=TRUE)
print(imglm_out)

im_sims_st<- imglm_out$sims.list
im_rhat_st<- cbind(unlist(imglm_out$Rhat)[!is.na(unlist(imglm_out$Rhat))], unlist(imglm_out$n.eff)[unlist(imglm_out$n.eff)>1])
# JAGS results saved as as a big table
write.table(im_sims_st, file='steelyhead/st_im/im_glm_sims_st.txt')
write.table(im_rhat_st, file='steelyhead/st_im//im_glm_rhat_st.txt')
save(imglm_out, file='steelyhead/st_im/im_glm_out.R')
# load(file='steelyhead/st_im/im_glm_out.R')
#####

# convert output ----
im_sims_st<- read.table('steelyhead/st_im/im_glm_sims_st.txt')
im_rhat_st<- read.table('steelyhead/st_im/im_glm_rhat_st.txt')

b_0_st=im_sims_st$b_0; b_juld_st=im_sims_st$b_juld; b_juld2_st=im_sims_st$b_juld2; b_temp_st=im_sims_st$b_temp;  b_ftt_st=im_sims_st$b_ftt; b_txf_st=im_sims_st$b_txf;  b_trans_st=im_sims_st$b_trans; sigma_yr_st=im_sims_st$sigma_yr; mu_v_st=im_sims_st$mu_v; sigma_v_st=im_sims_st$sigma_v; devi_st=im_sims_st$deviance
t_obs_st=im_sims_st$t_obs; t_rep_st=im_sims_st$t_rep
# b_temp2_st=im_sims_st$b_temp2;b_t2xf_st=im_sims_st$b_t2xf;
# ayrs_st<- im_sims_st$a_yr

a_yr_st<- NA
for(i in 1:14){
  a_yr_st[i]= im_sims_st[7+i]
}
ayrs_st<- data.frame(cbind(unlist(a_yr_st[1]), unlist(a_yr_st[2]), unlist(a_yr_st[3]), unlist(a_yr_st[4]), unlist(a_yr_st[5]), unlist(a_yr_st[6]), unlist(a_yr_st[7]), unlist(a_yr_st[8]), unlist(a_yr_st[9]), unlist(a_yr_st[10]), unlist(a_yr_st[11]), unlist(a_yr_st[12]), unlist(a_yr_st[13]), unlist(a_yr_st[14]) ))

outtab_st<- cbind(b_0_st, b_juld_st, b_juld2_st, b_temp_st, b_ftt_st, b_txf_st, b_trans_st, ayrs_st[,1:14], sigma_yr_st, mu_v_st, sigma_v_st, devi_st)
 # b_temp2_st, b_t2xf_st,

# output table ----
im_median_st<- cbind(apply(outtab_st, 2, median))
im_se_st<- cbind(apply(outtab_st, 2, sd))
im_cri_st<- apply(outtab_st, 2, function(x) quantile(x, c(0.025,0.975)))

summ_st<- data.frame(cbind(round(im_median_st,3), round(im_se_st,3), paste0('(', round(im_cri_st[1,], 3),', ', round(im_cri_st[2,], 3),')'), round(im_rhat_st[-c(10,11),], 3)))
row.names(summ_st)<- c('(Intercept)', 'Arrival Date', 'Arrival^2^', 'Temperature', 'Travel Time', 'TempxFtt', 'Transported','Year 2003','Year 2004','Year 2005','Year 2006','Year 2007','Year 2008','Year 2009','Year 2010','Year 2011','Year 2012','Year 2013','Year 2014','Year 2015','Year 2016', '$\\sigma_{yr}$', '$\\mu_{vel}$', '$\\sigma_{vel}$', 'Deviance')
colnames(summ_st)<- c('Median','SD','95% CRI','$\\hat{R}$','Eff size')
summ_st
# 'Temp^2^', 'Temp^2^xFtt', 

# traceplot ----
windows(8,9)
par(mfrow=c(4,2))
names(outtab_st)<- c('b0 (Intercept)', 'b Arrival', 'b Arr^2', 'b Temp', 'b Ftt', 'b TempxFtt', 'b Trans', 'Year2003','Year2004','Year2005','Year2006','Year2007','Year2008','Year2009','Year2010','Year2011','Year2012','Year2013','Year2014','Year2015','Year2016', 'SigmaYr', 'MuVel', 'SigmaVel', 'Deviance')
# 'b Temp^2', 'b Temp^2xFtt', 

# ncol(outtab_st)
pn1<- 1:4; pn2<- 5:8; pn3<- 9:12; pn4<- 13:16; pn5<- 17:20; pn6<- 21:24; pn7<- 25
for(i in pn7){
  plot_pds(outtab_st[,i], lab=names(outtab_st)[i], colr='grey70')
}

# simulated ftt vs. observed ftt ----
vel_pre<- rnorm(nrow(sts), 19.31, 12.671)
vel_pre<- vel_pre[vel_pre> 0]
ftt_pre<- 225/vel_pre

hist(sts$ftt, breaks=100, freq= F, col=rgb(0,0,0,0.8),
  main='Steelhead', xlab='Days')
hist(ftt_pre[ftt_pre<800], breaks=80, freq= F, col=rgb(1,1,1,.5), add= T)
legend(500, 0.04, c('Observed','Predicted'), pch=c(15,0), col=c(1,1), bty='n')
#####

# plotting survival relationships ----
pn<- nrow(outtab_st)
nsim<- 1000
r<- sample(1:pn, nsim)
windows(10,4)
par(mfrow=c(1,2))
par(mar=c(5,4,2,2)+0.1) # original 5,4,4,2
# with temperature ----
# quantile(sts$ihr_temp, c(0.025,0.975), na.rm=TRUE) # 17.61250 21.61574
plot(0,0, xlim=c(13,23), ylim=c(0,1), ty='n',
  xlab='Temperature (Celsius)', ylab='Conversion')
invisible(apply(outtab_st[r, c('b_0_st','b_juld_st','b_juld2_st','b_temp_st','b_trans_st')], 1, function(x) surv_temp(x, lcol=c('grey90','grey80')) ))
invisible(apply(rbind(colMeans(outtab_st[r, c('b_0_st','b_juld_st','b_juld2_st','b_temp_st','b_trans_st')])), 1, function(x) surv_temp(x, lcol=c('grey70','grey60'), lw=3)))
invisible(apply(outtab_st[r, c('b_0_st','b_juld_st','b_juld2_st','b_temp_st','b_trans_st')], 1, function(x) surv_temp(x, alpha=17, omega=22)))
invisible(apply(rbind(colMeans(outtab_st[r, c('b_0_st','b_juld_st','b_juld2_st','b_temp_st','b_trans_st')])), 1, function(x) surv_temp(x, alpha=17, omega=22, lcol=c('navy','deeppink'), lw=3)))
legend(18, 0.4, c(' ',' '), col=c('cyan','lightpink'), lwd=10, bty='n')
legend(18, 0.4, c('In-River','Transported'), col=c('navy','deeppink'), lwd=3, bty='n')

# sts$tempbin<- cut(sts$ihr_temp, breaks=c(10,15,17,19,21,23))
# points(seq(15,23, by=2), tapply(sts$gra_det, sts$tempbin, mean), pch=20, cex=2)

# with ftt ----
# quantile(sts$ftt, c(0.025,0.975), na.rm=TRUE) # 6.000 108.425
plot(0,0, xlim=c(3,25), ylim=c(0,1), ty='n',
  xlab='Travel Time (Days)', ylab='Conversion')
invisible(apply(outtab_st[r, c('b_0_st','b_temp_st','b_ftt_st','b_txf_st')], 1, function(x) surv_ftt(x, temp=19, lcol=c('grey90','grey80')) ))
invisible(apply(rbind(colMeans(outtab_st[r, c('b_0_st','b_temp_st','b_ftt_st','b_txf_st')])), 1, function(x) surv_ftt(x, temp=19, lcol=c('grey70','grey60'), lw=3)))
invisible(apply(outtab_st[r, c('b_0_st','b_temp_st','b_ftt_st','b_txf_st')], 1, function(x) surv_ftt(x, temp=19, alpha=4, omega=18)))
invisible(apply(rbind(colMeans(outtab_st[r, c('b_0_st','b_temp_st','b_ftt_st','b_txf_st')])), 1, function(x) surv_ftt(x, temp=19, alpha=4, omega=18, lcol=c('olivedrab','turquoise4'), lw=3)))
legend(15, 0.4, c(' ',' '), col=c('chartreuse','aquamarine'), lwd=10, bty='n')
legend(15, 0.4, c('19C','21C'), col=c('olivedrab','turquoise4'), lwd=3, bty='n')
#####

# mcnary passage and temp ----
plot(sts$mca_jul, sts$ihr_temp/735, pch=19, cex=0.5, ylim=c(0,0.04))
hist(sts$mca_jul, breaks=30, freq=FALSE, add=TRUE, col=rgb(0,0,0,0.5))
abline(h=20/735, lty=2)

# model selection ----
# require(lme4)
# m1<- glmer(gra_det~ jul_sca+ jul2+ temp_sca+ temp2+ dis_sca+ mig_his+ (1|mca_yr), family= binomial, data= sts)
# vif_mer(m1)

m1<- glm(gra_det~ jul_sca+ jul2+ temp_sca+ temp2+ dis_sca+ dis2+ mig_his+ as.factor(mca_yr), family= binomial, data= sts)
m2<- update(m1, .~ .- as.factor(mca_yr))
AIC(m1,m2)
(6067.654-6075.209)/ (21-8)
# [1] -0.5811538
drop1(m2, test='Chisq')
m3<- update(m2, .~ .- dis2- dis_sca)
AIC(m2,m3)
drop1(m3, test='Chisq')
m4<- update(m3, .~ .- temp2)
drop1(m4, test='Chisq')

car::vif(m4)
m5<- update(m4,.~ .- jul2)
car::vif(m5) # no problem

summary(m4)

# check stray assignment ----
ftt_stry<- table(sts$stray, round(sts$ftt))
barplot(ftt_stry, main="Ftt for 0 vs 1 detection",
  xlab="Days", col=c("black","deeppink"), xlim=c(0,100),
  legend = rownames(ftt_stry))

#####

# same shit in Stan
rm(list=ls())
wd<- 'G:/STAFF/Bobby/css/adult_still_suc_2018/'
source(file=paste0(wd, "steelyhead/st_suc_util.R")) # where the functions are
sts<- load_dat_st(wd)
#
# stan model ----
cat("
  data {
    int<lower=0> n_obs;
    int<lower=0> n_mis;
    int<lower=0> N;
      int<lower=0, upper=1> stray[N];
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
    real b_trans;
    real b_ftt;

    real<lower=0> mu_v[2];
    real<lower=0> sigma_v[2];

    real<lower=0> vel_mis[n_mis];
  }
  
  transformed parameters{
    vector<lower=0, upper=1>[N] phi;
    real ftt_mis[n_mis];
    for (i in 1:n_obs)
      phi[i]= inv_logit(b_0+ b_juld*juld[i]+ b_juld2*juld2[i]+
        b_temp*temp[i]+ b_ftt*ftt_obs[i]+ b_trans*trans[i]);
    for (j in (n_obs+1):N) {
      ftt_mis[j-n_obs]= 225/ vel_mis[j-n_obs];
      // ftt_mis[j-n_obs]= (225/ vel_mis[j-n_obs]- m_ftt)/ sd_ftt;
      phi[j]= inv_logit(b_0+ b_juld*juld[j]+ b_juld2*juld2[j]+
        b_temp*temp[j]+ b_ftt*ftt_mis[j-n_obs]+ b_trans*trans[j]);
    }
  }

  model{
    // GLM
    b_0~ student_t(1, 0, 10);
    b_juld~ student_t(1, 0, 2.5);
    // b_juld2~ student_t(1, 0, 2.5);
    b_temp~ student_t(1, 0, 2.5);
    b_ftt~ student_t(1, 0, 2.5);
    b_trans~ student_t(1, 0, 2.5);

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
    real ir_rep[N];
    real tr_rep[N];
    real ir_obs[N];
    real tr_obs[N];
    real tm_rep;
    real trm_rep;
    real trm_obs;
  
    for (i in 1:n_obs)
      phi_rep[i]= inv_logit(b_0+ b_juld*juld[i]+ b_juld2*juld2[i]+
        b_temp*temp[i]+ b_ftt*ftt_obs[i]+ b_trans*trans[i]);
    for (j in (n_obs+1):N)
      phi_rep[j]= inv_logit(b_0+ b_juld*juld[j]+ b_juld2*juld2[j]+
        b_temp*temp[j]+ b_ftt*ftt_mis[j-n_obs]+ b_trans*trans[j]);
    // posterior predictive
    for (n in 1:N){
      det_rep[n]= bernoulli_rng(phi_rep[n]);
      t_rep[n]= det_rep[n]* temp[n];
        // mig history
      ir_rep[n]= (trans[n]-1)* (-1)* (det_rep[n]- phi_rep[n]);
      tr_rep[n]= trans[n]* (det_rep[n]- phi_rep[n]);
      ir_obs[n]= (trans[n]-1)* (-1)* (det[n]- phi[n]);
      tr_obs[n]= trans[n]* (det[n]- phi[n]);
    }
    tm_rep= mean(t_rep);
    trm_rep= sum(ir_rep)/(sum(trans-1)*(-1))- sum(tr_rep)/sum(trans);
    trm_obs= sum(ir_obs)/(sum(trans-1)*(-1))- sum(tr_obs)/sum(trans);
  }

",fill=TRUE, file=paste0(wd, "steelyhead/st_im/in_stan/st_im_glm.stan"))
#####

# data
strytime<- 999
im_data_s<- stan_dat(sts, strytime=strytime)
str(im_data_s)

require(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# run stan ----
# nc<- 1; ni<- 60; nt<- 1 # test run, burn-in 50%
# nc<- 4; ni<- 500; nt<- 1
nc<- 4; ni<- 5000; nt<- 1 # ~2 hrs

parameters <- c('b_0','b_juld','b_juld2','b_temp','b_ftt','b_trans','mu_v','sigma_v','tm_rep','trm_rep','trm_obs','vel_mis')#

steely_fit<- stan(data=im_data_s, file=paste0(wd, "steelyhead/st_im/in_stan/st_im_glm.stan"), chains=nc, iter=ni, thin=nt, pars=parameters, include=TRUE)

fit_summ <- summary(steely_fit)
round(fit_summ$summary, 3)[c(1:13,919),]
sum(fit_summ$summary[,10]>1.01)

# steely_sims <- extract(steely_fit)
# print(names(steely_sims))

df_steely <- as.data.frame(steely_fit)
dim(df_steely)

# save(steely_fit, file=paste0(wd, 'steelyhead/st_im/in_stan/steely_fit.R'))
# write.table(df_steely, file=paste0(wd, 'steelyhead/st_im/in_stan/im_glm_sims_st.txt'))
# im_rhat_st<- fit_summ$summary[,9:10]
# write.table(im_rhat_st, file=paste0(wd, 'steelyhead/st_im/in_stan/im_rhat_st.txt'))
df_steely<- read.table('steelyhead/st_im/in_stan/im_glm_sims_st.txt')
#####

# output table ----
im_median_st<- cbind(apply(df_steely, 2, median))
im_se_st<- cbind(apply(df_steely, 2, sd))
im_cri_st<- apply(df_steely, 2, function(x) quantile(x, c(0.025,0.975)))

summ_st<- data.frame(cbind(round(im_median_st,3), round(im_se_st,3), paste0('(', round(im_cri_st[1,], 3),', ', round(im_cri_st[2,], 3),')'), round(im_rhat_st, 3)))[-c(11:918),]
row.names(summ_st)<- c('(Intercept)', 'Arrival Date', 'Arrival^2^', 'Temperature', 'Travel Time', 'Transported', '$\\mu_{1vel}$', '$\\mu_{2vel}$', '$\\sigma_{1vel}$', '$\\sigma_{2vel}$', 'lp__')
colnames(summ_st)<- c('Median','SD','95% CRI','Eff size','$\\hat{R}$')
summ_st

# traceplot ----
windows(8,9)
par(mfrow=c(4,2))
lnames<- c('b0 (Intercept)', 'b Arrival', 'b Arr^2', 'b Temp', 'b Ftt', 'b Trans', 'Mu1Vel', 'Mu2Vel', 'Sigma1Vel', 'Sigma2Vel')

# ncol(df_steely)
pn1<- 1:4; pn2<- 5:8; pn3<- 9:10
for(i in pn1){
  plot_pds(df_steely[,i], lab=names(df_steely)[i], colr='grey70')
}


# simulated ftt vs. observed ftt ----
nstry<- table(sts$stray)
vel1_pre_st<- rnorm(nstry[1], 22.708, 9.116)
vel1_pre_st<- vel1_pre_st[vel1_pre_st> 0]
vel2_pre_st<- rnorm(nstry[2], 5.317, 3.028)
vel2_pre_st<- vel2_pre_st[vel2_pre_st> 0]
ftt_pre_st<- 225/ c(vel1_pre_st, vel2_pre_st)
# ftt1_pre_st<- 225/ vel1_pre_st

impftt<- 225/df_steely[, 14:918] # imputated ftt

windows()
par(mfrow=c(3,3))
d<- sample(1:nrow(df_steely), 9)
for(i in 1:9) {
  obsftt<- c(as.numeric(impftt[d[i],]), sts$ftt)
  hist(obsftt[obsftt< 100], breaks= 70, freq= F, xlim=c(0,100),
    col=rgb(0,0,0,1), border='white', main=d[i], xlab='Days')
  hist(ftt_pre_st[ftt_pre_st< 100], breaks=70, freq= F, col=rgb(1,1,1,0.5), add= T)
  # hist(ftt1_pre_st[ftt1_pre_st< 60], breaks=50, freq= F, col=rgb(1,1,1,0.5), add= T)
}
legend(40, 0.15, c('Predicted','Observed'), pch=c(0,15), col=c(1,1), bty='n')

impvel<- df_steely[, 14:918] # imputated ftt
for(i in 1:9) {
  obsvel<- c(as.numeric(impvel[d[i],]), sts$vel)
  hist(obsvel, breaks= 60, freq= F, xlim=c(0,100),
    col=rgb(0,0,0,1), border='white', main=d[i], xlab='Days')
  hist(225/ftt_pre_st, breaks=60, freq= F, col=rgb(1,1,1,0.5), add= T)
  # hist(ftt1_pre_st[ftt1_pre_st< 60], breaks=50, freq= F, col=rgb(1,1,1,0.5), add= T)
}
legend(40, 0.15, c('Predicted','Observed'), pch=c(0,15), col=c(1,1), bty='n')

# compare with and without mixture
par(mfrow=c(1,2))
ftt1_pre_st<- 225/ vel1_pre_st
r<- sample(1:nrow(df_steely), 1)
obsftt<- c(as.numeric(impftt[r,]), sts$ftt)
# vel 1
hist(obsftt[obsftt< 100], breaks= 60, freq= F, xlim=c(0,100),
  col=rgb(0,0,0,1), border='white', main=d[i], xlab='Days')
hist(ftt1_pre_st[ftt1_pre_st< 100], breaks=60, freq= F, col=rgb(1,1,1,0.5), add= T)
# vel 1 and 2
hist(obsftt[obsftt< 100], breaks=60, freq= F, xlim=c(0,100),
  col=rgb(0,0,0,1), border='white', main=d[i], xlab='Days')
hist(ftt_pre_st[ftt_pre_st< 100], breaks=60, freq= F, col=rgb(1,1,1,0.5), add= T)

# stacked ftt ----
nusts<- subset(sts, ftt< strytime|is.na(ftt))
impftt<- 225/df_steely[, 14:918] # imputated ftt
windows()
par(mfrow=c(1,3))
d<- sample(1:nrow(df_steely), 6)
for (i in 1:3){
  impsts<- nusts[order(nusts$ftt),]
  impsts[is.na(impsts$ftt),]$ftt<- as.numeric(impftt[d[i],])
  fttcts<- with(impsts, table(gra_det, round(ftt)))[c(2,1),]
  barplot(fttcts, main="Ftt for 0 vs 1 detection",
    xlab="Days", col=c("black","deeppink"), xlim=c(0,100),
    legend = rownames(fttcts))
}
#####

# plotting survival relationships ----
pn<- nrow(df_steely)
nsim<- 1000
r<- sample(1:pn, nsim)
windows(10,4)
par(mfrow=c(1,2))
par(mar=c(5,4,2,2)+0.1) # original 5,4,4,2
# with temperature ----
# quantile(sts$ihr_temp, c(0, 0.025, 0.5, 0.975, 1), na.rm=TRUE)
#       0%     2.5%      50%    97.5%     100% 
# 14.57917 17.93333 20.05833 21.88333 22.77778 
dafr<- df_steely[, c('b_0','b_temp','b_ftt','b_trans')]
plot(0,0, xlim=c(14,23), ylim=c(0,1), ty='n',
  xlab='Temperature (Celsius)', ylab='Conversion')
invisible(apply(dafr[r,], 1,
  function(x) surv_temp_st(x, ftt=6, lcol=c('grey90','grey80')) ))
invisible(apply(rbind(colMeans(dafr)), 1,
  function(x) surv_temp_st(x, ftt=6, lcol=c('grey70','grey60'), lw=3)))
invisible(apply(dafr[r,], 1,
  function(x) surv_temp_st(x, ftt=6, alpha=17, omega=22)))
invisible(apply(rbind(colMeans(dafr)), 1,
  function(x) surv_temp_st(x, ftt=6, alpha=17, omega=22,
    lcol=c('navy',rgb(255,20,147,alpha=150, max=255)), lw=3)))
legend(14, 0.4, c(' ',' '), col=c('cyan','lightpink'), lwd=10, bty='n')
legend(14, 0.4, c('In-River','Transported'), col=c('navy','deeppink'), lwd=3, bty='n')

# sts$tempbin<- cut(sts$ihr_temp, breaks=c(10,15,17,19,21,23))
# points(seq(15,23, by=2), tapply(sts$gra_det, sts$tempbin, mean), pch=20, cex=2)

# with ftt ----
# quantile(sts$ftt, c(0, 0.025, 0.5, 0.975, 1), na.rm=TRUE)
# 0%  2.5%   50% 97.5%  100% 
# 3     6    11    94   300
plot(0,0, xlim=c(3,60), ylim=c(0,1),
  ty='n', xlab='Travel Time (Days)', ylab='Conversion')
invisible(apply(dafr[r,], 1,
  function(x) surv_ftt_st(x, temp=19, lcol=c('grey90','grey80')) ))
invisible(apply(rbind(colMeans(dafr)), 1,
  function(x) surv_ftt_st(x, temp=19, lcol=c('grey70','grey60'), lw=3)))
invisible(apply(dafr[r,], 1,
  function(x) surv_ftt_st(x, temp=19, alpha=6, omega=94)))
invisible(apply(rbind(colMeans(dafr)), 1,
  function(x) surv_ftt_st(x, temp=19, alpha=6, omega=94,
    lcol=c('olivedrab','turquoise4'), lw=3)))
legend(15, 0.4, c(' ',' '), col=c('chartreuse','aquamarine'), lwd=10, bty='n')
legend(15, 0.4, c('19C','21C'), col=c('olivedrab','turquoise4'), lwd=3, bty='n')
#####


































