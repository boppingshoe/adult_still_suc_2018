
# load/format data
rm(list=ls())
wd<- 'G:/STAFF/Bobby/css/adult_still_suc_2018/'
source(file=paste0(wd, "fall_chinooka/fc_suc_util.R")) # where the functions are
fcs<- load_dat_fc(wd)
#

## low snake survivals vs. temperature
# with GLM ----
cat('
model{
  
  for (i in 1:n_ind){
    # GLM
    det[i]~ dbern(phi[i])
    # det[i]~ dbern(phi_bound[i])
    # phi_bound[i]<- max(0.0000000001, min(0.9999999999, phi[i]))
    logit(phi[i])<- b_0+ b_juld*juld[i]+ b_juld2*juld2[i]+ b_temp*temp[i]+
      b_ftt*ftt[i]+ b_txf*temp[i]*ftt[i]+ b_trans*trans[i]
    r_obs[i]<- det[i]- phi[i]
    det_rep[i]~ dbern(phi_rep[i])
    # det_rep[i]~ dbern(pr_bound[i])
    # pr_bound[i]<- max(0.0000000001, min(0.9999999999, phi_rep[i]))
    logit(phi_rep[i])<- b_0+ b_juld*juld[i]+ b_juld2*juld2[i]+ b_temp*temp[i]+
      b_ftt*ftt[i]+ b_txf*temp[i]*ftt[i]+ b_trans*trans[i]
    r_rep[i]<- det_rep[i]- phi_rep[i]

    # FTT
    ftt[i]<- (225/vel[i]- 7.88)/ 4.29 # scaled ftt 
    vel[i]~ dnorm(mu_v, tau_v)T(0,)
  }

  # priors
  # GLM
  b_0~ dt(0, 0.1, 1)
  b_juld~ dt(0, 0.4, 1)
  b_juld2~ dt(0, 0.4, 1)
  b_temp~ dt(0, 0.4, 1)
  b_ftt~ dt(0, 0.4, 1)
  b_txf~ dt(0, 0.4, 1)
  b_trans~ dt(0, 0.4, 1)

  # FTT
  mu_v~ dt(0, 0.1, 1)T(0,)
  sigma_v~ dt(0, 0.444, 1)T(0,)
  tau_v<- pow(sigma_v, -2)

  # posterior predictive
  t_obs<- sum(r_obs)/n_ind
  t_rep<- sum(r_rep)/n_ind

}', fill=TRUE, file = paste0(wd,'fall_chinooka/fc_im/fc_im_glm2.txt'))
#####
require(jagsUI)

im_data<- prep_dat_fc(fcs, typ='jags')
str(im_data)

# run JAGS ----
parameters <- c('b_0','b_juld','b_juld2','b_temp','b_ftt','b_txf','b_trans','mu_v','sigma_v','t_obs','t_rep')
# inits<- function() {list(b_0=runif(1,-1,1), b_juld=runif(1,-1,1), b_juld2=runif(1,-1,1), b_temp=runif(1,-1,1), b_ftt=runif(1,-1,1), b_txf=runif(1,-1,1), b_trans=runif(1,-1,1), mu_v=runif(1,1,8), sigma_v=runif(1,0,2) )}
inits<- function() {list(b_0=4, b_juld=6, b_juld2=-6, b_temp=runif(1,-1,1), b_ftt=runif(1,1,2), b_txf=runif(1,0,1), b_trans=runif(1,-2,1), mu_v=33, sigma_v=10 )}

# nc<- 4   ;   ni<- 100   ;   nb<- 0   ;   nt<- 1 # test run
nc<- 4   ;   ni<- 1000   ;   nb<- 500   ;   nt<- 1 # test run2
# nc<- 4   ;   ni<- 30000   ;   nb<-20000   ;   nt<- 2 # 6 hrs
nc<- 4   ;   ni<- 80000   ;   nb<-50000   ;   nt<- 3

imglm_out<- jags(im_data, inits, parameters, "fall_chinooka/fc_im/fc_im_glm2.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)
# im_out<- autojags(im_data, inits, parameters, "fall_chinooka/fc_im/fc_im.txt", n.thin=nt, n.chains=nc, n.burnin=2000, iter.increment=5000, max.iter=42000, parallel=TRUE)
# imglm_out<- update(imglm_out, parameters.to.save=parameters, n.thin=1, n.chains=4, n.iter=50000, parallel=TRUE)
print(imglm_out)

im_sims_fc<- imglm_out$sims.list
im_rhat_fc<- cbind(unlist(imglm_out$Rhat)[!is.na(unlist(imglm_out$Rhat))], unlist(imglm_out$n.eff)[unlist(imglm_out$n.eff)>1])
# JAGS results saved as as a big table
# write.table(im_sims_fc, file='fall_chinooka/fc_im/im_glm2_sims_fc.txt')
# write.table(im_rhat_fc, file='fall_chinooka/fc_im/im_glm2_rhat_fc.txt')
# save(imglm_out, file='fall_chinooka/fc_im/im_glm_out.R')
# load(file='fall_chinooka/fc_im/im_glm_out.R')

# convert output ----
im_sims_fc<- read.table('fall_chinooka/fc_im/im_glm2_sims_fc.txt')
im_rhat_fc<- read.table('fall_chinooka/fc_im/im_glm2_rhat_fc.txt')

b_0_fc=im_sims_fc$b_0; b_juld_fc=im_sims_fc$b_juld; b_juld2_fc=im_sims_fc$b_juld2; b_temp_fc=im_sims_fc$b_temp; b_ftt_fc=im_sims_fc$b_ftt; b_txf_fc=im_sims_fc$b_txf; b_trans_fc=im_sims_fc$b_trans; mu_v_fc=im_sims_fc$mu_v; sigma_v_fc=im_sims_fc$sigma_v; devi_fc=im_sims_fc$deviance
t_obs_fc=im_sims_fc$t_obs; t_rep_fc=im_sims_fc$t_rep

outtab_fc<- cbind(b_0_fc, b_juld_fc, b_juld2_fc, b_temp_fc, b_ftt_fc, b_txf_fc, b_trans_fc, mu_v_fc, sigma_v_fc, devi_fc)

# output table ----
im_median_fc<- cbind(apply(outtab_fc, 2, median))
im_se_fc<- cbind(apply(outtab_fc, 2, sd))
im_cri_fc<- apply(outtab_fc, 2, function(x) quantile(x, c(0.025,0.975)))

summ_fc<- data.frame(cbind(round(im_median_fc,3), round(im_se_fc,3), paste0('(', round(im_cri_fc[1,], 3),', ', round(im_cri_fc[2,], 3),')'), round(im_rhat_fc[-c(10,11),], 3)))
row.names(summ_fc)<- c('(Intercept)', 'Arrival Date', 'Arrival^2^', 'Temperature', 'Travel Time', 'TempxFtt', 'Transported', '$\\mu_{vel}$', '$\\sigma_{vel}$', 'Deviance')
colnames(summ_fc)<- c('Median','SD','95% CRI','$\\hat{R}$','Eff size')
summ_fc

# traceplot ----
windows(8,9)
par(mfrow=c(4,2))
names(outtab_fc)<- c('b0 (Intercept)', 'b Arrival', 'b Arr^2', 'b Temp', 'b Ftt', 'b TempxFtt', 'b Trans', 'MuVel', 'SigmaVel', 'Deviance')

# ncol(outtab_fc)
pn1<- 1:4; pn2<- 5:8; pn3<- 9:10
for(i in pn1){
  plot_pds(outtab_fc[,i], lab=names(outtab_fc)[i], colr='grey70')
}

# plotting survival relationships ----
pn<- nrow(outtab_fc)
nsim<- 1000
r<- sample(1:pn, nsim)
windows(10,4)
par(mfrow=c(1,2))
par(mar=c(5,4,2,2)+0.1) # original 5,4,4,2
# with temperature ----
# quantile(fcs$ihr_temp, c(0.025,0.975), na.rm=TRUE) # 17.61250 21.61574
# quantile(fcs$mca_jul, c(0.025,0.975), na.rm=TRUE) # 228 277 (range= 128 to 319)
plot(0,0, xlim=c(13,23), ylim=c(0,1), ty='n',
  xlab='Temperature (Celsius)', ylab='Conversion')
invisible(apply(outtab_fc[r, c('b_0_fc','b_juld_fc','b_juld2_fc','b_temp_fc','b_trans_fc')], 1, function(x) surv_temp(x, lcol=c('grey90','grey80')) ))
invisible(apply(rbind(colMeans(outtab_fc[r, c('b_0_fc','b_juld_fc','b_juld2_fc','b_temp_fc','b_trans_fc')])), 1, function(x) surv_temp(x, lcol=c('grey70','grey60'), lw=3)))
invisible(apply(outtab_fc[r, c('b_0_fc','b_juld_fc','b_juld2_fc','b_temp_fc','b_trans_fc')], 1, function(x) surv_temp(x, alpha=17, omega=22)))
invisible(apply(rbind(colMeans(outtab_fc[r, c('b_0_fc','b_juld_fc','b_juld2_fc','b_temp_fc','b_trans_fc')])), 1, function(x) surv_temp(x, alpha=17, omega=22, lcol=c('navy','deeppink'), lw=3)))
legend(18, 0.4, c(' ',' '), col=c('cyan','lightpink'), lwd=10, bty='n')
legend(18, 0.4, c('In-River','Transported'), col=c('navy','deeppink'), lwd=3, bty='n')

# fcs$tempbin<- cut(fcs$ihr_temp, breaks=c(10,15,17,19,21,23))
# points(seq(15,23, by=2), tapply(fcs$gra_det, fcs$tempbin, mean), pch=20, cex=2)

# with ftt ----
# quantile(fcs$ftt, c(0.025,0.975), na.rm=TRUE) # 4.219896 18.005590
plot(0,0, xlim=c(3,25), ylim=c(0,1), ty='n',
  xlab='Travel Time (Days)', ylab='Conversion')
invisible(apply(outtab_fc[r, c('b_0_fc','b_temp_fc','b_ftt_fc','b_txf_fc','b_trans_fc')], 1, function(x) surv_ftt(x, temp=19, lcol=c('grey90','grey80')) ))
invisible(apply(rbind(colMeans(outtab_fc[r, c('b_0_fc','b_temp_fc','b_ftt_fc','b_txf_fc','b_trans_fc')])), 1, function(x) surv_ftt(x, temp=19, lcol=c('grey70','grey60'), lw=3)))
invisible(apply(outtab_fc[r, c('b_0_fc','b_temp_fc','b_ftt_fc','b_txf_fc','b_trans_fc')], 1, function(x) surv_ftt(x, temp=19, alpha=4, omega=18)))
invisible(apply(rbind(colMeans(outtab_fc[r, c('b_0_fc','b_temp_fc','b_ftt_fc','b_txf_fc','b_trans_fc')])), 1, function(x) surv_ftt(x, temp=19, alpha=4, omega=18, lcol=c('olivedrab','turquoise4'), lw=3)))
legend(15, 0.4, c(' ',' '), col=c('chartreuse','aquamarine'), lwd=10, bty='n')
legend(15, 0.4, c('19C','21C'), col=c('olivedrab','turquoise4'), lwd=3, bty='n')
#####
# mcnary passage and temp ----
plot(fcs$mca_jul, fcs$ihr_temp/735, pch=19, cex=0.5, ylim=c(0,0.04))
hist(fcs$mca_jul, breaks=30, freq=FALSE, add=TRUE, col=rgb(0,0,0,0.5))
abline(h=20/735, lty=2)

# model selection ----
m1<- glm(gra_det~ jul_sca+ jul2+ temp_sca+ temp2+ dis_sca+ dis2+ mig_his+ as.factor(mca_yr), family= binomial, data= nufcs)
m2<- update(m1, .~ .- as.factor(mca_yr))
AIC(m1,m2)
(2758.244-2775.002)/ (22-8)
# [1] -1.197
drop1(m2, test='Chisq')
m3<- update(m2, .~ .- temp2)
m4<- update(m3, .~ .- dis2)
m5<- update(m4, .~ .- dis_sca)
m6<- update(m5, .~ .- jul2)
AIC(m2,m3,m4,m5,m6)
# df      AIC
# m2  8 2775.002
# m3  7 2773.089
# m4  6 2776.757
# m5  5 2774.855
# m6  4 2815.251

# strays ----
fcs$stray<- apply (fcs[,c('pra_obs','ria_obs','wea_obs')], 1, function(x) as.numeric(any(!is.na(x))))
table(fcs$ftt>21, fcs$stray)
table(fcs$gra_det, fcs$stray)

# check stray assignment ----
ftt_stry<- table(fcs$stray, round(fcs$ftt))
barplot(ftt_stry, main="Ftt for 0 vs 1 detection",
  xlab="Days", col=c("black","deeppink"),
  legend = rownames(ftt_stry))

gs<- table(fcs$gra_det, fcs$stray)
mcgs<- table(fcs$gra_det, fcs$mcstray)
gs[2,2]<- gs[2,2]-mcgs[2,2]
stry_tbl<- rbind(c(' ','Erratic','Stray'), cbind(gs, mcgs[,2]))
stry_tbl[2,2]<- '-'
rownames(stry_tbl)<- c(' ', 'Missing', 'Succeed')
colnames(stry_tbl)<- c('Continuous', 'Interruted', ' ')
noquote(stry_tbl)
#####


# same shit in Stan
rm(list=ls())
wd<- 'G:/STAFF/Bobby/css/adult_still_suc_2018/'
source(file=paste0(wd, "fall_chinooka/fc_suc_util.R")) # where the functions are
fcs<- load_dat_fc(wd)
#
# stan model ----
cat("
  data {
    int<lower=0> n_obs;
    int<lower=0> n_mis;
    int<lower=0> N;
    int<lower=0, upper=1> det[N];
      int<lower=0, upper=1> stray[N];
    real juld[N];
    real juld2[N];
    real temp[N];
    real<lower=0> vel_obs[n_obs];
    real ftt_obs[n_obs];
    vector<lower=0, upper=1>[N] trans;
    // real m_ftt;
    // real sd_ftt;
  }
  
  parameters{
    real b_0;
    real b_juld;
    real b_juld2;
    real b_temp;
    real b_ftt;
    // real b_txf;
    real b_trans;
    
    real<lower=0> mu_v[2];
    real<lower=0> sigma_v[2];
    
    real<lower=0> vel_mis[n_mis];
  }
  
  transformed parameters{
    vector<lower=0, upper=1>[N] phi;
    real ftt_mis[n_mis];
    for (i in 1:n_obs)
      phi[i]= inv_logit(b_0+ b_juld*juld[i]+ b_juld2*juld2[i]+
        b_temp*temp[i]+ b_ftt*ftt_obs[i]+// b_txf*temp[i]*ftt_obs[i]+
        b_trans*trans[i]);
    for (j in (n_obs+1):N) {
      // ftt_mis[j-n_obs]= 225/ vel_mis[j-n_obs]- m_ftt)/ sd_ftt;
      ftt_mis[j-n_obs]= 225/ vel_mis[j-n_obs];
      phi[j]= inv_logit(b_0+ b_juld*juld[j]+ b_juld2*juld2[j]+
        b_temp*temp[j]+ b_ftt*ftt_mis[j-n_obs]+// b_txf*temp[j]*ftt_mis[j-n_obs]+
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
    // b_txf~ student_t(1, 0, 2.5);
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
    // vel_obs~ normal(mu_v, sigma_v);
    // vel_mis~ normal(mu_v, sigma_v);
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
        b_temp*temp[i]+ b_ftt*ftt_obs[i]+// b_txf*temp[i]*ftt_obs[i]+
        b_trans*trans[i]);
    for (j in (n_obs+1):N)
      phi_rep[j]= inv_logit(b_0+ b_juld*juld[j]+ b_juld2*juld2[j]+
        b_temp*temp[j]+ b_ftt*ftt_mis[j-n_obs]+// b_txf*temp[j]*ftt_mis[j-n_obs]+
        b_trans*trans[j]);
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

",fill=TRUE, file=paste0(wd, "fall_chinooka/fc_im/in_stan/fc_im_glm.stan"))
#####

# data
strytime<- 999
im_data_s<- prep_dat_fc(fcs, strytime=strytime, typ='stan')
str(im_data_s)

require(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# run stan ----
# nc<- 1; ni<- 60; nt<- 1 # test run, burn-in 50%
nc<- 4; ni<- 5000; nt<- 1 # ~25 mins

parameters <- c('b_0','b_juld','b_juld2','b_temp','b_ftt','b_trans','mu_v','sigma_v','tm_rep','trm_rep','trm_obs','vel_mis') # ,'b_txf'

fc_fit<- stan(data=im_data_s, file=paste0(wd, "fall_chinooka/fc_im/in_stan/fc_im_glm.stan"), chains=nc, iter=ni, thin=nt, pars=parameters, include=TRUE)#, control = list(adapt_delta = 0.9))

fit_summ <- summary(fc_fit)
round(fit_summ$summary, 3)[c(1:13,416),]
any(fit_summ$summary[,10]>1.01)

df_fc <- as.data.frame(fc_fit)
dim(df_fc)

# save(fc_fit, file=paste0(wd, 'fall_chinooka/fc_im/in_stan/fc_fit.R'))
# write.table(df_fc, file=paste0(wd, 'fall_chinooka/fc_im/in_stan/im_glm_sims_fc.txt'))
# im_rhat_fc<- fit_summ$summary[,9:10]
# write.table(im_rhat_fc, file=paste0(wd, 'fall_chinooka/fc_im/in_stan/im_rhat_fc.txt'))

# convert output ----
df_fc<- read.table('fall_chinooka/fc_im/in_stan/im_glm_sims_fc.txt')

nufcs<- subset(fcs, ftt< strytime|is.na(ftt))
#
# output table ----
im_median_fc<- cbind(apply(df_fc, 2, median))
im_se_fc<- cbind(apply(df_fc, 2, sd))
im_cri_fc<- apply(df_fc, 2, function(x) quantile(x, c(0.025,0.975)))

summ_fc<- data.frame(cbind(round(im_median_fc,3), round(im_se_fc,3), paste0('(', round(im_cri_fc[1,], 3),', ', round(im_cri_fc[2,], 3),')'), round(im_rhat_fc, 3)))[-10:413,]
row.names(summ_fc)<- c('(Intercept)', 'Arrival Date', 'Arrival^2^', 'Temperature', 'Travel Time', 'TempxFtt', 'Transported', '$\\mu_{vel}$', '$\\sigma_{vel}$', 'lp__')
colnames(summ_fc)<- c('Median','SD','95% CRI','Eff size','$\\hat{R}$')
summ_fc

# traceplot ----
windows(8,9)
par(mfrow=c(4,2))
lnames<- c('b0 (Intercept)', 'b Arrival', 'b Arr^2', 'b Temp', 'b Ftt', 'b Trans', 'Mu1Vel', 'Mu2Vel', 'Sigma1Vel', 'Sigma2Vel', 'tm_rep', 'trm_rep', 'trm_obs', 'lp__')#, 'b TempxFtt'

# ncol(df_fc)
pn1<- 1:4; pn2<- 5:8; pn3<- c(9,10,416)
for(i in pn3){
  plot_pds(df_fc[,i], lab=lnames[i], colr='grey70')
}

# simulated ftt vs. observed ftt ----
nstry<- table(fcs$stray)
vel1_pre_fc<- rnorm(nstry[1], 33.169, 9.562)
vel1_pre_fc<- vel1_pre_fc[vel1_pre_fc> 0]
vel2_pre_fc<- rnorm(nstry[2], 7.807, 2.928)
vel2_pre_fc<- vel2_pre_fc[vel2_pre_fc> 0]
ftt_pre_fc<- 225/ c(vel1_pre_fc, vel2_pre_fc)

impftt<- 225/df_fc[, 14:415] # imputated ftt

windows()
par(mfrow=c(3,3))
d<- sample(1:nrow(df_fc), 9)
for(i in 1:9) {
  obsftt<- c(as.numeric(impftt[d[i],]), fcs$ftt)
  hist(obsftt[obsftt< 60], breaks= 50, freq= F, xlim=c(0,60),
    col=rgb(0,0,0,1), border='white', main=d[i], xlab='Days')
  hist(ftt_pre_fc[ftt_pre_fc< 60], breaks=50, freq= F, col=rgb(1,1,1,0.5), add= T)
}
legend(40, 0.15, c('Predicted','Observed'), pch=c(0,15), col=c(1,1), bty='n')

# compare with and without mixture
par(mfrow=c(1,2))
ftt1_pre_fc<- 225/ vel1_pre_fc
r<- sample(1:nrow(df_fc), 1)
obsftt<- c(as.numeric(impftt[r,]), fcs$ftt)
# vel 1
hist(obsftt[obsftt< 60], breaks= 50, freq= F, xlim=c(0,60),
  col=rgb(0,0,0,1), border='white', main=d[i], xlab='Days')
hist(ftt1_pre_fc[ftt1_pre_fc< 60], breaks=50, freq= F, col=rgb(1,1,1,0.5), add= T)
# vel 1 and 2
hist(obsftt[obsftt< 60], breaks=50, freq= F, xlim=c(0,60),
  col=rgb(0,0,0,1), border='white', main=d[i], xlab='Days')
hist(ftt_pre_fc[ftt_pre_fc< 60], breaks=50, freq= F, col=rgb(1,1,1,0.5), add= T)

# stacked ftt ----
impftt<- 225/df_fc[, 14:415] # imputated ftt
windows()
par(mfrow=c(1,3))
d<- sample(1:nrow(df_fc), 6)
for (i in 1:3){
  impfcs<- nufcs[order(nufcs$ftt),]
  # impfcs<- fcs[order(fcs$ftt),]
  impfcs[is.na(impfcs$ftt),]$ftt<- as.numeric(impftt[d[i],])
  fttcts<- with(impfcs, table(gra_det, round(ftt)))[c(2,1),]
  barplot(fttcts, main="Ftt for 0 vs 1 detection",
    xlab="Days", col=c("black","deeppink"),
    legend = rownames(fttcts))
}

#####

# plotting survival relationships ----
pn<- nrow(df_fc)
nsim<- 1000
r<- sample(1:pn, nsim)
windows(10,4)
par(mfrow=c(1,2))
par(mar=c(5,4,2,2)+0.1) # original 5,4,4,2
dafr_fc<- df_fc[, c('b_0','b_juld','b_juld2','b_temp','b_ftt','b_trans')]
# with temperature ----
# quantile(sts$ihr_temp, c(0.025,0.975), na.rm=TRUE) # 17.61250 21.61574
# quantile(sts$mca_jul, c(0.025,0.975), na.rm=TRUE) # 228 277 (range= 128 to 319)
plot(0,0, xlim=c(13,23), ylim=c(0,1), ty='n',
  xlab='Temperature (Celsius)', ylab='Conversion')
invisible(apply(dafr_fc[r,], 1, function(x) surv_temp(x, lcol=c('grey90','grey80')) ))
invisible(apply(rbind(colMeans(dafr_fc)), 1, function(x) surv_temp(x, lcol=c('grey70','grey60'), lw=3)))
invisible(apply(dafr_fc[r,], 1, function(x) surv_temp(x, alpha=17, omega=22)))
invisible(apply(rbind(colMeans(dafr_fc)), 1, function(x) surv_temp(x, alpha=17, omega=22, lcol=c('navy','deeppink'), lw=3)))
legend(14, 0.4, c(' ',' '), col=c('cyan','lightpink'), lwd=10, bty='n')
legend(14, 0.4, c('In-River','Transported'), col=c('navy','deeppink'), lwd=3, bty='n')

# sts$tempbin<- cut(sts$ihr_temp, breaks=c(10,15,17,19,21,23))
# points(seq(15,23, by=2), tapply(sts$gra_det, sts$tempbin, mean), pch=20, cex=2)

# with ftt ----
# quantile(sts$ftt, c(0.025,0.975), na.rm=TRUE) # 4.219896 18.005590
plot(0,0, xlim=c(3,25), ylim=c(0,1), ty='n',
  xlab='Travel Time (Days)', ylab='Conversion')
invisible(apply(dafr_fc[r,], 1, function(x) surv_ftt(x, temp=19, lcol=c('grey90','grey80')) ))
invisible(apply(rbind(colMeans(dafr_fc)), 1, function(x) surv_ftt(x, temp=19, lcol=c('grey70','grey60'), lw=3)))
invisible(apply(dafr_fc[r,], 1, function(x) surv_ftt(x, temp=19, alpha=4, omega=18)))
invisible(apply(rbind(colMeans(dafr_fc)), 1, function(x) surv_ftt(x, temp=19, alpha=4, omega=18, lcol=c('olivedrab','turquoise4'), lw=3)))
legend(5, 0.4, c(' ',' '), col=c('chartreuse','aquamarine'), lwd=10, bty='n')
legend(5, 0.4, c('19C','21C'), col=c('olivedrab','turquoise4'), lwd=3, bty='n')





