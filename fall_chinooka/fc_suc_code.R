
# load/format data
rm(list=ls())
wd<- 'G:/STAFF/Bobby/css/adult_still_suc_2018/'
source(file=paste0(wd, "fall_chinooka/fc_suc_util.R")) # where the functions are
fcs<- fc_load_dat(wd)
#
## intergrated model
# full set up with cjs (needs modify for mcn-lgr reach) ----
cat('
model{

  for (i in 1:n_ind){
    # CJS
    phi[i,1]<- g_0*exp(- g_1*ftt[i])
    phi[i,2]<- phi2p3
    
    z[i,1]<- 1
    for (t in 2:n_occ){
      z[i,t]~ dbern(phi[i,t-1]* z[i,t-1])
      y[i,t]~ dbern(p[t-1]* z[i,t])
    }
    
    # FTT
    # ftt[i]<- 236/vel[i] # bon-mcn
    ftt[i]<- 304/vel[i] # bon-ihr
    vel[i]~ dnorm(mu[i], 0.01)T(0,)
    mu[i]<- b_0+ b_juld*juld[i]+ b_temp*temp[i]+ w*b_temp2*temp2[i]+
      b_dis*dis[i]+ b_trans*trans[i]+ a_yr[yr[i]]
    # mu[i]<- b_0+ b_juld*juld[i]+ b_temp*temp[i]+
    # b_dis*dis[i]+ b_trans*trans[i]+ a_yr[yr[i]]
  }
  
  # priors
  # CJS
  phi2p3~ dunif(0.5, 1)
  p[1]~ dunif(0.5, 1)
  p[2]<- 1
  g_0~ dunif(0.5, 1)
  g_1~ dunif(0, 0.2)
  # FTT
  b_0~ dt(0, 0.1, 1)
  b_juld~ dt(0, 0.4, 1)
  b_temp~ dt(0, 0.4, 1)
  b_temp2~ dt(0, 0.4, 1)
  b_dis~ dt(0, 0.4, 1)
  b_trans~ dt(0, 0.4, 1)
  for (j in 1:15){
    a_yr[j]~ dnorm(0, tau_yr)
  }
  sigma_yr~ dt(0, 0.444, 1)T(0,5)
  tau_yr<- pow(sigma_yr, -2)
  w~ dbern(0.5)

}', fill=TRUE, file = paste0(wd,'fall_chinooka/fc_im/fc_im_cjs.txt'))
# ----
require(jagsUI)

im_data<- prep_dat(fcs, typ='w/cjs')
str(im_data)

# run JAGS ----
parameters <- c('b_0','b_juld','b_temp','b_temp2','b_dis','b_trans','a_yr','p','phi2p3','g_0','g_1','sigma_yr','w')
inits<- function() {list(z=cjs_init_z(im_data$y), b_0=runif(1,-1,1), b_juld=runif(1,-1,1), b_temp=runif(1,-1,1), b_temp2=runif(1,-1,1), b_dis=runif(1,-1,1), b_trans=runif(1,-1,1), p.1=runif(1,0.5,1), phi2p3=runif(1,0.5,1), g_0=runif(1,0.5,1), g_1=runif(1,0,0.1), sigma_yr=runif(1,0,2), w=rbinom(1,0,1))}
# linear (not doing it)

# nc<- 4   ;   ni<- 100   ;   nb<- 0   ;   nt<- 1 # test run
nc<- 4   ;   ni<- 80000   ;   nb<-10000   ;   nt<- 7

im_out<- jags(im_data, inits, parameters, "fall_chinooka/fc_im/fc_im_cjs.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)
# im_out<- update(im_out, parameters.to.save=parameters, n.thin=1, n.chains=4, n.iter=5000, parallel=TRUE)
print(im_out)

im_sims_fc<- im_out$sims.list
im_rhat_fc<- cbind(unlist(im_out$Rhat)[!is.na(unlist(im_out$Rhat))], unlist(im_out$n.eff)[unlist(im_out$n.eff)>1])
# JAGS results saved as as a big table
# write.table(im_sims_fc, file='fall_chinooka/fc_im/im_cjs_ihr_sims.txt')
# write.table(im_rhat_fc, file='fall_chinooka/fc_im/im_cjs_ihr_rhat.txt')

# convert saved JAGS results into vectors ----
im_sims_fc<- read.table('fall_chinooka/fc_im/im_cjs_ihr_sims.txt')
im_rhat_fc<- read.table('fall_chinooka/fc_im/im_cjs_ihr_rhat.txt')

b_0_fc=im_sims_fc$b_0; b_juld_fc=im_sims_fc$b_juld; b_temp_fc=im_sims_fc$b_temp; b_temp2_fc=im_sims_fc$b_temp2; b_dis_fc=im_sims_fc$b_dis; b_trans_fc=im_sims_fc$b_trans; ihr_p_fc=im_sims_fc$p.1; phi2p3_fc=im_sims_fc$phi2p3; g_0_fc=im_sims_fc$g_0; g_1_fc=im_sims_fc$g_1; sigma_yr_fc=im_sims_fc$sigma_yr; devi_fc=im_sims_fc$deviance

a_yr<- 3:17
for(i in 1:15){
  a_yr[i]= im_sims_fc[5+i]
}
ayrs<- data.frame(cbind(unlist(a_yr[1]), unlist(a_yr[2]), unlist(a_yr[3]), unlist(a_yr[4]), unlist(a_yr[5]), unlist(a_yr[6]), unlist(a_yr[7]), unlist(a_yr[8]), unlist(a_yr[9]), unlist(a_yr[10]), unlist(a_yr[11]), unlist(a_yr[12]), unlist(a_yr[13]), unlist(a_yr[14]), unlist(a_yr[15]) ))

# output table ----
outtab_fc<- cbind(b_0_fc, b_juld_fc, b_temp_fc, b_temp2_fc, b_dis_fc, b_trans_fc, ayrs[,1], ayrs[,2], ayrs[,3], ayrs[,4], ayrs[,5], ayrs[,6], ayrs[,7], ayrs[,8], ayrs[,9], ayrs[,10], ayrs[,11], ayrs[,12], ayrs[,13], ayrs[,14], ayrs[,15], ihr_p_fc, phi2p3_fc, g_0_fc, g_1_fc, sigma_yr_fc, devi_fc)
im_mean_fc<- cbind(colMeans(outtab_fc))
im_se_fc<- cbind(apply(outtab_fc, 2, sd))
im_cri_fc<- apply(outtab_fc, 2, function(x) quantile(x, c(0.025,0.975)))

summ_fc<- data.frame(cbind(round(im_mean_fc,3), round(im_se_fc,3), paste0('(', round(im_cri_fc[1,], 3),', ', round(im_cri_fc[2,], 3),')'), round(im_rhat_fc, 3)))
row.names(summ_fc)<- c('(Intercept)', 'Arrival Date', 'Temperature', 'Temperature2', 'Flow', 'Transported','Year 2003','Year 2004','Year 2005','Year 2006','Year 2007','Year 2008','Year 2009','Year 2010','Year 2011','Year 2012','Year 2013','Year 2014','Year 2015','Year 2016','Year 2017', 'Detection (IHR)', '$\\phi_2\\cdot p_3$', '$\\gamma_0$', '$\\gamma_1$', '$\\sigma_{year}$','Deviance')
colnames(summ_fc)<- c('Mean','SD','95% CRI','$\\hat{R}$','Eff size')
summ_fc

# traceplot ----
windows(8,9)
par(mfrow=c(4,2))
names(outtab_fc)<- c('b0 (Intercept)', 'b Arrival', 'b Temp', 'b Temp2', 'b Flow', 'b Trans', 'Year2003','Year2004','Year2005','Year2006','Year2007','Year2008','Year2009','Year2010','Year2011','Year2012','Year2013','Year2014','Year2015','Year2016','Year2017', 'IHR Detect', 'Phi2p3', 'gamma0', 'gamma1', 'SigmaYr', 'Deviance')

# ncol(outtab_fc)
pn1<- 1:4; pn2<- 5:8; pn3<- 9:12; pn4<- 13:16; pn5<- 17:20; pn6<- 21:24; pn7<- 25:27
for(i in pn7){
  plot_pds(outtab_fc[,i], lab=names(outtab_fc)[i], colr='grey70')
}

# surv plots ----
windows(10,4)
par(mfrow=c(1,2))
niter<- 1000
r<- sample(1:40000, size=niter)
# surv vs. ftt
# quantile(fcs$ftt, c(0.025,0.975), na.rm=TRUE)
plot(0,0, xlim=c(0,80), ylim=c(0,1), ty='n',
  xlab='Travel Time (Days)', ylab='Survival')
invisible(apply(rbind(outtab_fc[r,c('g_0_fc','g_1_fc')]), 1,
  function(x) surv_ftt(x, lcol='grey80')))
invisible(apply(rbind(colMeans(outtab_fc[,c('g_0_fc','g_1_fc')])), 1,
  function(x) surv_ftt(x, lcol='grey70',lw=3)))
invisible(apply(cbind(outtab_fc[r,c('g_0_fc','g_1_fc')]), 1,
  function(x) surv_ftt(x, alpha=4, omega=18)))
invisible(apply(rbind(colMeans(outtab_fc[,c('g_0_fc','g_1_fc')])), 1,
  function(x) surv_ftt(x, lcol='navy',lw=3, alpha=4, omega=18)))
legend(40, 0.9, '', col='grey50', lwd=10, bty='n')
legend(40, 0.9, 'Middle 95%', col='navy', lwd=3, bty='n')

# surv vs. temp
# travel time is intermediate between surv and temp
# if travel time show little relation, temperature can be dampen
# quantile(fcs$temp, c(0.025,0.975))
plot(0,0, xlim=c(15,25), ylim=c(0,1), ty='n',
  xlab='Temperature (Celsius)', ylab='Survival')
invisible(apply(outtab_fc[r, c('g_0_fc','g_1_fc','b_0_fc','b_temp_fc','b_temp2_fc','b_trans_fc')], 1, function(x) surv_temp(x, lcol=c('grey80','grey90'), alpha=15, omega=25, sig_v=5) ))
invisible(apply(rbind(colMeans(outtab_fc[r, c('g_0_fc','g_1_fc','b_0_fc','b_temp_fc','b_temp2_fc','b_trans_fc')])), 1, function(x) surv_temp(x, lcol=c('grey60','grey70'), lw=3, alpha=15, omega=25, sig_v=0)))
invisible(apply(outtab_fc[r, c('g_0_fc','g_1_fc','b_0_fc','b_temp_fc','b_temp2_fc','b_trans_fc')], 1, function(x) surv_temp(x, alpha=17, omega=23, sig_v=5)))
invisible(apply(rbind(colMeans(outtab_fc[r, c('g_0_fc','g_1_fc','b_0_fc','b_temp_fc','b_temp2_fc','b_trans_fc')])), 1, function(x) surv_temp(x, lcol=c('navy','deeppink'), lw=3, alpha=17, omega=23, sig_v=0)))
legend(20, 0.3, c(' ',' '), col=c('cyan','lightpink'), lwd=10, bty='n')
legend(20, 0.3, c('In-River','Transported'), col=c('navy','deeppink'), lwd=3, bty='n')

# ftt vs. temp
# plot(0,0, xlim=c(18,22), ylim=c(5,9), ty='n',
#   xlab='Temperature (Celsius)', ylab='Travel Time (days)')
# invisible(apply(outtab_fc[r, c('b_0_fc','b_temp_fc','b_temp2_fc','b_trans_fc')], 1, plot_ftt))
# invisible(apply(cbind(38.5, 19.3, -19.9, -3.6), 1, function(x) plot_ftt(x, lcol=c('blue','deeppink'), lw=2)))

# simulate travel time with posterior distributions ----
vel_pre_func<- function(bx){
  bx[1]+ bx[2]*bx[7]+ bx[3]*bx[8]+ bx[4]*bx[9]+ bx[5]*bx[10]+ bx[6]*bx[11]
}
niter<- 100
trans<- ifelse(fcs$mig_his=='trans',1,0)
covariates<- cbind(fcs[,c('jul_sca','temp_sca','temp2','dis_sca')], trans)
ftt_pre<- matrix(NA, nrow=nrow(covariates), ncol=niter)
for (i in 1:niter){
  r<- sample(1:40000, size=1)
  betty<- matrix(outtab_fc[r,1:6], ncol=6, nrow=nrow(covariates), byrow=TRUE)
  mu<- apply(cbind(betty, covariates), 1, vel_pre_func)
  ftt_pre[,i]<- 236/apply(cbind(mu), 1, function(x) rnorm(1, x, 10))
}
# plotting distribution of travel time and predicted travel time
library(plotrix)
windows()
par(mfrow=c(2,1))
hist(fcs$ftt, main='Travel Time from Survived Only', xlab='Days',
  freq=FALSE, breaks=seq(0, ceiling(max(fcs$ftt, na.rm=TRUE)), by=1))

hist(ftt_pre[ftt_pre>0], main='Posterior Predicted Travel Time',
  xlab='Days', freq=FALSE,
  breaks=seq(0, ceiling(max(ftt_pre)), by=1),
  xlim=c(0, ceiling(max(fcs$ftt, na.rm=TRUE))), xaxt='n')
axis(1, at= seq(0,ceiling(max(fcs$ftt, na.rm=TRUE)), by=10),
  labels= c(seq(0,ceiling(max(fcs$ftt, na.rm=TRUE))-10, by=10),
    round(max(ftt_pre),-2)))
axis.break(1,65, style='slash') 

m<- rep(NA, 100)
for (i in 1:100){
  m[i]<- mean(fcs[ftt_pre[,i]>19&ftt_pre[,i]<20,]$mca_det)
}
mean(m, na.rm=TRUE)
m
# ----


## low snake survivals vs. temperature
# with GLM ----
cat('
model{
  
  for (i in 1:n_ind){
    # GLM
    det[i]~ dbern(phi[i])
    logit(phi[i])<- b_0+ b_juld*juld[i]+ b_temp*temp[i]+ b_ftt*ftt[i]+
      w*b_txf*temp[i]*ftt[i]+ b_dis*dis[i]+ b_trans*trans[i]+ a_yr[yr[i]]
    logit(phi_rep[i])<- b_0+ b_juld*juld[i]+ b_temp*temp[i]+ b_ftt*ftt[i]+
      w*b_txf*temp[i]*ftt[i]+ b_dis*dis[i]+ b_trans*trans[i]+ a_yr[yr[i]]

    # FTT
    ftt[i]<- (225/vel[i]- 7.88)/ 4.29 # scaled ftt 
    vel[i]~ dnorm(mu_v, tau_v)T(0,)
  }

  # priors
  # GLM
  b_0~ dt(0, 0.1, 1)
  b_juld~ dt(0, 0.4, 1)
  b_temp~ dt(0, 0.4, 1)
  b_ftt~ dt(0, 0.4, 1)
  b_txf~ dt(0, 0.4, 1)
  b_dis~ dt(0, 0.4, 1)
  b_trans~ dt(0, 0.4, 1)
  for (j in 1:15){
    a_yr[j]~ dnorm(0, tau_yr)
  }
  sigma_yr~ dt(0, 0.444, 1)T(0,5)
  tau_yr<- pow(sigma_yr, -2)
  w~ dbern(0.5)

  # FTT
  mu_v~ dt(0, 0.1, 1)T(0,)
  sigma_v~ dt(0, 0.444, 1)T(0,)
  tau_v<- pow(sigma_v, -2)

  # posterior predictive check
  phi_fit<- mean(phi_rep)

}', fill=TRUE, file = paste0(wd,'fall_chinooka/fc_im/fc_im_glm.txt'))
# ----
require(jagsUI)

im_data<- prep_dat(fcs, typ='glm')
str(im_data)

# run JAGS ----
parameters <- c('b_0','b_juld','b_temp','b_ftt','b_txf','b_dis','b_trans','a_yr','sigma_yr','w','mu_v','sigma_v', 'phi_fit')
inits<- function() {list(b_0=runif(1,-1,1), b_juld=runif(1,-1,1), b_temp=runif(1,-1,1), b_ftt=runif(1,-1,1), b_txf=runif(1,-1,1), b_dis=runif(1,-1,1), b_trans=runif(1,-1,1), sigma_yr=runif(1,0,2), w=rbinom(1,0,1), mu_v=runif(1,1,8), sigma_v=runif(1,0,2) )}

nc<- 4   ;   ni<- 100   ;   nb<- 0   ;   nt<- 1 # test run
# nc<- 4   ;   ni<- 2000   ;   nb<- 1000   ;   nt<- 1 # test run2
# nc<- 4   ;   ni<- 80000   ;   nb<-10000   ;   nt<- 7

imglm_out<- jags(im_data, inits, parameters, "fall_chinooka/fc_im/fc_im_glm.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)
# im_out<- autojags(im_data, inits, parameters, "fall_chinooka/fc_im/fc_im.txt", n.thin=nt, n.chains=nc, n.burnin=2000, iter.increment=5000, max.iter=42000, parallel=TRUE)
imglm_out<- update(imglm_out, parameters.to.save=parameters, n.thin=1, n.chains=4, n.iter=5000, parallel=TRUE)
print(imglm_out)

im_sims_fc<- imglm_out$sims.list
im_rhat_fc<- cbind(unlist(imglm_out$Rhat)[!is.na(unlist(imglm_out$Rhat))], unlist(imglm_out$n.eff)[unlist(imglm_out$n.eff)>1])
# JAGS results saved as as a big table
write.table(im_sims_fc, file='fall_chinooka/fc_im/im_glm_sims_fc.txt')
write.table(im_rhat_fc, file='fall_chinooka/fc_im/im_glm_rhat_fc.txt')
# ----

im_sims_fc<- read.table('fall_chinooka/fc_im/im_glm_sims_fc.txt')
im_rhat_fc<- read.table('fall_chinooka/fc_im/im_glm_rhat_fc.txt')

b_0_fc=im_sims_fc$b_0; b_juld_fc=im_sims_fc$b_juld; b_temp_fc=im_sims_fc$b_temp; b_ftt_fc=im_sims_fc$b_ftt; b_txf_fc=im_sims_fc$b_txf; b_dis_fc=im_sims_fc$b_dis; b_trans_fc=im_sims_fc$b_trans; sigma_yr_fc=im_sims_fc$sigma_yr; mu_v_fc=im_sims_fc$mu_v; sigma_v_fc=im_sims_fc$sigma_v; devi_fc=im_sims_fc$deviance

a_yr_fc<- 3:17
for(i in 1:15){
  a_yr_fc[i]= im_sims_fc[7+i]
}
ayrs_fc<- data.frame(cbind(unlist(a_yr_fc[1]), unlist(a_yr_fc[2]), unlist(a_yr_fc[3]), unlist(a_yr_fc[4]), unlist(a_yr_fc[5]), unlist(a_yr_fc[6]), unlist(a_yr_fc[7]), unlist(a_yr_fc[8]), unlist(a_yr_fc[9]), unlist(a_yr_fc[10]), unlist(a_yr_fc[11]), unlist(a_yr_fc[12]), unlist(a_yr_fc[13]), unlist(a_yr_fc[14]), unlist(a_yr_fc[15]) ))

# output table ----
outtab_fc<- cbind(b_0_fc, b_juld_fc, b_temp_fc, b_ftt_fc, b_txf_fc, b_dis_fc, b_trans_fc, ayrs_fc[,1], ayrs_fc[,2], ayrs_fc[,3], ayrs_fc[,4], ayrs_fc[,5], ayrs_fc[,6], ayrs_fc[,7], ayrs_fc[,8], ayrs_fc[,9], ayrs_fc[,10], ayrs_fc[,11], ayrs_fc[,12], ayrs_fc[,13], ayrs_fc[,14], ayrs_fc[,15], sigma_yr_fc, mu_v_fc, sigma_v_fc, devi_fc)

im_mean_fc<- cbind(colMeans(outtab_fc))
im_se_fc<- cbind(apply(outtab_fc, 2, sd))
im_cri_fc<- apply(outtab_fc, 2, function(x) quantile(x, c(0.025,0.975)))

summ_fc<- data.frame(cbind(round(im_mean_fc,3), round(im_se_fc,3), paste0('(', round(im_cri_fc[1,], 3),', ', round(im_cri_fc[2,], 3),')'), round(im_rhat_fc, 3)))
row.names(summ_fc)<- c('(Intercept)', 'Arrival Date', 'Temperature', 'Travel Time', 'Temperature$\\cdot$Travel Time', 'Flow', 'Transported','Year 2003','Year 2004','Year 2005','Year 2006','Year 2007','Year 2008','Year 2009','Year 2010','Year 2011','Year 2012','Year 2013','Year 2014','Year 2015','Year 2016','Year 2017', '$\\sigma_{year}$', '$\\mu_{vel}$', '$\\sigma_{vel}$', 'Deviance')
colnames(summ_fc)<- c('Mean','SD','95% CRI','$\\hat{R}$','Eff size')
summ_fc

# traceplot ----
windows(8,9)
par(mfrow=c(4,2))
names(outtab_fc)<- c('b0 (Intercept)', 'b Arrival', 'b Temp', 'b Ftt', 'b Temp.Ftt', 'b Flow', 'b Trans', 'Year2003','Year2004','Year2005','Year2006','Year2007','Year2008','Year2009','Year2010','Year2011','Year2012','Year2013','Year2014','Year2015','Year2016','Year2017', 'SigmaYr', 'MuVel', 'SigmaVel', 'Deviance')

# ncol(outtab_fc)
pn1<- 1:4; pn2<- 5:8; pn3<- 9:12; pn4<- 13:16; pn5<- 17:20; pn6<- 21:24; pn7<- 25:26
for(i in pn2){
  plot_pds(outtab_fc[,i], lab=names(outtab_fc)[i], colr='grey70')
}



# simulated ftt vs. observed ftt
hist(fcs$ftt, breaks=30, freq= F, col=rgb(0,0,0,0.8))
hist(ftt_sim, breaks=150, freq= F, col=rgb(1,1,1,.5), add= T)


# plotting survival relationships ----
pn<- nrow(outtab_fc)
nsim<- 1000
r<- sample(1:pn, nsim)
windows(10,4)
par(mfrow=c(1,2))
par(mar=c(5,4,2,2)+0.1) # original 5,4,4,2
# with temperature ----
# quantile(sos$ihr_temp, c(0.025,0.975), na.rm=TRUE)
plot(0,0, xlim=c(13,23), ylim=c(0,1), ty='n',
  xlab='Temperature (Celsius)', ylab='Conversion')
invisible(apply(outtab_fc[r, c('b_0_fc','b_temp_fc','b_trans_fc')], 1, function(x) surv_temp(x, lcol=c('grey80','grey90')) ))
invisible(apply(rbind(colMeans(outtab_fc[r, c('b_0_fc','b_temp_fc','b_trans_fc')])), 1, function(x) surv_temp(x, lcol=c('grey60','grey70'), lw=3)))
invisible(apply(outtab_fc[r, c('b_0_fc','b_temp_fc','b_trans_fc')], 1, function(x) surv_temp(x, alpha=14, omega=22)))
invisible(apply(rbind(colMeans(outtab_fc[r, c('b_0_fc','b_temp_fc','b_trans_fc')])), 1, function(x) surv_temp(x, alpha=14, omega=22, lcol=c('navy','deeppink'), lw=3)))
legend(18, 0.4, c(' ',' '), col=c('cyan','lightpink'), lwd=10, bty='n')
legend(18, 0.4, c('In-River','Transported'), col=c('navy','deeppink'), lwd=3, bty='n')

# fcs$tempbin<- cut(fcs$ihr_temp, breaks=c(10,15,17,19,21,23))
# points(seq(15,23, by=2), tapply(fcs$gra_det, fcs$tempbin, mean), pch=20, cex=2)

# with ftt ----
# quantile(sos$ftt, c(0.025,0.975), na.rm=TRUE)
plot(0,0, xlim=c(0,50), ylim=c(0,1), ty='n',
  xlab='Travel Time (Days)', ylab='Conversion')
invisible(apply(outtab_fc[r, c('b_0_fc','b_temp_fc','b_ftt_fc','b_txf_fc','b_trans_fc')], 1, function(x) surv_ftt(x, lcol=c('grey80','grey90')) ))
invisible(apply(rbind(colMeans(outtab_fc[r, c('b_0_fc','b_temp_fc','b_ftt_fc','b_txf_fc','b_trans_fc')])), 1, function(x) surv_ftt(x, lcol=c('grey60','grey70'), lw=3)))
invisible(apply(outtab_fc[r, c('b_0_fc','b_temp_fc','b_ftt_fc','b_txf_fc','b_trans_fc')], 1, function(x) surv_ftt(x, alpha=4, omega=25)))
invisible(apply(rbind(colMeans(outtab_fc[r, c('b_0_fc','b_temp_fc','b_ftt_fc','b_txf_fc','b_trans_fc')])), 1, function(x) surv_ftt(x, alpha=4, omega=25, lcol=c('navy','deeppink'), lw=3)))
legend(25, 0.4, c(' ',' '), col=c('cyan','lightpink'), lwd=10, bty='n')
legend(25, 0.4, c('In-River','Transported'), col=c('navy','deeppink'), lwd=3, bty='n')
# ----






# CJS ----
cat('
model{
  
  for (i in 1:n_ind){
    z[i,1]<- 1
    for (t in 2:n_occ) {
      z[i,t]~ dbern(phi[i,t-1]* z[i,t-1])
      y[i,t]~ dbern(p[t-1]* z[i,t])
    }
    phi[i,1]<- phi1
    logit(phi[i,2])<- b_0+ b_juld*juld[i]+ b_temp*temp[i]+
      b_dis*dis[i]+ b_trans*trans[i]+ w1*a_yr[yr[i]]
    logit(phi2_rep[i])<- b_0+ b_juld*juld[i]+ b_temp*temp[i]+
      b_dis*dis[i]+ b_trans*trans[i]+ w1*a_yr[yr[i]] # posterior predictive
  }

  # priors
  phi1~ dunif(0.5, 1) # s(mcn to ihr) * p(ihr) 
  p[1]~ dunif(0.5, 1) # p2 or p(ihr)
  p[2]<- 1 # p3 or p(lgr)

  b_0~ dt(0, 0.1, 1)
  b_juld~ dt(0, 0.4, 1)
  b_temp~ dt(0, 0.4, 1)
  b_dis~ dt(0, 0.4, 1)
  b_trans~ dt(0, 0.4, 1)
  sigma_yr~ dt(0, 0.444, 1)T(0,5)
  tau_yr<- pow(sigma_yr, -2)
  for (j in 1:15){
    a_yr[j]~ dnorm(0, tau_yr)
  }
  w1~ dbern(0.5)

  # obs and fit
  obs<- mean(z[,3])
  fit<- mean(phi2_rep)

}', fill=TRUE, file = paste0(wd,'fall_chinooka/fc_im/fc_ls_cjs.txt'))
# ----

# run JAGS ----
require(jagsUI)

cjs_data<- prep_dat(fcs, typ='w/cjs')
str(cjs_data)

parameters <- c('b_0','b_juld','b_temp','b_dis','b_trans','a_yr','sigma_yr','p','phi1','w1','obs','fit')
inits<- function() {list(b_0=runif(1,-1,1), b_juld=runif(1,-1,1), b_temp=runif(1,-1,1), b_dis=runif(1,-1,1), b_trans=runif(1,-1,1), sigma_yr=runif(1,0,2), p.1=runif(1,0.5,1), phi1=runif(1,0.5,1), w1=rbinom(1,0,1) )}

# nc<- 4   ;   ni<- 60   ;   nb<- 10   ;   nt<- 1 # test run dis bitch
nc<- 4   ;   ni<- 80000   ;   nb<-40000   ;   nt<- 4

cjs_out<- jags(cjs_data, inits, parameters, "fall_chinooka/fc_im/fc_ls_cjs.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)
# cjs_out<- autojags(cjs_data, inits, parameters, "fall_chinooka/fc_im/fc_ls_cjs.txt", n.thin=nt, n.chains=nc, n.burnin=nb, iter.increment=50, max.iter=100, parallel=TRUE)
# cjs_out<- update(cjs_out, parameters.to.save=parameters, n.thin=8, n.chains=4, n.iter=8000, parallel=TRUE)
print(cjs_out)

# trim more data
# library('coda')
# b_temp2_nu= cjs_out$sims.list$b_temp2
# b_temp2_nu<- as.data.frame(cbind(b_temp2_nu[1:5000],b_temp2_nu[5001:10000],
#   b_temp2_nu[10001:15000],b_temp2_nu[15001:20000]))
# mcmc_object<- as.mcmc.list(lapply(b_temp2_nu, mcmc))
# niter(mcmc_object)
# windowed_object <- window(mcmc_object, start=2501)
# summary(windowed_object)
# gelman.diag(windowed_object)

cjs_sims_fc<- cjs_out$sims.list
cjs_rhat_fc<- cbind(unlist(cjs_out$Rhat)[!is.na(unlist(cjs_out$Rhat))], unlist(cjs_out$n.eff)[unlist(cjs_out$n.eff)>1])
# JAGS results saved as as a big table
write.table(cjs_sims_fc, file='fall_chinooka/fc_im/cjs_sims_fc.txt')
write.table(cjs_rhat_fc, file='fall_chinooka/fc_im/cjs_rhat_fc.txt')

# convert saved cjs results into vectors ----
cjs_sims_fc<- read.table('fall_chinooka/fc_im/cjs_sims_fc.txt')
cjs_rhat_fc<- read.table('fall_chinooka/fc_im/cjs_rhat_fc.txt')

b_0_fc=cjs_sims_fc$b_0; b_juld_fc=cjs_sims_fc$b_juld; b_juld2_fc=cjs_sims_fc$b_juld2; b_temp_fc=cjs_sims_fc$b_temp; b_temp2_fc=cjs_sims_fc$b_temp2; b_dis_fc=cjs_sims_fc$b_dis; b_trans_fc=cjs_sims_fc$b_trans; sigma_yr_fc=cjs_sims_fc$sigma_yr; ihr_p_fc=cjs_sims_fc$p.1; mcn_phi_fc=cjs_sims_fc$phi1; w1_fc=cjs_sims_fc$w1; w2_fc=cjs_sims_fc$w2; phi2_fit_fc=cjs_sims_fc$fit; devi_fc=cjs_sims_fc$deviance

a_yr_fc<- 3:17
for(i in 1:15){
  a_yr_fc[i]= cjs_sims_fc[7+i]
}
ayrs_fc<- data.frame(cbind(unlist(a_yr_fc[1]), unlist(a_yr_fc[2]), unlist(a_yr_fc[3]), unlist(a_yr_fc[4]), unlist(a_yr_fc[5]), unlist(a_yr_fc[6]), unlist(a_yr_fc[7]), unlist(a_yr_fc[8]), unlist(a_yr_fc[9]), unlist(a_yr_fc[10]), unlist(a_yr_fc[11]), unlist(a_yr_fc[12]), unlist(a_yr_fc[13]), unlist(a_yr_fc[14]), unlist(a_yr_fc[15]) ))

# output table ----
outtab_fc<- cbind(b_0_fc, b_juld_fc, b_juld2_fc, b_temp_fc, b_temp2_fc, b_dis_fc, b_trans_fc, ayrs_fc[,1], ayrs_fc[,2], ayrs_fc[,3], ayrs_fc[,4], ayrs_fc[,5], ayrs_fc[,6], ayrs_fc[,7], ayrs_fc[,8], ayrs_fc[,9], ayrs_fc[,10], ayrs_fc[,11], ayrs_fc[,12], ayrs_fc[,13], ayrs_fc[,14], ayrs_fc[,15], sigma_yr_fc, ihr_p_fc, mcn_phi_fc, w1_fc, w2_fc, phi2_fit_fc, devi_fc)
cjs_mean_fc<- cbind(colMeans(outtab_fc))
cjs_se_fc<- cbind(apply(outtab_fc, 2, sd))
cjs_cri_fc<- apply(outtab_fc, 2, function(x) quantile(x, c(0.025,0.975)))

summ_fc<- data.frame(cbind(round(cjs_mean_fc,3), round(cjs_se_fc,3), paste0('(', round(cjs_cri_fc[1,], 3),', ', round(cjs_cri_fc[2,], 3),')'), round(cjs_rhat_fc, 3)))
row.names(summ_fc)<- c('(Intercept)', 'Arrival Date', 'Arrival2', 'Temperature', 'Temperature2', 'Flow', 'Transported','Year 2003','Year 2004','Year 2005','Year 2006','Year 2007','Year 2008','Year 2009','Year 2010','Year 2011','Year 2012','Year 2013','Year 2014','Year 2015','Year 2016','Year 2017', '$\\sigma_{year}$', 'Detection (IHR)', 'Survival (McN)', 'w1', 'w2', '$\\phi_2$fit', 'Deviance')
colnames(summ_fc)<- c('Mean','SD','95% CRI','$\\hat{R}$','Eff size')
summ_fc

# traceplot ----
windows(8,9)
par(mfrow=c(4,2))
names(outtab_fc)<- c('b0 (Intercept)', 'b Arrival', 'b Arrival2', 'b Temp', 'b Temp2', 'b Flow', 'b Trans', 'Year2003','Year2004','Year2005','Year2006','Year2007','Year2008','Year2009','Year2010','Year2011','Year2012','Year2013','Year2014','Year2015','Year2016','Year2017', 'SigmaYr', 'IHR Detect', 'McN Survival', 'W1', 'w2', 'Phi2 Fit', 'Deviance')

# ncol(outtab_fc)
pn1<- 1:4; pn2<- 5:8; pn3<- 9:12; pn4<- 13:16; pn5<- 17:20; pn6<- 21:24; pn7<- 25:28; pn8<- 29
for(i in pn1){
  plot_pds(outtab_fc[,i], lab=names(outtab_fc)[i], colr='grey70')
}


# posterior predictive check ----
phi2_obs_fc= cjs_sim_fc$obs
mean(phi2_fit_fc > phi2_obs_fc)
with(cjs_out$sims.list, hist(fit, xlab="Predicted Phi2*p3",
  breaks=30, xlim=c(0.92,0.97)))
with(cjs_out$sims.list, abline(v=phi2_obs_fc[1], lwd=2, col="red")) 






