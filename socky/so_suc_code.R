
# load/format data
rm(list=ls())
wd<- 'G:/STAFF/Bobby/css/adult_still_suc_2018/'
source(file=paste0(wd, "socky/so_suc_util.R")) # where the functions are
sos<- so_load_dat(wd)

## low snake survivals vs. temperature
# intergrated GLM ----
cat('
model{
  
  for (i in 1:n_ind){
    # GLM
    det[i]~ dbern(phi[i])
    logit(phi[i])<- a_yr[yr[i]]+ b_0+ b_ftt*ftt[i]+
      b_temp*temp[i]+ b_dis*dis[i]+ b_trans*trans[i]
    r_obs[i]<- det[i]- phi[i]
    det_rep[i]~ dbern(phi_rep[i])
    logit(phi_rep[i])<- a_yr[yr[i]]+ b_0+ b_ftt*ftt[i]+
      b_temp*temp[i]+ b_dis*dis[i]+ b_trans*trans[i]
    r_rep[i]<- det_rep[i]- phi_rep[i]

    # FTT
    vel[i]~ dnorm(mu_v, tau_v)T(0,)
    ftt[i]<- (225/vel[i]- 8.37)/ 7.7 # scaled ftt 
  }

  for (j in 1:8){
    a_yr[j]~ dnorm(0, tau_yr)
  }
  sigma_yr~ dt(0, 0.444, 1)T(0,10)
  tau_yr<- pow(sigma_yr, -2)

  # priors
  # GLM
  b_0~ dt(0, 0.1, 1)
  b_temp~ dt(0, 0.4, 1)
  b_dis~ dt(0, 0.4, 1)
  b_trans~ dt(0, 0.4, 1)
  b_ftt~ dt(0, 0.4, 1)

  # FTT
  mu_v~ dt(0, 0.1, 1)T(0,)
  sigma_v~ dt(0, 0.444, 1)T(0,12)
  tau_v<- pow(sigma_v, -2)

  # posterior predictive
  t_obs<- sum(r_obs)/n_ind
  t_rep<- sum(r_rep)/n_ind
  
}', fill=TRUE, file = paste0(wd,'socky/so_im/so_im_glm3.txt'))
# ----
require(jagsUI)

im_data<- prep_dat(sos)
str(im_data)

# run JAGS ----
parameters <- c('b_0','b_temp','b_dis','b_trans','b_ftt','a_yr','sigma_yr','mu_v','sigma_v','t_obs','t_rep')
inits<- function() {list(b_0=runif(1,-1,1), b_temp=runif(1,-1,1), b_temp2=runif(1,-1,1), b_trans=runif(1,-1,1), mu_v=runif(1,1,8), sigma_v=runif(1,0,2), sigma_yr=runif(1,0,2), sigma_ftt=runif(1,0,2) )}

# nc<- 4   ;   ni<- 100   ;   nb<- 0   ;   nt<- 1 # test run
# nc<- 4   ;   ni<- 10000   ;   nb<- 5000   ;   nt<- 1 # test run2
# nc<- 4   ;   ni<- 50000   ;   nb<-30000   ;   nt<- 4
nc<- 4   ;   ni<- 100000   ;   nb<-50000   ;   nt<- 10

# imglm_out<- jags(im_data, inits, parameters, "socky/so_im/so_im_glm3.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)
im_out<- autojags(im_data, inits, parameters, "socky/so_im/so_im_glm3.txt", n.thin=nt, n.chains=nc, n.burnin=nb, iter.increment=50000, max.iter=500000, parallel=TRUE)
# imglm_out<- update(imglm_out, parameters.to.save=parameters, n.thin=5, n.chains=4, n.iter=50000, parallel=TRUE)
print(imglm_out)

im_sims_so<- imglm_out$sims.list
im_rhat_so<- cbind(unlist(imglm_out$Rhat)[!is.na(unlist(imglm_out$Rhat))], unlist(imglm_out$n.eff)[unlist(imglm_out$n.eff)>1])
# JAGS results saved as as a big table
# write.table(im_sims_so, file='socky/so_im/im_glm3_sims_so.txt')
# write.table(im_rhat_so, file='socky/so_im/im_glm3_rhat_so.txt')
#####

# varying intercept with posterior predictive check (glm3)
# convert saved JAGS results into vectors ----
im_sims_so<- read.table('socky/so_im/im_glm3_sims_so.txt')
im_rhat_so<- read.table('socky/so_im/im_glm3_rhat_so.txt')

b_0_so=im_sims_so$b_0; b_temp_so=im_sims_so$b_temp; b_dis_so=im_sims_so$b_dis; b_trans_so=im_sims_so$b_trans; b_ftt_so=im_sims_so$b_ftt; sigma_yr_so=im_sims_so$sigma_yr; mu_v_so=im_sims_so$mu_v; sigma_v_so=im_sims_so$sigma_v; devi_so=im_sims_so$deviance; b_txf_so=im_sims_so$b_txf
t_obs_so=im_sims_so$t_obs; t_rep_so=im_sims_so$t_rep

a_yr_so<- 1:8
for(i in 1:8){
  a_yr_so[i]= im_sims_so[5+i]
}
ayrs_so<- data.frame(cbind(unlist(a_yr_so[1]), unlist(a_yr_so[2]), unlist(a_yr_so[3]), unlist(a_yr_so[4]), unlist(a_yr_so[5]), unlist(a_yr_so[6]), unlist(a_yr_so[7]), unlist(a_yr_so[8]) ))

outtab_so<- cbind(b_0_so, b_temp_so, b_dis_so, b_trans_so, b_ftt_so, ayrs_so[,1:8], sigma_yr_so, mu_v_so, sigma_v_so, devi_so)
# 
# output table ----
im_med_so<- cbind(apply(outtab_so, 2, median))
im_se_so<- cbind(apply(outtab_so, 2, sd))
im_cri_so<- apply(outtab_so, 2, function(x) quantile(x, c(0.025,0.975)))

summ_so<- data.frame(cbind(round(im_med_so,3), round(im_se_so,3), paste0('(', round(im_cri_so[1,], 3),', ', round(im_cri_so[2,], 3),')'), round(im_rhat_so[-c(17,18),], 3)))
row.names(summ_so)<- c('(Intercept)', 'Temperature', 'Flow', 'Transported', 'Travel Time', '<=Year 2010','Year 2011','Year 2012','Year 2013','Year 2014','Year 2015','Year 2016','Year 2017', '$\\sigma_{year}$', '$\\mu_{vel}$', '$\\sigma_{vel}$', 'Deviance')
colnames(summ_so)<- c('Mean','SD','95% CRI','$\\hat{R}$','Eff size')
summ_so
# 
# traceplot ----
windows(8,9)
par(mfrow=c(4,2))
names(outtab_so)<- c('b 0', 'b Temp', 'b Flow', 'b Trans', 'b Ftt', '<=Year2010','Year2011','Year2012','Year2013','Year2014','Year2015','Year2016','Year2017', 'SigmaYr', 'MuVel', 'SigmaVel', 'Deviance')

# ncol(outtab_so)
pn1<- 1:4; pn2<- 5:8; pn3<- 9:12; pn4<- 13:16; pn5<- 17
for(i in pn5){
  plot_pds(outtab_so[,i], lab=names(outtab_so)[i], colr='grey70')
}

# plotting survival relationships ----
pn<- nrow(outtab_so)
nsim<- 1000
r<- sample(1:pn, nsim)
windows(10,4)
par(mfrow=c(1,2))
par(mar=c(5,4,2,2)+0.1) # original 5,4,4,2
# with temperature ----
# quantile(sos$ihr_temp, c(0.025,0.975), na.rm=TRUE) # 14.61250 22.21325
plot(0,0, xlim=c(13,23), ylim=c(0,1), ty='n',
  xlab='Temperature (Celsius)', ylab='Conversion')
invisible(apply(outtab_so[r, c('b 0','b Temp','b Trans')], 1, function(x) surv_temp_so(x, lcol=c('grey80','grey90')) ))
invisible(apply(rbind(colMeans(outtab_so[r, c('b 0','b Temp','b Trans')])), 1, function(x) surv_temp_so(x, lcol=c('grey60','grey70'), lw=3)))
invisible(apply(outtab_so[r, c('b 0','b Temp','b Trans')], 1, function(x) surv_temp_so(x, alpha=14, omega=22)))
invisible(apply(rbind(colMeans(outtab_so[r, c('b 0','b Temp','b Trans')])), 1, function(x) surv_temp_so(x, alpha=14, omega=22, lcol=c('navy','deeppink'), lw=3)))
legend(14, 0.4, c(' ',' '), col=c('cyan','lightpink'), lwd=10, bty='n')
legend(14, 0.4, c('In-River','Transported'), col=c('navy','deeppink'), lwd=3, bty='n')

# with ftt ----
# quantile(sos$ftt, c(0.025,0.975), na.rm=TRUE) # 4.501189 24.761454
plot(0,0, xlim=c(3,30), ylim=c(0,1), ty='n',
  xlab='Travel Time (Days)', ylab='Conversion')
invisible(apply(outtab_so[r, c('b 0','b Temp','b Ftt')], 1, function(x) surv_ftt_so(x, lcol=c('grey80','grey90'), temp=19) ))
invisible(apply(rbind(colMeans(outtab_so[r, c('b 0','b Temp','b Ftt')])), 1, function(x) surv_ftt_so(x, lcol=c('grey60','grey70'), temp=19, lw=3)))
invisible(apply(outtab_so[r, c('b 0','b Temp','b Ftt')], 1, function(x) surv_ftt_so(x, alpha=4, omega=25, temp=19)))
invisible(apply(rbind(colMeans(outtab_so[r, c('b 0','b Temp','b Ftt')])), 1, function(x) surv_ftt_so(x, alpha=4, omega=25, lcol=c('olivedrab','turquoise4'), temp=19, lw=3)))
legend(20, 0.4, c(' ',' '), col=c('chartreuse','aquamarine'), lwd=10, bty='n')
legend(20, 0.4, c('19C','21C'), col=c('olivedrab','turquoise4'), lwd=3, bty='n')

# posterior predictive ----
plot(t_obs_so, t_rep_so, pch= 20, cex= .6, xlab="Observed Average Total Resid", ylab="Predicted Average Total Resid", xlim=c(range(t_obs_so))+c(-0.002,0.002), ylim=c(range(t_rep_so))+c(-0.002,0.002))
abline(0,1, lwd= 1, col= "red")

# travel time simulation
vel_pre_so<- rnorm(nrow(sos), 34.15, 10.22)
vel_pre_so<- vel_pre_so[vel_pre_so> 0]
ftt_pre_so<- 225/vel_pre_so

hist(sos$ftt, breaks=80, freq= F, col=rgb(0,0,0,1), border='white',
  main='Sockeye Travel Time', xlab='Days')
hist(ftt_pre_so[ftt_pre_so<100], breaks=80, freq= F, col=rgb(1,1,1,0.5), add= T)
legend(70, 0.15, c('Predicted','Observed'), pch=c(0,15), col=c(1,1), bty='n')
#####


# varying intercept only (glm)
# convert saved JAGS results into vectors ----
im_sims_so<- read.table('socky/so_im/im_glm_sims_so.txt')
im_rhat_so<- read.table('socky/so_im/im_glm_rhat_so.txt')

b_0_so=im_sims_so$b_0; b_temp_so=im_sims_so$b_temp; b_temp2_so=im_sims_so$b_temp2; b_ftt_so=im_sims_so$b_ftt; b_trans_so=im_sims_so$b_trans; sigma_yr_so=im_sims_so$sigma_yr; mu_v_so=im_sims_so$mu_v; sigma_v_so=im_sims_so$sigma_v; devi_so=im_sims_so$deviance

a_yr_so<- 1:8
for(i in 1:8){
  a_yr_so[i]= im_sims_so[7+i]
}
ayrs_so<- data.frame(cbind(unlist(a_yr_so[1]), unlist(a_yr_so[2]), unlist(a_yr_so[3]), unlist(a_yr_so[4]), unlist(a_yr_so[5]), unlist(a_yr_so[6]), unlist(a_yr_so[7]), unlist(a_yr_so[8]) ))

# output table ----
outtab_so<- cbind(b_0_so, b_temp_so, b_temp2_so, b_ftt_so, b_trans_so, mu_v_so, sigma_v_so, ayrs_so[,1], ayrs_so[,2], ayrs_so[,3], ayrs_so[,4], ayrs_so[,5], ayrs_so[,6], ayrs_so[,7], ayrs_so[,8], sigma_yr_so, devi_so)

im_mean_so<- cbind(colMeans(outtab_so))
im_se_so<- cbind(apply(outtab_so, 2, sd))
im_cri_so<- apply(outtab_so, 2, function(x) quantile(x, c(0.025,0.975)))

summ_so<- data.frame(cbind(round(im_mean_so,3), round(im_se_so,3), paste0('(', round(im_cri_so[1,], 3),', ', round(im_cri_so[2,], 3),')'), round(im_rhat_so, 3)))
row.names(summ_so)<- c('(Intercept)', 'Temperature', 'Temp^2', 'Travel Time', 'Transported', '$\\mu_{vel}$', '$\\sigma_{vel}$','<=Year 2010','Year 2011','Year 2012','Year 2013','Year 2014','Year 2015','Year 2016','Year 2017', '$\\sigma_{year}$', 'Deviance')
colnames(summ_so)<- c('Mean','SD','95% CRI','$\\hat{R}$','Eff size')
summ_so

# traceplot ----
windows(8,9)
par(mfrow=c(4,2))
names(outtab_so)<- c('b0 (Intercept)', 'b Temp', 'b Temp^2', 'b Ftt', 'b Trans', 'MuVel', 'SigmaVel','<=Year2010','Year2011','Year2012','Year2013','Year2014','Year2015','Year2016','Year2017', 'SigmaYr', 'Deviance')

# ncol(outtab_so)
pn1<- 1:4; pn2<- 5:8; pn3<- 9:12; pn4<- 13:16; pn5<- 17
for(i in pn5){
  plot_pds(outtab_so[,i], lab=names(outtab_so)[i], colr='grey70')
}
# simulated ftt vs. observed ftt ----
# overlapping
vel_pre<- rnorm(nrow(sos), xxxx, xxxx)
vel_pre<- vel_pre[vel_pre> 0]
ftt_pre<- 225/vel_pre

hist(sos$ftt, breaks=50, freq= F, col=rgb(0,0,0,0.8))
hist(ftt_pre[ftt_pre<200], breaks=100, freq= F, col=rgb(1,1,1,.5), add= T)

# stacked
library(plotrix)
windows()
par(mfrow=c(2,1))
hist(sos$ftt, main='Travel Time from Survived Only', xlab='Days',
  freq=FALSE, breaks=seq(0, ceiling(max(sos$ftt, na.rm=TRUE)), by=1))

hist(ftt_pre[ftt_pre>0], main='Posterior Predicted Travel Time',
  xlab='Days', freq=FALSE,
  breaks=seq(0, ceiling(max(ftt_pre)), by=1),
  xlim=c(0, ceiling(max(sos$ftt, na.rm=TRUE))), xaxt='n')
axis(1, at= seq(0,ceiling(max(sos$ftt, na.rm=TRUE)), by=10),
  labels= c(seq(0,ceiling(max(sos$ftt, na.rm=TRUE))-10, by=10),
    round(max(ftt_pre),-2)))
axis.break(1,65, style='slash') 

m<- rep(NA, 100)
for (i in 1:100){
  m[i]<- mean(sos[ftt_pre>30, ]$gra_det)
}
mean(m, na.rm=TRUE)
m

# posterior predictive (i don't think this fairly assess travel time in the model) ----
sos$ftt_pre<- sos$ftt
sos[is.na(sos$ftt), c('ftt_pre')]<- 225/rnorm(sum(is.na(sos$ftt)), xxx, xxx)
sos$trans<- im_data$trans
pp_chk<- function(dat){
  plogis(x[1]+ x[2]*dat[1]+ x[3]*dat[2]+ x[4]*dat[3]+ x[5]*dat[4]+ x[6]*dat[5]+ x[7]*dat[6]+ x[dat[7]-2002+7])
}

es<- rep(NA, 200)
for (i in 1:200){
  x<- outtab_so[i,1:22]
  s<- apply(subset(sos, , c(jul_sca, temp_sca, temp2, ftt_pre, dis_sca, trans, mca_yr)), 1, pp_chk)
  es[i]<- mean(s)
}
hist(s, breaks=30, xlim=c(0.5,1))
mean(sos[,'gra_det'])
abline(v=0.94, lwd=3, col='red')

# plotting survival relationships ----
pn<- nrow(outtab_so)
nsim<- 1000
r<- sample(1:pn, nsim)
windows(10,4)
par(mfrow=c(1,2))
par(mar=c(5,4,2,2)+0.1) # original 5,4,4,2
# with temperature ----
# quantile(sos$ihr_temp, c(0.025,0.975), na.rm=TRUE)
plot(0,0, xlim=c(13,23), ylim=c(0,1), ty='n',
  xlab='Temperature (Celsius)', ylab='Conversion')
invisible(apply(outtab_so[r, c('b_0_so','b_temp_so','b_temp2_so','b_trans_so')], 1, function(x) surv_temp(x, lcol=c('grey80','grey90')) ))
invisible(apply(rbind(colMeans(outtab_so[r, c('b_0_so','b_temp_so','b_temp2_so','b_trans_so')])), 1, function(x) surv_temp(x, lcol=c('grey60','grey70'), lw=3)))
invisible(apply(outtab_so[r, c('b_0_so','b_temp_so','b_temp2_so','b_trans_so')], 1, function(x) surv_temp(x, alpha=14, omega=22)))
invisible(apply(rbind(colMeans(outtab_so[r, c('b_0_so','b_temp_so','b_temp2_so','b_trans_so')])), 1, function(x) surv_temp(x, alpha=14, omega=22, lcol=c('navy','deeppink'), lw=3)))
legend(14, 0.4, c(' ',' '), col=c('cyan','lightpink'), lwd=10, bty='n')
legend(14, 0.4, c('In-River','Transported'), col=c('navy','deeppink'), lwd=3, bty='n')

# sos$tempbin<- cut(sos$ihr_temp, breaks=c(12,16,18,20,22,23))
# points(seq(14,22, by=2), tapply(sos$gra_det, sos$tempbin, mean), pch=20, cex=2)

# with ftt ----
# quantile(sos$ftt, c(0.025,0.975), na.rm=TRUE)
plot(0,0, xlim=c(0,50), ylim=c(0,1), ty='n',
  xlab='Travel Time (Days)', ylab='Conversion')
invisible(apply(outtab_so[r, c('b_0_so','b_temp_so','b_temp2_so','b_ftt_so','b_trans_so')], 1, function(x) surv_ftt(x, lcol=c('grey80','grey90')) ))
invisible(apply(rbind(colMeans(outtab_so[r, c('b_0_so','b_temp_so','b_temp2_so','b_ftt_so','b_trans_so')])), 1, function(x) surv_ftt(x, lcol=c('grey60','grey70'), lw=3)))
invisible(apply(outtab_so[r, c('b_0_so','b_temp_so','b_temp2_so','b_ftt_so','b_trans_so')], 1, function(x) surv_ftt(x, alpha=4, omega=25)))
invisible(apply(rbind(colMeans(outtab_so[r, c('b_0_so','b_temp_so','b_temp2_so','b_ftt_so','b_trans_so')])), 1, function(x) surv_ftt(x, alpha=4, omega=25, lcol=c('navy','deeppink'), lw=3)))
legend(25, 0.4, c(' ',' '), col=c('cyan','lightpink'), lwd=10, bty='n')
legend(25, 0.4, c('In-River','Transported'), col=c('navy','deeppink'), lwd=3, bty='n')
#####


# varying intercept and slope (glm2)
# convert saved JAGS results into vectors ----
im_sims_so<- read.table('socky/so_im/im_glm2_sims_so.txt')
im_rhat_so<- read.table('socky/so_im/im_glm2_rhat_so.txt')

b_temp_so=im_sims_so$b_temp; b_temp2_so=im_sims_so$b_temp2; b_trans_so=im_sims_so$b_trans; mu_v_so=im_sims_so$mu_v; sigma_v_so=im_sims_so$sigma_v; sigma_yr_so=im_sims_so$sigma_yr; sigma_ftt_so=im_sims_so$sigma_ftt; devi_so=im_sims_so$deviance

a_yr_so<- 1:8
for(i in 1:8){
  a_yr_so[i]= im_sims_so[3+i]
}
ayrs_so<- data.frame(cbind(unlist(a_yr_so[1]), unlist(a_yr_so[2]), unlist(a_yr_so[3]), unlist(a_yr_so[4]), unlist(a_yr_so[5]), unlist(a_yr_so[6]), unlist(a_yr_so[7]), unlist(a_yr_so[8]) ))

b_ftt_so<- 1:8
for(i in 1:8){
  b_ftt_so[i]= im_sims_so[11+i]
}
bftts_so<- data.frame(cbind(unlist(b_ftt_so[1]), unlist(b_ftt_so[2]), unlist(b_ftt_so[3]), unlist(b_ftt_so[4]), unlist(b_ftt_so[5]), unlist(b_ftt_so[6]), unlist(b_ftt_so[7]), unlist(b_ftt_so[8]) ))

# output table ----
outtab_so<- cbind(b_temp_so, b_temp2_so, b_trans_so, ayrs_so[,1:8], bftts_so[,1:8], mu_v_so, sigma_v_so, sigma_yr_so, sigma_ftt_so, devi_so)

im_med_so<- cbind(apply(outtab_so, 2, median))
im_se_so<- cbind(apply(outtab_so, 2, sd))
im_cri_so<- apply(outtab_so, 2, function(x) quantile(x, c(0.025,0.975)))

summ_so<- data.frame(cbind(round(im_med_so,3), round(im_se_so,3), paste0('(', round(im_cri_so[1,], 3),', ', round(im_cri_so[2,], 3),')'), round(im_rhat_so, 3)))
row.names(summ_so)<- c('Temperature', 'Temp^2', 'Transported', '<=Year 2010','Year 2011','Year 2012','Year 2013','Year 2014','Year 2015','Year 2016','Year 2017', 'FTT <=Y2010', 'FTT Y2011','FTT Y2012','FTT Y2013','FTT Y2014','FTT Y2015','FTT Y2016','FTT Y2017', '$\\mu_{vel}$', '$\\sigma_{vel}$', '$\\sigma_{year}$', '$\\sigma_{ftt}$', 'Deviance')
colnames(summ_so)<- c('Mean','SD','95% CRI','$\\hat{R}$','Eff size')
summ_so

# traceplot ----
windows(8,9)
par(mfrow=c(4,2))
names(outtab_so)<- c('b Temp', 'b Temp^2', 'b Trans','<=Year2010','Year2011','Year2012','Year2013','Year2014','Year2015','Year2016','Year2017', 'b Ftt<=2010', 'b Ftt2011', 'b Ftt2012', 'b Ftt2013', 'b Ftt2014', 'b Ftt2015', 'b Ftt2016', 'b Ftt2017', 'MuVel', 'SigmaVel', 'SigmaYr', 'SigmaFtt', 'Deviance')

# ncol(outtab_so)
pn1<- 1:4; pn2<- 5:8; pn3<- 9:12; pn4<- 13:16; pn5<- 17:20; pn6<- 21:24
for(i in pn1){
  plot_pds(outtab_so[,i], lab=names(outtab_so)[i], colr='grey70')
}

# plotting survival relationships ----
colnames(outtab_so)<- c('b Temp', 'b Temp^2', 'b Trans','<=Year2010','Year2011','Year2012','Year2013','Year2014','Year2015','Year2016','Year2017', 'b Ftt<=2010', 'b Ftt2011', 'b Ftt2012', 'b Ftt2013', 'b Ftt2014', 'b Ftt2015', 'b Ftt2016', 'b Ftt2017', 'MuVel', 'SigmaVel', 'SigmaYr', 'SigmaFtt', 'Deviance')
pn<- nrow(outtab_so)
nsim<- 1000
r<- sample(1:pn, nsim)
windows(10,4)
par(mfrow=c(1,2))
par(mar=c(5,4,2,2)+0.1) # original 5,4,4,2
# with temperature ----
# quantile(sos$ihr_temp, c(0.025,0.975), na.rm=TRUE)
plot(0,0, xlim=c(13,23), ylim=c(0,1), ty='n',
  xlab='Temperature (Celsius)', ylab='Conversion', main='2016')
invisible(apply(outtab_so[r, c('Year2016','b Temp','b Temp^2','b Trans')], 1, function(x) surv_temp(x, lcol=c('grey80','grey90')) ))
invisible(apply(rbind(colMeans(outtab_so[r, c('Year2016','b Temp','b Temp^2','b Trans')])), 1, function(x) surv_temp(x, lcol=c('grey60','grey70'), lw=3)))
invisible(apply(outtab_so[r, c('Year2016','b Temp','b Temp^2','b Trans')], 1, function(x) surv_temp(x, alpha=14, omega=22)))
invisible(apply(rbind(colMeans(outtab_so[r, c('Year2016','b Temp','b Temp^2','b Trans')])), 1, function(x) surv_temp(x, alpha=14, omega=22, lcol=c('navy','deeppink'), lw=3)))
legend(14, 0.4, c(' ',' '), col=c('cyan','lightpink'), lwd=10, bty='n')
legend(14, 0.4, c('In-River','Transported'), col=c('navy','deeppink'), lwd=3, bty='n')

# with ftt ----
# quantile(sos$ftt, c(0.025,0.975), na.rm=TRUE)
plot(0,0, xlim=c(0,50), ylim=c(0,1), ty='n',
  xlab='Travel Time (Days)', ylab='Conversion', main='2016')
invisible(apply(outtab_so[r, c('Year2016','b Temp','b Temp^2','b Ftt2016','b Trans')], 1, function(x) surv_ftt(x, lcol=c('grey80','grey90')) ))
invisible(apply(rbind(colMeans(outtab_so[r, c('Year2016','b Temp','b Temp^2','b Ftt2016','b Trans')])), 1, function(x) surv_ftt(x, lcol=c('grey60','grey70'), lw=3)))
invisible(apply(outtab_so[r, c('Year2016','b Temp','b Temp^2','b Ftt2016','b Trans')], 1, function(x) surv_ftt(x, alpha=4, omega=25)))
invisible(apply(rbind(colMeans(outtab_so[r, c('Year2016','b Temp','b Temp^2','b Ftt2016','b Trans')])), 1, function(x) surv_ftt(x, alpha=4, omega=25, lcol=c('navy','deeppink'), lw=3)))
legend(10, 0.4, c(' ',' '), col=c('cyan','lightpink'), lwd=10, bty='n')
legend(10, 0.4, c('In-River','Transported'), col=c('navy','deeppink'), lwd=3, bty='n')
#####


# mcnary passage and temp ----
plot(sos$mca_jul, sos$ihr_temp/400, pch=19, cex=0.5, ylim=c(0,0.07))
hist(sos$mca_jul, breaks=30, freq=FALSE, add=TRUE, col=rgb(0,0,0,0.5))
abline(h=20/400, lty=2)

# model selection ----
m1<- glm(gra_det~ jul_sca+ jul2+ temp_sca+ temp2+ dis_sca+ dis2+ mig_his+ mig_yr, family= binomial, data= sos)
m2<- update(m1, .~ .- mig_yr)
m3<- update(m2, .~ .- dis2)
m4<- update(m3, .~ .- temp2)
m5<- update(m4, .~ .- jul2)
m6<- update(m5, .~ .- dis_sca)
m7<- update(m6, .~ .- temp_sca)
AIC(m1,m2,m3,m4,m5,m6,m7)
# df      AIC
# m1 15 685.5861
# m2  8 722.2962
# m3  7 726.7729
# m4  6 744.0397
# m5  5 743.3874
# m6  4 749.0244
# m7  3 989.8187
(685.5861- 722.2962)/ (15-8)
# [1] -5.2443

m8<- update(m1, .~ .- dis2)
m9<- update(m8, .~ .- temp2)
m10<- update(m9, .~ .- jul2)

AIC(m1,m8,m9,m10)
# df      AIC
# m1  15 685.5861
# m8  14 684.6453
# m9  13 684.3226
# m10 12 682.4509

require(lme4)
m1<- glmer(gra_det~ jul_sca+ temp_sca+ dis_sca+ mig_his+ (1|mig_yr), family= binomial, data= sos)
m2<- update(m1, .~.- dis_sca)
m3<- update(m1, .~.- jul_sca)
m4<- update(m1, .~.- temp_sca)
AIC(m1,m2,m3,m4)
# df      AIC
# m1  6 699.0734
# m2  5 707.7490
# m3  5 697.4637
# m4  5 714.9870
summary(m3)
#####































