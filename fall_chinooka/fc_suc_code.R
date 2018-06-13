
# load/format data
rm(list=ls())
wd<- 'G:/STAFF/Bobby/css/adult_still_suc_2018/'
source(file=paste0(wd, "fall_chinooka/fc_suc_util.R")) # where the functions are
fcs<- fc_load_dat(wd)
#
# im ----
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
    ftt[i]<- 236/vel[i]
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

}', fill=TRUE, file = paste0(wd,'fall_chinooka/fc_im/fc_im.txt'))
# ----
require(jagsUI)

im_data<- prep_dat(fcs, typ='qua')
str(im_data)

# quadratic
parameters <- c('b_0','b_juld','b_temp','b_temp2','b_dis','b_trans','a_yr','p','phi2p3','g_0','g_1','sigma_yr','w')
inits<- function() {list(z=cjs_init_z(im_data$y), b_0=runif(1,-1,1), b_juld=runif(1,-1,1), b_temp=runif(1,-1,1), b_temp2=runif(1,-1,1), b_dis=runif(1,-1,1), b_trans=runif(1,-1,1), p.1=runif(1,0.5,1), phi2p3=runif(1,0.5,1), g_0=runif(1,0.5,1), g_1=runif(1,0,0.1), sigma_yr=runif(1,0,2), w=rbinom(1,0,1))}
# linear (not doing it)

nc<- 4   ;   ni<- 100   ;   nb<- 0   ;   nt<- 1 # test run
# nc<- 4   ;   ni<- 80000   ;   nb<-10000   ;   nt<- 7

im_out<- jags(im_data, inits, parameters, "fall_chinooka/fc_im/fc_im.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)
# im_out<- autojags(im_data, inits, parameters, "fall_chinooka/fc_im/fc_im.txt", n.thin=nt, n.chains=nc, n.burnin=2000, iter.increment=5000, max.iter=42000, parallel=TRUE)
im_out<- update(im_out, parameters.to.save=parameters, n.thin=1, n.chains=4, n.iter=5000, parallel=TRUE)
print(im_out)



im_sims_fc<- im_out$sims.list
im_rhat_fc<- cbind(unlist(im_out$Rhat)[!is.na(unlist(im_out$Rhat))], unlist(im_out$n.eff)[unlist(im_out$n.eff)>1])
# JAGS results saved as as a big table
write.table(im_sims_fc, file='fall_chinooka/fc_im/im_sims2.txt')
write.table(im_rhat_fc, file='fall_chinooka/fc_im/im_rhat2.txt')



# convert saved JAGS results into vectors ----
im_sims_fc<- read.table('fall_chinooka/fc_im/im_sims2.txt')
im_rhat_fc<- read.table('fall_chinooka/fc_im/im_rhat2.txt')

b_0_fc=im_sims_fc$b_0; b_juld_fc=im_sims_fc$b_juld; b_temp_fc=im_sims_fc$b_temp; b_temp2_fc=im_sims_fc$b_temp2; b_dis_fc=im_sims_fc$b_dis; b_trans_fc=im_sims_fc$b_trans; mcn_p_fc=im_sims_fc$p.1; phi2p2_fc=im_sims_fc$phi_2; g_0_fc=im_sims_fc$g_0; g_1_fc=im_sims_fc$g_1; sigma_yr_fc=im_sims_fc$sigma_yr; sigma_v_fc=im_sims_fc$sigma_v; devi_fc=im_sims_fc$deviance


a_yr<- 3:17
for(i in 1:15){
  a_yr[i]= im_sims_fc[5+i]
}
ayrs<- data.frame(cbind(unlist(a_yr[1]), unlist(a_yr[2]), unlist(a_yr[3]), unlist(a_yr[4]), unlist(a_yr[5]), unlist(a_yr[6]), unlist(a_yr[7]), unlist(a_yr[8]), unlist(a_yr[9]), unlist(a_yr[10]), unlist(a_yr[11]), unlist(a_yr[12]), unlist(a_yr[13]), unlist(a_yr[14]), unlist(a_yr[15]) ))

# output table ----
outtab_fc<- cbind(b_0_fc, b_juld_fc, b_temp_fc, b_temp2_fc, b_dis_fc, b_trans_fc, ayrs[,1], ayrs[,2], ayrs[,3], ayrs[,4], ayrs[,5], ayrs[,6], ayrs[,7], ayrs[,8], ayrs[,9], ayrs[,10], ayrs[,11], ayrs[,12], ayrs[,13], ayrs[,14], ayrs[,15], mcn_p_fc, phi2p2_fc, g_0_fc, g_1_fc, sigma_yr_fc, sigma_v_fc, devi_fc)
im_mean_fc<- cbind(colMeans(outtab_fc))
im_se_fc<- cbind(apply(outtab_fc, 2, sd))
im_cri_fc<- apply(outtab_fc, 2, function(x) quantile(x, c(0.025,0.975)))

summ_fc<- data.frame(cbind(round(im_mean_fc,3), round(im_se_fc,3), paste0('(', round(im_cri_fc[1,], 3),', ', round(im_cri_fc[2,], 3),')'), round(im_rhat_fc, 3)))
row.names(summ_fc)<- c('(Intercept)', 'Arrival Date', 'Temperature', 'Temperature2', 'Flow', 'Transported','Year 2003','Year 2004','Year 2005','Year 2006','Year 2007','Year 2008','Year 2009','Year 2010','Year 2011','Year 2012','Year 2013','Year 2014','Year 2015','Year 2016','Year 2017', 'Detection (McN)', '$\\phi_2\\cdot p_2$', '$\\gamma_0$', '$\\gamma_1$', '$\\sigma_{year}$', '$\\sigma_{vel}$','Deviance')
colnames(summ_fc)<- c('Mean','SD','95% CRI','$\\hat{R}$','Eff size')
summ_fc

# traceplot ----
windows(8,9)
par(mfrow=c(4,2))
names(outtab_fc)<- c('b0 (Intercept)', 'b Arrival', 'b Temp', 'b Temp2', 'b Flow', 'b Trans', 'Year2003','Year2004','Year2005','Year2006','Year2007','Year2008','Year2009','Year2010','Year2011','Year2012','Year2013','Year2014','Year2015','Year2016','Year2017', 'McN Detect', 'Phi2p2', 'gamma0', 'gamma1', 'SigmaYr', 'SigmaVel', 'Deviance')

# ncol(outtab_fc)
pn1<- 1:4; pn2<- 5:8; pn3<- 9:12; pn4<- 13:16; pn5<- 17:20; pn6<- 21:24; pn7<- 25:28
for(i in pn7){
  plot_pds(outtab_fc[,i], lab=names(outtab_fc)[i], colr='grey70')
}

# surv plots ----
windows(10,4)
par(mfrow=c(1,2))
niter<- 500
r<- sample(1:40000, size=niter)
# surv vs. ftt
# quantile(fcs$ftt, c(0.025,0.975), na.rm=TRUE)
plot(0,0, xlim=c(0,80), ylim=c(0,1), ty='n',
  xlab='Travel Time (Days)', ylab='Survival')
invisible(apply(rbind(outtab_fc[r,c('g_0_fc','g_1_fc','sigma_v_fc')]), 1,
  function(x) surv_ftt(x, lcol='grey80')))
invisible(apply(cbind(mean(outtab_fc[,'g_0_fc']),mean(outtab_fc[,'g_1_fc']), 0), 1,
  function(x) surv_ftt(x, lcol='grey70',lw=3)))
invisible(apply(cbind(outtab_fc[r,c('g_0_fc','g_1_fc','sigma_v_fc')]), 1,
  function(x) surv_ftt(x, alpha=4, omega=18)))
invisible(apply(cbind(mean(outtab_fc[,'g_0_fc']),mean(outtab_fc[,'g_1_fc']),0), 1,
  function(x) surv_ftt(x, lcol='navy',lw=3, alpha=4, omega=18)))
legend(40, 0.4, '', col='grey50', lwd=10, bty='n')
legend(40, 0.4, 'Middle 95%', col='navy', lwd=3, bty='n')

#surv vs. temp
# quantile(fcs$temp, c(0.025,0.975))
plot(0,0, xlim=c(15,25), ylim=c(0,1), ty='n',
  xlab='Temperature (Celsius)', ylab='Survival')
epsilon<- rnorm(niter, 0, 10)
invisible(apply(cbind(outtab_fc[r, c('g_0_fc','g_1_fc','b_0_fc','b_temp_fc','b_temp2_fc','b_trans_fc')],epsilon), 1, function(x) surv_temp(x, lcol=c('grey80','grey90'), alpha=15, omega=25)))
invisible(apply(cbind(rbind(colMeans(outtab_fc[r, c('g_0_fc','g_1_fc','b_0_fc','b_temp_fc','b_temp2_fc','b_trans_fc')])),epsilon[1]), 1, function(x) surv_temp(x, lcol=c('grey60','grey70'), lw=3, alpha=15, omega=25)))
invisible(apply(cbind(outtab_fc[r, c('g_0_fc','g_1_fc','b_0_fc','b_temp_fc','b_temp2_fc','b_trans_fc')],epsilon), 1, function(x) surv_temp(x, alpha=17, omega=23)))
invisible(apply(cbind(rbind(colMeans(outtab_fc[r, c('g_0_fc','g_1_fc','b_0_fc','b_temp_fc','b_temp2_fc','b_trans_fc')])),epsilon[1]), 1, function(x) surv_temp(x, lcol=c('navy','deeppink'), lw=3, alpha=17, omega=23)))
legend(20, 0.3, c(' ',' '), col=c('cyan','lightpink'), lwd=10, bty='n')
legend(20, 0.3, c('In-River','Transported'), col=c('navy','deeppink'), lwd=3, bty='n')

# ftt vs. temp
# plot(0,0, xlim=c(18,22), ylim=c(5,9), ty='n',
#   xlab='Temperature (Celsius)', ylab='Travel Time (days)')
# invisible(apply(outtab_fc[r, c('b_0_fc','b_temp_fc','b_temp2_fc','b_trans_fc')], 1, plot_ftt))
# invisible(apply(cbind(38.5, 19.3, -19.9, -3.6), 1, function(x) plot_ftt(x, lcol=c('blue','deeppink'), lw=2)))

# simulate travel time with posterior distributions
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



















