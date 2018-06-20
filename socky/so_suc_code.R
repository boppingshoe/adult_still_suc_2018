
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
    logit(phi[i])<- b_0+ b_temp*temp[i]+ b_temp2*temp2[i]+
      b_ftt*ftt[i]+ b_trans*trans[i]+ a_yr[yr[i]]

    # FTT
    ftt[i]<- (225/vel[i]- 8.37)/ 7.7 # scaled ftt 
    vel[i]~ dnorm(mu_v, tau_v)T(0,)
  }

  # priors
  # GLM
  b_0~ dt(0, 0.1, 1)
  b_temp~ dt(0, 0.4, 1)
  b_temp2~ dt(0, 0.4, 1)
  b_ftt~ dt(0, 0.4, 1)
  b_trans~ dt(0, 0.4, 1)
  for (j in 1:15){
    a_yr[j]~ dnorm(0, tau_yr)
  }
  sigma_yr~ dt(0, 0.444, 1)T(0,5)
  tau_yr<- pow(sigma_yr, -2)

  # FTT
  mu_v~ dt(0, 0.1, 1)T(0,)
  sigma_v~ dt(0, 0.444, 1)T(0,15)
  tau_v<- pow(sigma_v, -2)

}', fill=TRUE, file = paste0(wd,'socky/so_im/so_im_glm.txt'))
# ----
require(jagsUI)

im_data<- prep_dat(sos)
str(im_data)

# run JAGS ----
parameters <- c('b_0','b_temp','b_temp2','b_ftt','b_trans','a_yr','sigma_yr','mu_v','sigma_v')
inits<- function() {list(b_0=runif(1,-1,1), b_temp=runif(1,-1,1), b_temp2=runif(1,-1,1), b_ftt=runif(1,-1,1), b_trans=runif(1,-1,1), sigma_yr=runif(1,0,2), mu_v=runif(1,1,8), sigma_v=runif(1,0,2) )}

# nc<- 4   ;   ni<- 100   ;   nb<- 0   ;   nt<- 1 # test run
# nc<- 4   ;   ni<- 1000   ;   nb<- 500   ;   nt<- 1 # test run2
nc<- 4   ;   ni<- 100000   ;   nb<-50000   ;   nt<- 5

imglm_out<- jags(im_data, inits, parameters, "socky/so_im/so_im_glm.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)
# im_out<- autojags(im_data, inits, parameters, "fall_chinooka/fc_im/fc_im.txt", n.thin=nt, n.chains=nc, n.burnin=2000, iter.increment=5000, max.iter=42000, parallel=TRUE)
# imglm_out<- update(imglm_out, parameters.to.save=parameters, n.thin=2, n.chains=4, n.iter=20000, parallel=TRUE)
print(imglm_out)

im_sims_so<- imglm_out$sims.list
im_rhat_so<- cbind(unlist(imglm_out$Rhat)[!is.na(unlist(imglm_out$Rhat))], unlist(imglm_out$n.eff)[unlist(imglm_out$n.eff)>1])
# JAGS results saved as as a big table
write.table(im_sims_so, file='socky/so_im/im_glm_sims_so.txt')
write.table(im_rhat_so, file='socky/so_im/im_glm_rhat_so.txt')

# convert saved JAGS results into vectors ----
im_sims_so<- read.table('socky/so_im/im_glm_sims_so.txt')
im_rhat_so<- read.table('socky/so_im/im_glm_rhat_so.txt')

b_0_so=im_sims_so$b_0; b_temp_so=im_sims_so$b_temp; b_temp2_so=im_sims_so$b_temp2; b_ftt_so=im_sims_so$b_ftt; b_trans_so=im_sims_so$b_trans; sigma_yr_so=im_sims_so$sigma_yr; mu_v_so=im_sims_so$mu_v; sigma_v_so=im_sims_so$sigma_v; devi_so=im_sims_so$deviance

a_yr_so<- 3:17
for(i in 1:15){
  a_yr_so[i]= im_sims_so[5+i]
}
ayrs_so<- data.frame(cbind(unlist(a_yr_so[1]), unlist(a_yr_so[2]), unlist(a_yr_so[3]), unlist(a_yr_so[4]), unlist(a_yr_so[5]), unlist(a_yr_so[6]), unlist(a_yr_so[7]), unlist(a_yr_so[8]), unlist(a_yr_so[9]), unlist(a_yr_so[10]), unlist(a_yr_so[11]), unlist(a_yr_so[12]), unlist(a_yr_so[13]), unlist(a_yr_so[14]), unlist(a_yr_so[15]) ))

# output table ----
outtab_so<- cbind(b_0_so, b_temp_so, b_temp2_so, b_ftt_so, b_trans_so, ayrs_so[,1], ayrs_so[,2], ayrs_so[,3], ayrs_so[,4], ayrs_so[,5], ayrs_so[,6], ayrs_so[,7], ayrs_so[,8], ayrs_so[,9], ayrs_so[,10], ayrs_so[,11], ayrs_so[,12], ayrs_so[,13], ayrs_so[,14], ayrs_so[,15], sigma_yr_so, mu_v_so, sigma_v_so, devi_so)

im_mean_so<- cbind(colMeans(outtab_so))
im_se_so<- cbind(apply(outtab_so, 2, sd))
im_cri_so<- apply(outtab_so, 2, function(x) quantile(x, c(0.025,0.975)))

summ_so<- data.frame(cbind(round(im_mean_so,3), round(im_se_so,3), paste0('(', round(im_cri_so[1,], 3),', ', round(im_cri_so[2,], 3),')'), round(im_rhat_so, 3)))
row.names(summ_so)<- c('(Intercept)', 'Temperature', 'Temp^2', 'Travel Time', 'Transported','Year 2003','Year 2004','Year 2005','Year 2006','Year 2007','Year 2008','Year 2009','Year 2010','Year 2011','Year 2012','Year 2013','Year 2014','Year 2015','Year 2016','Year 2017', '$\\sigma_{year}$', '$\\mu_{vel}$', '$\\sigma_{vel}$', 'Deviance')
colnames(summ_so)<- c('Mean','SD','95% CRI','$\\hat{R}$','Eff size')
summ_so

# traceplot ----
windows(8,9)
par(mfrow=c(4,2))
names(outtab_so)<- c('b0 (Intercept)', 'b Temp', 'b Temp^2', 'b Ftt', 'b Trans', 'Year2003','Year2004','Year2005','Year2006','Year2007','Year2008','Year2009','Year2010','Year2011','Year2012','Year2013','Year2014','Year2015','Year2016','Year2017', 'SigmaYr', 'MuVel', 'SigmaVel', 'Deviance')

# ncol(outtab_so)
pn1<- 1:4; pn2<- 5:8; pn3<- 9:12; pn4<- 13:16; pn5<- 17:20; pn6<- 21:24#; pn7<- 25
for(i in pn6){
  plot_pds(outtab_so[,i], lab=names(outtab_so)[i], colr='grey70')
}


# simulated ftt vs. observed ftt ----
# overlapping ----
vel_pre<- rnorm(nrow(sos), xxxx, xxxx)
vel_pre<- vel_pre[vel_pre> 0]
ftt_pre<- 225/vel_pre

hist(sos$ftt, breaks=50, freq= F, col=rgb(0,0,0,0.8))
hist(ftt_pre[ftt_pre<200], breaks=100, freq= F, col=rgb(1,1,1,.5), add= T)

# stacked ----
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

# with ftt ----
# quantile(sos$ftt, c(0.025,0.975), na.rm=TRUE)
plot(0,0, xlim=c(4,50), ylim=c(0,1), ty='n',
  xlab='Travel Time (Days)', ylab='Conversion')
invisible(apply(outtab_so[r, c('b_0_so','b_temp2_so','b_ftt_so','b_trans_so')], 1, function(x) surv_ftt(x, lcol=c('grey80','grey90')) ))
invisible(apply(rbind(colMeans(outtab_so[r, c('b_0_so','b_temp2_so','b_ftt_so','b_trans_so')])), 1, function(x) surv_ftt(x, lcol=c('grey60','grey70'), lw=3)))
invisible(apply(outtab_so[r, c('b_0_so','b_temp2_so','b_ftt_so','b_trans_so')], 1, function(x) surv_ftt(x, alpha=4, omega=25)))
invisible(apply(rbind(colMeans(outtab_so[r, c('b_0_so','b_temp2_so','b_ftt_so','b_trans_so')])), 1, function(x) surv_ftt(x, alpha=4, omega=25, lcol=c('navy','deeppink'), lw=3)))
legend(10, 0.4, c(' ',' '), col=c('cyan','lightpink'), lwd=10, bty='n')
legend(10, 0.4, c('In-River','Transported'), col=c('navy','deeppink'), lwd=3, bty='n')
































