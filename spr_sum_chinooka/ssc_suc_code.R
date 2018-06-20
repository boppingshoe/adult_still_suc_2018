
# load/format data
rm(list=ls())
wd<- 'G:/STAFF/Bobby/css/adult_still_suc_2018/'
source(file=paste0(wd, "spr_sum_chinooka/ssc_suc_util.R")) # where the functions are
sscs<- ssc_load_dat(wd)

## low snake survivals vs. temperature
# intergrated GLM ----
cat('
model{
  
  for (i in 1:n_ind){
    # GLM
    det[i]~ dbern(phi[i])
    logit(phi[i])<- b_0+ b_run*summer[i]+ b_temp*temp[i]+ b_temp2*temp2[i]+
      b_ftt*ftt[i]+ b_dis*dis[i]+ b_trans*trans[i]+ a_yr[yr[i]]

    # FTT
    ftt[i]<- (225/vel[i]- 8.95)/ 6.36 # scaled ftt 
    vel[i]~ dnorm(mu_v, tau_v)T(0,)
  }

  # priors
  # GLM
  b_0~ dt(0, 0.1, 1)
  b_run~ dt(0, 0.4, 1)
  b_temp~ dt(0, 0.4, 1)
  b_temp2~ dt(0, 0.4, 1)
  b_ftt~ dt(0, 0.4, 1)
  b_dis~ dt(0, 0.4, 1)
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

}', fill=TRUE, file = paste0(wd,'spr_sum_chinooka/ssc_im/ssc_im_glm.txt'))
# ----
require(jagsUI)

im_data<- prep_dat(sscs)
str(im_data)

# run JAGS ----
parameters <- c('b_0','b_run','b_temp','b_temp2','b_ftt','b_dis','b_trans','a_yr','sigma_yr','mu_v','sigma_v')
inits<- function() {list(b_0=runif(1,-1,1), b_run=runif(1,-1,1), b_temp=runif(1,-1,1), b_temp2=runif(1,-1,1), b_ftt=runif(1,-1,1), b_dis=runif(1,-1,1), b_trans=runif(1,-1,1), sigma_yr=runif(1,0,2), mu_v=runif(1,1,8), sigma_v=runif(1,0,2) )}

# nc<- 4   ;   ni<- 100   ;   nb<- 0   ;   nt<- 1 # test run
# nc<- 4   ;   ni<- 1000   ;   nb<- 500   ;   nt<- 1 # test run2
nc<- 4   ;   ni<- 20000   ;   nb<-10000   ;   nt<- 2

imglm_out<- jags(im_data, inits, parameters, "spr_sum_chinooka/ssc_im/ssc_im_glm.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)
# im_out<- autojags(im_data, inits, parameters, "fall_chinooka/fc_im/fc_im.txt", n.thin=nt, n.chains=nc, n.burnin=2000, iter.increment=5000, max.iter=42000, parallel=TRUE)
# imglm_out<- update(imglm_out, parameters.to.save=parameters, n.thin=1, n.chains=4, n.iter=5000, parallel=TRUE)
print(imglm_out)

im_sims_ssc<- imglm_out$sims.list
im_rhat_ssc<- cbind(unlist(imglm_out$Rhat)[!is.na(unlist(imglm_out$Rhat))], unlist(imglm_out$n.eff)[unlist(imglm_out$n.eff)>1])
# JAGS results saved as as a big table
write.table(im_sims_ssc, file='spr_sum_chinooka/ssc_im/im_glm_sims_ssc.txt')
write.table(im_rhat_ssc, file='spr_sum_chinooka/ssc_im/im_glm_rhat_ssc.txt')

# convert saved JAGS results into vectors ----
im_sims_ssc<- read.table('spr_sum_chinooka/ssc_im/im_glm_sims_ssc.txt')
im_rhat_ssc<- read.table('spr_sum_chinooka/ssc_im/im_glm_rhat_ssc.txt')

b_0_ssc=im_sims_ssc$b_0; b_run_ssc=im_sims_ssc$b_run; b_temp_ssc=im_sims_ssc$b_temp; b_temp2_ssc=im_sims_ssc$b_temp2; b_ftt_ssc=im_sims_ssc$b_ftt; b_dis_ssc=im_sims_ssc$b_dis; b_trans_ssc=im_sims_ssc$b_trans; sigma_yr_ssc=im_sims_ssc$sigma_yr; mu_v_ssc=im_sims_ssc$mu_v; sigma_v_ssc=im_sims_ssc$sigma_v; devi_ssc=im_sims_ssc$deviance

a_yr_ssc<- 3:17
for(i in 1:15){
  a_yr_ssc[i]= im_sims_ssc[7+i]
}
ayrs_ssc<- data.frame(cbind(unlist(a_yr_ssc[1]), unlist(a_yr_ssc[2]), unlist(a_yr_ssc[3]), unlist(a_yr_ssc[4]), unlist(a_yr_ssc[5]), unlist(a_yr_ssc[6]), unlist(a_yr_ssc[7]), unlist(a_yr_ssc[8]), unlist(a_yr_ssc[9]), unlist(a_yr_ssc[10]), unlist(a_yr_ssc[11]), unlist(a_yr_ssc[12]), unlist(a_yr_ssc[13]), unlist(a_yr_ssc[14]), unlist(a_yr_ssc[15]) ))

# output table ----
outtab_ssc<- cbind(b_0_ssc, b_run_ssc, b_temp_ssc, b_temp2_ssc, b_ftt_ssc, b_dis_ssc, b_trans_ssc, ayrs_ssc[,1], ayrs_ssc[,2], ayrs_ssc[,3], ayrs_ssc[,4], ayrs_ssc[,5], ayrs_ssc[,6], ayrs_ssc[,7], ayrs_ssc[,8], ayrs_ssc[,9], ayrs_ssc[,10], ayrs_ssc[,11], ayrs_ssc[,12], ayrs_ssc[,13], ayrs_ssc[,14], ayrs_ssc[,15], sigma_yr_ssc, mu_v_ssc, sigma_v_ssc, devi_ssc)

im_mean_ssc<- cbind(colMeans(outtab_ssc))
im_se_ssc<- cbind(apply(outtab_ssc, 2, sd))
im_cri_ssc<- apply(outtab_ssc, 2, function(x) quantile(x, c(0.025,0.975)))

summ_ssc<- data.frame(cbind(round(im_mean_ssc,3), round(im_se_ssc,3), paste0('(', round(im_cri_ssc[1,], 3),', ', round(im_cri_ssc[2,], 3),')'), round(im_rhat_ssc, 3)))
row.names(summ_ssc)<- c('(Intercept)', 'Summer Run', 'Temperature', 'Temp^2', 'Travel Time', 'Flow', 'Transported','Year 2003','Year 2004','Year 2005','Year 2006','Year 2007','Year 2008','Year 2009','Year 2010','Year 2011','Year 2012','Year 2013','Year 2014','Year 2015','Year 2016','Year 2017', '$\\sigma_{year}$', '$\\mu_{vel}$', '$\\sigma_{vel}$', 'Deviance')
colnames(summ_ssc)<- c('Mean','SD','95% CRI','$\\hat{R}$','Eff size')
summ_ssc

# traceplot ----
windows(8,9)
par(mfrow=c(4,2))
names(outtab_ssc)<- c('b0 (Intercept)', 'b RunType', 'b Temp', 'b Temp^2', 'b Ftt', 'b Flow', 'b Trans', 'Year2003','Year2004','Year2005','Year2006','Year2007','Year2008','Year2009','Year2010','Year2011','Year2012','Year2013','Year2014','Year2015','Year2016','Year2017', 'SigmaYr', 'MuVel', 'SigmaVel', 'Deviance')

# ncol(outtab_ssc)
pn1<- 1:4; pn2<- 5:8; pn3<- 9:12; pn4<- 13:16; pn5<- 17:20; pn6<- 21:24; pn7<- 25
for(i in pn7){
  plot_pds(outtab_ssc[,i], lab=names(outtab_ssc)[i], colr='grey70')
}


# simulated ftt vs. observed ftt ----
# overlapping ----
vel_pre<- rnorm(nrow(sscs), 31.182, 12.385)
vel_pre<- vel_pre[vel_pre> 0]
ftt_pre<- 225/vel_pre

hist(sscs$ftt, breaks=50, freq= F, col=rgb(0,0,0,0.8))
hist(ftt_pre[ftt_pre<200], breaks=100, freq= F, col=rgb(1,1,1,.5), add= T)

# stacked ----
library(plotrix)
windows()
par(mfrow=c(2,1))
hist(sscs$ftt, main='Travel Time from Survived Only', xlab='Days',
  freq=FALSE, breaks=seq(0, ceiling(max(sscs$ftt, na.rm=TRUE)), by=1))

hist(ftt_pre[ftt_pre>0], main='Posterior Predicted Travel Time',
  xlab='Days', freq=FALSE,
  breaks=seq(0, ceiling(max(ftt_pre)), by=1),
  xlim=c(0, ceiling(max(sscs$ftt, na.rm=TRUE))), xaxt='n')
axis(1, at= seq(0,ceiling(max(sscs$ftt, na.rm=TRUE)), by=10),
  labels= c(seq(0,ceiling(max(sscs$ftt, na.rm=TRUE))-10, by=10),
    round(max(ftt_pre),-2)))
axis.break(1,65, style='slash') 

m<- rep(NA, 100)
for (i in 1:100){
  m[i]<- mean(sscs[ftt_pre>30, ]$gra_det)
}
mean(m, na.rm=TRUE)
m

# posterior predictive (i don't think this fairly assess travel time in the model) ----
sscs$summer<- im_data$summer
sscs$ftt_pre<- sscs$ftt
sscs[is.na(sscs$ftt), c('ftt_pre')]<- 225/rnorm(sum(is.na(sscs$ftt)), 31.182, 12.385)
sscs$trans<- im_data$trans
pp_chk<- function(dat){
  plogis(x[1]+ x[2]*dat[1]+ x[3]*dat[2]+ x[4]*dat[3]+ x[5]*dat[4]+ x[6]*dat[5]+ x[7]*dat[6]+ x[dat[7]-2002+7])
}

sdff<- rep(NA, 200)
for (i in 1:200){
  x<- outtab_ssc[i,1:22]
  stra<- apply(subset(sscs, trans==1, c(summer, temp_sca, temp2, ftt_pre, dis_sca, trans, mca_yr)), 1, pp_chk)
  sriv<- apply(subset(sscs, trans==0, c(summer, temp_sca, temp2, ftt_pre, dis_sca, trans, mca_yr)), 1, pp_chk)
  sdff[i]<- mean(sriv)- mean(stra) 
}
hist(sdff, breaks=30, xlim=c(0.02, max(sdff)))
# mean(sscs[sscs$trans==0,'gra_det'])- mean(sscs[sscs$trans==1,'gra_det'])
abline(v=0.0286, lwd=3, col='red')

es<- rep(NA, 200)
for (i in 1:200){
  x<- outtab_ssc[i,1:22]
  s<- apply(subset(sscs, , c(summer, temp_sca, temp2, ftt_pre, dis_sca, trans, mca_yr)), 1, pp_chk)
  es[i]<- mean(s)
}
hist(s, breaks=30, xlim=c(0.5,1))
mean(sscs[,'gra_det'])
abline(v=0.94, lwd=3, col='red')

# plotting survival relationships ----
pn<- nrow(outtab_ssc)
nsim<- 1000
r<- sample(1:pn, nsim)
windows(10,4)
par(mfrow=c(1,2))
par(mar=c(5,4,2,2)+0.1) # original 5,4,4,2
# with temperature ----
# quantile(sscs$ihr_temp, c(0.025,0.975), na.rm=TRUE)
plot(0,0, xlim=c(8,22), ylim=c(0,1), ty='n',
  xlab='Temperature (Celsius)', ylab='Conversion')
invisible(apply(outtab_ssc[r, c('b_0_ssc','b_temp_ssc','b_temp2_ssc','b_trans_ssc')], 1, function(x) surv_temp(x, lcol=c('grey80','grey90')) ))
invisible(apply(rbind(colMeans(outtab_ssc[r, c('b_0_ssc','b_temp_ssc','b_temp2_ssc','b_trans_ssc')])), 1, function(x) surv_temp(x, lcol=c('grey60','grey70'), lw=3)))
invisible(apply(outtab_ssc[r, c('b_0_ssc','b_temp_ssc','b_temp2_ssc','b_trans_ssc')], 1, function(x) surv_temp(x, alpha=9, omega=19)))
invisible(apply(rbind(colMeans(outtab_ssc[r, c('b_0_ssc','b_temp_ssc','b_temp2_ssc','b_trans_ssc')])), 1, function(x) surv_temp(x, alpha=9, omega=19, lcol=c('navy','deeppink'), lw=3)))
legend(8, 0.5, c(' ',' '), col=c('cyan','lightpink'), lwd=10, bty='n')
legend(8, 0.5, c('In-River','Transported'), col=c('navy','deeppink'), lwd=3, bty='n')

# with ftt ----
# quantile(sscs$ftt, c(0.025,0.975), na.rm=TRUE)
plot(0,0, xlim=c(3,50), ylim=c(0,1), ty='n',
  xlab='Travel Time (Days)', ylab='Conversion')
invisible(apply(outtab_ssc[r, c('b_0_ssc','b_temp2_ssc','b_ftt_ssc','b_trans_ssc')], 1, function(x) surv_ftt(x, lcol=c('grey80','grey90')) ))
invisible(apply(rbind(colMeans(outtab_ssc[r, c('b_0_ssc','b_temp2_ssc','b_ftt_ssc','b_trans_ssc')])), 1, function(x) surv_ftt(x, lcol=c('grey60','grey70'), lw=3)))
invisible(apply(outtab_ssc[r, c('b_0_ssc','b_temp2_ssc','b_ftt_ssc','b_trans_ssc')], 1, function(x) surv_ftt(x, alpha=4, omega=27)))
invisible(apply(rbind(colMeans(outtab_ssc[r, c('b_0_ssc','b_temp2_ssc','b_ftt_ssc','b_trans_ssc')])), 1, function(x) surv_ftt(x, alpha=4, omega=27, lcol=c('navy','deeppink'), lw=3)))
legend(25, 1, c(' ',' '), col=c('cyan','lightpink'), lwd=10, bty='n')
legend(25, 1, c('In-River','Transported'), col=c('navy','deeppink'), lwd=3, bty='n')
































