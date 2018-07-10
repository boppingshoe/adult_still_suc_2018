
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
    r_obs[i]<- det[i]- phi[i]
    det_rep[i]~ dbern(phi_rep[i])
    logit(phi_rep[i])<- b_0+ b_run*summer[i]+ b_temp*temp[i]+ b_temp2*temp2[i]+
      b_ftt*ftt[i]+ b_dis*dis[i]+ b_trans*trans[i]+ a_yr[yr[i]]
    r_rep[i]<- det_rep[i]- phi_rep[i]

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

  # posterior predictive
  t_obs<- sum(r_obs)/n_ind
  t_rep<- sum(r_rep)/n_ind

}', fill=TRUE, file = paste0(wd,'spr_sum_chinooka/ssc_im/ssc_im_glm3.txt'))
#####
require(jagsUI)

im_data<- prep_dat(sscs)
str(im_data)

# run JAGS ----
# parameters <- c('b_0','b_run','b_temp','b_temp2','b_ftt','b_dis','b_trans','a_yr','sigma_yr','mu_v','sigma_v')
parameters <- c('b_0','b_run','b_temp','b_temp2','b_ftt','b_dis','b_trans','a_yr','sigma_yr','mu_v','sigma_v','t_obs','t_rep')
inits<- function() {list(b_0=runif(1,-1,1), b_run=runif(1,-1,1), b_temp=runif(1,-1,1), b_temp2=runif(1,-1,1), b_ftt=runif(1,-1,1), b_dis=runif(1,-1,1), b_trans=runif(1,-1,1), sigma_yr=runif(1,0,2), mu_v=runif(1,1,8), sigma_v=runif(1,0,2) )}

# nc<- 4   ;   ni<- 100   ;   nb<- 0   ;   nt<- 1 # test run
# nc<- 4   ;   ni<- 1000   ;   nb<- 500   ;   nt<- 1 # test run2
nc<- 4   ;   ni<- 20000   ;   nb<-10000   ;   nt<- 2

imglm_out<- jags(im_data, inits, parameters, "spr_sum_chinooka/ssc_im/ssc_im_glm3.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)
# im_out<- autojags(im_data, inits, parameters, "fall_chinooka/fc_im/fc_im.txt", n.thin=nt, n.chains=nc, n.burnin=2000, iter.increment=5000, max.iter=42000, parallel=TRUE)
# imglm_out<- update(imglm_out, parameters.to.save=parameters, n.thin=1, n.chains=4, n.iter=5000, parallel=TRUE)
print(imglm_out)

im_sims_ssc<- imglm_out$sims.list
im_rhat_ssc<- cbind(unlist(imglm_out$Rhat)[!is.na(unlist(imglm_out$Rhat))], unlist(imglm_out$n.eff)[unlist(imglm_out$n.eff)>1])
# JAGS results saved as as a big table
write.table(im_sims_ssc, file='spr_sum_chinooka/ssc_im/im_glm3_sims_ssc.txt')
write.table(im_rhat_ssc, file='spr_sum_chinooka/ssc_im/im_glm3_rhat_ssc.txt')

# convert saved JAGS results into vectors ----
im_sims_ssc<- read.table('spr_sum_chinooka/ssc_im/im_glm3_sims_ssc.txt')
im_rhat_ssc<- read.table('spr_sum_chinooka/ssc_im/im_glm3_rhat_ssc.txt')

b_0_ssc=im_sims_ssc$b_0; b_run_ssc=im_sims_ssc$b_run; b_temp_ssc=im_sims_ssc$b_temp; b_temp2_ssc=im_sims_ssc$b_temp2; b_ftt_ssc=im_sims_ssc$b_ftt; b_dis_ssc=im_sims_ssc$b_dis; b_trans_ssc=im_sims_ssc$b_trans; sigma_yr_ssc=im_sims_ssc$sigma_yr; mu_v_ssc=im_sims_ssc$mu_v; sigma_v_ssc=im_sims_ssc$sigma_v; devi_ssc=im_sims_ssc$deviance

t_obs_ssc=im_sims_ssc$t_obs; t_rep_ssc=im_sims_ssc$t_rep

a_yr_ssc<- 3:17
for(i in 1:15){
  a_yr_ssc[i]= im_sims_ssc[7+i]
}
ayrs_ssc<- data.frame(cbind(unlist(a_yr_ssc[1]), unlist(a_yr_ssc[2]), unlist(a_yr_ssc[3]), unlist(a_yr_ssc[4]), unlist(a_yr_ssc[5]), unlist(a_yr_ssc[6]), unlist(a_yr_ssc[7]), unlist(a_yr_ssc[8]), unlist(a_yr_ssc[9]), unlist(a_yr_ssc[10]), unlist(a_yr_ssc[11]), unlist(a_yr_ssc[12]), unlist(a_yr_ssc[13]), unlist(a_yr_ssc[14]), unlist(a_yr_ssc[15]) ))

outtab_ssc<- cbind(b_0_ssc, b_run_ssc, b_temp_ssc, b_temp2_ssc, b_ftt_ssc, b_dis_ssc, b_trans_ssc, ayrs_ssc[,1], ayrs_ssc[,2], ayrs_ssc[,3], ayrs_ssc[,4], ayrs_ssc[,5], ayrs_ssc[,6], ayrs_ssc[,7], ayrs_ssc[,8], ayrs_ssc[,9], ayrs_ssc[,10], ayrs_ssc[,11], ayrs_ssc[,12], ayrs_ssc[,13], ayrs_ssc[,14], ayrs_ssc[,15], sigma_yr_ssc, mu_v_ssc, sigma_v_ssc, devi_ssc)
#
# output table ----
im_median_ssc<- cbind(apply(outtab_ssc, 2, median))
im_se_ssc<- cbind(apply(outtab_ssc, 2, sd))
im_cri_ssc<- apply(outtab_ssc, 2, function(x) quantile(x, c(0.025,0.975)))

summ_ssc<- data.frame(cbind(round(im_median_ssc,3), round(im_se_ssc,3), paste0('(', round(im_cri_ssc[1,], 3),', ', round(im_cri_ssc[2,], 3),')'), round(im_rhat_ssc[-c(26,27),], 3)))
row.names(summ_ssc)<- c('(Intercept)', 'Summer Run', 'Temperature', 'Temp^2', 'Travel Time', 'Flow', 'Transported','Year 2003','Year 2004','Year 2005','Year 2006','Year 2007','Year 2008','Year 2009','Year 2010','Year 2011','Year 2012','Year 2013','Year 2014','Year 2015','Year 2016','Year 2017', '$\\sigma_{year}$', '$\\mu_{vel}$', '$\\sigma_{vel}$', 'Deviance')
colnames(summ_ssc)<- c('Median','SD','95% CRI','$\\hat{R}$','Eff size')
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
#####

# simulated ftt vs. observed ftt ----
# overlapping ----
vel_pre<- rnorm(nrow(sscs), 31.182, 12.385)
vel_pre<- vel_pre[vel_pre> 0]
ftt_pre<- 225/vel_pre

hist(sscs$ftt, breaks=50, freq= F, col=rgb(0,0,0,0.8),
  main='Spring/Summer Chinook', xlab='Days')
hist(ftt_pre[ftt_pre<200], breaks=100, freq= F, col=rgb(1,1,1,.5), add= T)
legend(70, 0.15, c('Observed','Predicted'), pch=c(15,0), col=c(1,1), bty='n')

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



hist(sdff, breaks=30, xlim=c(0.02, max(sdff)))
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
#####

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
legend(8, 0.4, c(' ',' '), col=c('cyan','lightpink'), lwd=10, bty='n')
legend(8, 0.4, c('In-River','Transported'), col=c('navy','deeppink'), lwd=3, bty='n')

# sscs$tempbin<- cut(sscs$ihr_temp, breaks=c(5,10,11:14,16,23))
# points(c(8,10,11,12,13,15,18), tapply(sscs$gra_det, sscs$tempbin, mean), pch=20, cex=1)

# with ftt ----
# quantile(sscs$ftt, c(0.025,0.975), na.rm=TRUE)
plot(0,0, xlim=c(3,50), ylim=c(0,1), ty='n',
  xlab='Travel Time (Days)', ylab='Conversion')
invisible(apply(outtab_ssc[r, c('b_0_ssc','b_temp_ssc','b_temp2_ssc','b_ftt_ssc','b_trans_ssc')], 1, function(x) surv_ftt(x, lcol=c('grey80','grey90')) ))
invisible(apply(rbind(colMeans(outtab_ssc[r, c('b_0_ssc','b_temp_ssc','b_temp2_ssc','b_ftt_ssc','b_trans_ssc')])), 1, function(x) surv_ftt(x, lcol=c('grey60','grey70'), lw=3)))
invisible(apply(outtab_ssc[r, c('b_0_ssc','b_temp_ssc','b_temp2_ssc','b_ftt_ssc','b_trans_ssc')], 1, function(x) surv_ftt(x, alpha=4, omega=27)))
invisible(apply(rbind(colMeans(outtab_ssc[r, c('b_0_ssc','b_temp_ssc','b_temp2_ssc','b_ftt_ssc','b_trans_ssc')])), 1, function(x) surv_ftt(x, alpha=4, omega=27, lcol=c('navy','deeppink'), lw=3)))
legend(3, 0.4, c(' ',' '), col=c('cyan','lightpink'), lwd=10, bty='n')
legend(3, 0.4, c('In-River','Transported'), col=c('navy','deeppink'), lwd=3, bty='n')
#####

# misc
# mcnary passage and temp ----
plot(sscs$mca_jul, sscs$ihr_temp/750, pch=19, cex=0.5, ylim=c(0,0.04))
hist(subset(sscs, run=='spring')$mca_jul, breaks=100, freq=FALSE, add=TRUE, col=rgb(0,0,0,0.5))
hist(subset(sscs, run=='summer')$mca_jul, breaks=50, freq=FALSE, add=TRUE, col=rgb(1,0,0,0.5))
abline(h=20/750, lty=2)

# model selection ----
m1<- glm(gra_det~ jul_sca+ jul2+ temp_sca+ temp2+ dis_sca+ dis2+ mig_his+ as.factor(mca_yr), family= binomial, data= sscs)
m2<- update(m1, .~ .- as.factor(mca_yr))
AIC(m1,m2)
#     df      AIC
# m1 22 9350.091
# m2  8 9463.816
(9463.816-9350.091)/ (22-8)

m1<- glmer(gra_det~ jul_sca+ temp_sca+ temp2+ dis_sca+ mig_his+ (1|mca_yr), family= binomial, data= sscs)

m2<- update(m1, .~ .- jul_sca)
m3<- update(m2, .~ .- dis_sca)

AIC(m1,m2,m3)
#    df      AIC
# m1  7 9373.739
# m2  6 9372.611
# m3  5 9379.814

# temp and arrival ----
load(file=paste0(wd, 'data_compile/low_snake_temp_2003_2018.Rdata')) # temper
require(ggplot2)
tdls$obs_date<- as.POSIXlt(tdls$obs_date, format="%Y-%m-%d")
tdls$yr <- as.numeric(format(tdls$obs_date, "%Y"))
tdls$julian <- tdls$obs_date$yday + 1
# plot
uqday<- length(unique(sscs$mca_jul)) # 140
uqyr<- length(unique(sscs$mca_yr)) # 15
days<- rep(unique(sscs$mca_jul), uqyr*2)
yr<- rep(rep(unique(sscs$mca_yr), each=uqday), 2)
trn<- rep(c('river','trans'), each=(uqyr*uqday))
ct<- rep(NA, uqyr*uqday*2)
for(i in 1:(uqyr*uqday*2)){
  ct[i]<- nrow(subset(sscs, mca_yr==yr[i]& mca_jul==days[i]& mig_his==trn[i]))
} 
count<- data.frame(days, yr, trn, ct)
colnames(count)<- c('days', 'yr', 'trn', 'ihr_temp')

g <- ggplot(data=subset(tdls, yr!= 2018), aes(x=julian, y=ihr_temp))
g + facet_wrap(~yr, ncol=5) + 
  geom_line(size=1.5) + 
  geom_hline(yintercept=20,linetype=2) +
  theme_bw() +
  scale_x_continuous("Detection Date at McNary Dam", limits=c(80,225), labels=function(x) format(as.Date(x, '2017-01-01'), '%b %d')) +
  scale_y_continuous("Ice Harbor Dam Tailrace Temperature (C)", breaks=c(14,17,20,23), labels=c(14,17,20,23), limits=c(7.9,24)) + 
  theme(legend.position="bottom", legend.title=element_blank(),
    plot.margin = unit(x = c(0.5, 0.5, 0.5, 0.5), units = "cm")) +
  geom_ribbon(data=count, aes(x=days, ymin=8, ymax=ihr_temp+8, fill=trn), alpha=0.5, colour='gray50') +
  scale_fill_discrete(labels=c('In river','Transported'))

# estimates diff between runs ----
par(mfrow=c(1, 2))
par(oma = c(3, 0, 0, 0))
par(mar = c(3, 4, 2, 2))

# diff between run types
# set up avg env cond for spring and summer
mtemp_ssc<- tapply(sscs$ihr_temp, sscs$run, mean)
mftt_ssc<- tapply(sscs$ftt, sscs$run, function(x) mean(x, na.rm=TRUE))
mdis_ssc<- tapply(sscs$ihr_dis, sscs$run, mean)

su<- plogis(b_0_ssc+ b_run_ssc+ b_temp_ssc*tempScale(mtemp_ssc[2])+ b_temp2_ssc*tempScale2(mtemp_ssc[2]^2)+ b_ftt_ssc*fttScale(mftt_ssc[2])+ b_dis_ssc*disScale(mdis_ssc[2]))- plogis(b_0_ssc+ b_temp_ssc*tempScale(mtemp_ssc[1])+ b_temp2_ssc*tempScale2(mtemp_ssc[1]^2)+ b_ftt_ssc*fttScale(mftt_ssc[1])+ b_dis_ssc*disScale(mdis_ssc[1]))
# su<- plogis(b_0_ssc+ b_run_ssc)- plogis(b_0_ssc)
quantile(su, c(0.025,0.5,0.975))
su_obs<- mean(sscs[sscs$run=='summer','gra_det'])-
  mean(sscs[sscs$run=='spring','gra_det'])
hist(su, breaks=50, main='Run', xlab= NULL, col='grey50', border='white')
abline(v=su_obs, col='red', lwd=3)

# diff between transport
tr<- plogis(b_0_ssc)- plogis(b_0_ssc+b_trans_ssc)
tr_obs<- mean(sscs[sscs$mig_his=='river','gra_det'])- mean(sscs[sscs$mig_his=='trans','gra_det'])
hist(tr, breaks=30, main='Migration History', xlab= NULL, col='grey50', border='white')
abline(v=tr_obs, col='red', lwd=3)

mtext('Difference in Conversion', side=1, outer=TRUE, line=0, cex=1.2)






















