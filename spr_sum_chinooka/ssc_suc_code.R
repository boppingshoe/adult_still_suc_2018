
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

im_data<- ssc_prep_dat(sscs, typ='jags')
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

# simulated ftt vs. observed ftt
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
# model selection ----
m1<- glm(gra_det~ jul_sca+ jul2+ temp_sca+ temp2+ dis_sca+ dis2+ mig_his+ run+ as.factor(mca_yr), family= binomial, data= sscs)
m2<- update(m1, .~ .- as.factor(mca_yr))
AIC(m1,m2)
#     df      AIC
# m1 23 9280.473
# m2  9 9367.917
(9280.472- 9367.917)/ (23- 8)

# library(lme4)
# m1<- glmer(gra_det~ jul_sca+ jul2+ temp_sca+ temp2+ dis_sca+ dis2+ mig_his+ run+ (1|mca_yr), family= binomial, data= sscs) # failed to converge

drop1(m1, test='Chisq')
m2<- update(m1, .~ .- jul2- jul_sca)
drop1(m2, test='Chisq')
m3<- update(m2, .~ .- dis2)
drop1(m3, test='Chisq')
m4<- update(m3, .~ .- dis_sca)

AIC(m1,m2,m3,m4)
#    df      AIC
# m1 23 9280.473
# m2 21 9279.298
# m3 20 9281.340
# m4 19 9290.256
car::vif(m3)
m4<- update(m3, .~ .- temp2)
car::vif(m4) # it's fine

# check stray assignment ----
ftt_stry<- table(sscs$stray, round(sscs$ftt))
barplot(ftt_stry, main="Ftt for 0 vs 1 detection",
  xlab="Days", col=c("black","deeppink"),
  legend = rownames(ftt_stry))

gs<- table(sscs$gra_det, sscs$stray)
mcgs<- table(sscs$gra_det, sscs$mcstray)
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
source(file=paste0(wd, "spr_sum_chinooka/ssc_suc_util.R")) # where the functions are
sscs<- ssc_load_dat(wd)
#
# stan model ----
cat("
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
  
",fill=TRUE, file=paste0(wd, "spr_sum_chinooka/ssc_im/in_stan/ssc_im_glm.stan"))
#####

# data
im_data<- ssc_prep_dat(sscs, typ='stan')
str(im_data)

require(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# run stan ----
# nc<- 1; ni<- 60; nt<- 1 # test run, burn-in 50%
# nc<- 4; ni<- 500; nt<- 1
# nc<- 4; ni<- 5000; nt<- 1 # 2.28 hours
nc<- 4; ni<- 10000; nt<- 2

parameters <- c('b_0','b_run','b_temp','b_temp2','b_ftt','b_dis','b_trans','a_yr','sigma_yr','mu_v','sigma_v','tm_rep','trm_rep','trm_obs','spm_rep','spm_obs','vel_mis')

ssc_fit<- stan(data=im_data, file=paste0(wd, "spr_sum_chinooka/ssc_im/in_stan/ssc_im_glm.stan"), chains=nc, iter=ni, thin=nt, pars=parameters, include=TRUE)

fit_summ <- summary(ssc_fit)
round(fit_summ$summary, 3)[c(1:32,1332),]
sum(fit_summ$summary[,10]>1.01)

df_ssc <- as.data.frame(ssc_fit)
dim(df_ssc)

save(ssc_fit, file=paste0(wd, 'spr_sum_chinooka/ssc_im/in_stan/ssc_fit.R'))
write.table(df_ssc, file=paste0(wd, 'spr_sum_chinooka/ssc_im/in_stan/im_glm_sims_ssc.txt'))
im_rhat_ssc<- fit_summ$summary[,9:10]
write.table(im_rhat_ssc, file=paste0(wd, 'spr_sum_chinooka/ssc_im/in_stan/im_rhat_ssc.txt'))
#####






















