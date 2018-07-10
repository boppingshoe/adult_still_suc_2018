
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
    # det[i]~ dbern(phi[i])
    det[i]~ dbern(phi_bound[i])
    phi_bound[i]<- max(0.0000000001, min(0.9999999999, phi[i]))
    logit(phi[i])<- b_0+ b_juld*juld[i]+ b_juld2*juld2[i]+
      b_temp*temp[i]+ w1*b_temp2*temp2[i]+
      b_ftt*ftt[i]+ w2*b_txf*temp[i]*ftt[i]+ w2*b_t2xf*temp2[i]*ftt[i]+
      b_trans*trans[i]
    r_obs[i]<- det[i]- phi[i]
    det_rep[i]~ dbern(pr_bound[i])
    pr_bound[i]<- max(0.0000000001, min(0.9999999999, phi_rep[i]))
    logit(phi_rep[i])<- b_0+ b_juld*juld[i]+ b_juld2*juld2[i]+
      b_temp*temp[i]+ w1*b_temp2*temp2[i]+
      b_ftt*ftt[i]+ w2*b_txf*temp[i]*ftt[i]+ w2*b_t2xf*temp2[i]*ftt[i]+
      b_trans*trans[i]
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
  w2~ dbern(0.5)
  b_txf~ dt(0, 0.4, 1)
  w1~ dbern(0.5)
  b_temp2~ dt(0, 0.4, 1)
  b_t2xf~ dt(0, 0.4, 1)

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

# run JAGS ----
parameters <- c('b_0','b_juld','b_juld2','b_temp','b_temp2','b_ftt','b_txf','b_t2xf','b_trans','mu_v','sigma_v','t_obs','t_rep','w1','w2' )
inits<- function() {list(b_0=runif(1,-1,1), b_juld=runif(1,-1,1), b_juld2=runif(1,-1,1), b_temp=runif(1,-1,1), b_ftt=runif(1,-1,1), b_txf=runif(1,-1,1), b_trans=runif(1,-1,1), mu_v=runif(1,1,8), sigma_v=runif(1,0,2), b_t2xf=runif(1,-1,1), b_temp2=runif(1,-1,1), w1=rbinom(1,1,0.5), w2=rbinom(1,1,0.5) )}

# nc<- 4   ;   ni<- 100   ;   nb<- 0   ;   nt<- 1 # test run
# nc<- 4   ;   ni<- 1000   ;   nb<- 500   ;   nt<- 1 # test run2
nc<- 4   ;   ni<- 40000   ;   nb<-20000   ;   nt<- 2
# nc<- 4   ;   ni<- 80000   ;   nb<-50000   ;   nt<- 3

imglm_out<- jags(im_data, inits, parameters, "steelyhead/st_im/st_im_glm.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)
# im_out<- autojags(im_data, inits, parameters, "steelyhead/st_im/st_im_glm.txt", n.thin=nt, n.chains=nc, n.burnin=2000, iter.increment=5000, max.iter=42000, parallel=TRUE)
# imglm_out<- update(imglm_out, parameters.to.save=parameters, n.thin=1, n.chains=4, n.iter=50000, parallel=TRUE)
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

b_0_st=im_sims_st$b_0; b_juld_st=im_sims_st$b_juld; b_juld2_st=im_sims_st$b_juld2; b_temp_st=im_sims_st$b_temp; b_ftt_st=im_sims_st$b_ftt; b_txf_st=im_sims_st$b_txf; b_trans_st=im_sims_st$b_trans; mu_v_st=im_sims_st$mu_v; sigma_v_st=im_sims_st$sigma_v; devi_st=im_sims_st$deviance
t_obs_so=im_sims_st$t_obs; t_rep_so=im_sims_st$t_rep

outtab_st<- cbind(b_0_st, b_juld_st, b_juld2_st, b_temp_st, b_ftt_st, b_txf_st, b_trans_st, mu_v_st, sigma_v_st, devi_st)

# output table ----
im_median_st<- cbind(apply(outtab_st, 2, median))
im_se_st<- cbind(apply(outtab_st, 2, sd))
im_cri_st<- apply(outtab_st, 2, function(x) quantile(x, c(0.025,0.975)))

summ_st<- data.frame(cbind(round(im_median_st,3), round(im_se_st,3), paste0('(', round(im_cri_st[1,], 3),', ', round(im_cri_st[2,], 3),')'), round(im_rhat_st[-c(10,11),], 3)))
row.names(summ_st)<- c('(Intercept)', 'Arrival Date', 'Arrival^2^', 'Temperature', 'Travel Time', 'TempxFtt', 'Transported', '$\\mu_{vel}$', '$\\sigma_{vel}$', 'Deviance')
colnames(summ_st)<- c('Median','SD','95% CRI','$\\hat{R}$','Eff size')
summ_st

# traceplot ----
windows(8,9)
par(mfrow=c(4,2))
names(outtab_st)<- c('b0 (Intercept)', 'b Arrival', 'b Arr^2', 'b Temp', 'b Ftt', 'b TempxFtt', 'b Trans', 'MuVel', 'SigmaVel', 'Deviance')

# ncol(outtab_st)
pn1<- 1:4; pn2<- 5:8; pn3<- 9:10
for(i in pn1){
  plot_pds(outtab_st[,i], lab=names(outtab_st)[i], colr='grey70')
}

# plotting survival relationships ----
pn<- nrow(outtab_st)
nsim<- 1000
r<- sample(1:pn, nsim)
windows(10,4)
par(mfrow=c(1,2))
par(mar=c(5,4,2,2)+0.1) # original 5,4,4,2
# with temperature ----
# quantile(sts$ihr_temp, c(0.025,0.975), na.rm=TRUE) # 17.61250 21.61574
# quantile(sts$mca_jul, c(0.025,0.975), na.rm=TRUE) # 228 277 (range= 128 to 319)
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
# quantile(sts$ftt, c(0.025,0.975), na.rm=TRUE) # 4.219896 18.005590
plot(0,0, xlim=c(3,25), ylim=c(0,1), ty='n',
  xlab='Travel Time (Days)', ylab='Conversion')
invisible(apply(outtab_st[r, c('b_0_st','b_temp_st','b_ftt_st','b_txf_st','b_trans_st')], 1, function(x) surv_ftt(x, temp=19, lcol=c('grey90','grey80')) ))
invisible(apply(rbind(colMeans(outtab_st[r, c('b_0_st','b_temp_st','b_ftt_st','b_txf_st','b_trans_st')])), 1, function(x) surv_ftt(x, temp=19, lcol=c('grey70','grey60'), lw=3)))
invisible(apply(outtab_st[r, c('b_0_st','b_temp_st','b_ftt_st','b_txf_st','b_trans_st')], 1, function(x) surv_ftt(x, temp=19, alpha=4, omega=18)))
invisible(apply(rbind(colMeans(outtab_st[r, c('b_0_st','b_temp_st','b_ftt_st','b_txf_st','b_trans_st')])), 1, function(x) surv_ftt(x, temp=19, alpha=4, omega=18, lcol=c('olivedrab','turquoise4'), lw=3)))
legend(15, 0.4, c(' ',' '), col=c('chartreuse','aquamarine'), lwd=10, bty='n')
legend(15, 0.4, c('19C','21C'), col=c('olivedrab','turquoise4'), lwd=3, bty='n')
#####

# mcnary passage and temp ----
plot(sts$mca_jul, sts$ihr_temp/735, pch=19, cex=0.5, ylim=c(0,0.04))
hist(sts$mca_jul, breaks=30, freq=FALSE, add=TRUE, col=rgb(0,0,0,0.5))
abline(h=20/735, lty=2)

# model selection ----
# require(lme4)
# m1<- glmer(gra_det~ jul_sca+ jul2+ temp_sca+ mig_his+ (1|mca_yr), family= binomial, data= sts)
# vif_mer(m1)

m1<- glm(gra_det~ jul_sca+ jul2+ temp_sca+ temp2+ dis_sca+ dis2+ mig_his+ as.factor(mca_yr), family= binomial, data= sts)
m2<- update(m1, .~ .- as.factor(mca_yr))
m3<- update(m1, .~ .- dis2)
m4<- update(m3, .~ .- dis_sca)
m5<- update(m4, .~ .- temp2)
AIC(m1,m3,m4,m5)
#    df      AIC
# m1 21 13909.55
# m2  8 14103.78
# m3 20 13908.03
# m4 19 13909.33
# m5 18 13921.44
(14103.78-13909.55)/ (21-8)
# [1] 14.94077

#####







