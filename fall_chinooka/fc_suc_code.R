
# load/format data
rm(list=ls())
wd<- 'G:/STAFF/Bobby/css/adult_still_suc_2018/'
source(file=paste0(wd, "fall_chinooka/fc_suc_util.R")) # where the functions are
fcs<- fc_load_dat(wd)
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
    logit(phi[i])<- b_0+ b_juld*juld[i]+ b_juld2*juld2[i]+ b_temp*temp[i]+
      b_ftt*ftt[i]+ b_txf*temp[i]*ftt[i]+ b_trans*trans[i]
    r_obs[i]<- det[i]- phi[i]
    det_rep[i]~ dbern(pr_bound[i])
    pr_bound[i]<- max(0.0000000001, min(0.9999999999, phi_rep[i]))
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

im_data<- prep_dat(fcs, typ='glm')
str(im_data)

# run JAGS ----
parameters <- c('b_0','b_juld','b_juld2','b_temp','b_ftt','b_txf','b_trans','mu_v','sigma_v','t_obs','t_rep')
inits<- function() {list(b_0=runif(1,-1,1), b_juld=runif(1,-1,1), b_juld2=runif(1,-1,1), b_temp=runif(1,-1,1), b_ftt=runif(1,-1,1), b_txf=runif(1,-1,1), b_trans=runif(1,-1,1), mu_v=runif(1,1,8), sigma_v=runif(1,0,2) )}

# nc<- 4   ;   ni<- 100   ;   nb<- 0   ;   nt<- 1 # test run
# nc<- 4   ;   ni<- 1000   ;   nb<- 500   ;   nt<- 1 # test run2
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
#####

# convert output ----
im_sims_fc<- read.table('fall_chinooka/fc_im/im_glm2_sims_fc.txt')
im_rhat_fc<- read.table('fall_chinooka/fc_im/im_glm2_rhat_fc.txt')

b_0_fc=im_sims_fc$b_0; b_juld_fc=im_sims_fc$b_juld; b_juld2_fc=im_sims_fc$b_juld2; b_temp_fc=im_sims_fc$b_temp; b_ftt_fc=im_sims_fc$b_ftt; b_txf_fc=im_sims_fc$b_txf; b_trans_fc=im_sims_fc$b_trans; mu_v_fc=im_sims_fc$mu_v; sigma_v_fc=im_sims_fc$sigma_v; devi_fc=im_sims_fc$deviance
t_obs_so=im_sims_fc$t_obs; t_rep_so=im_sims_fc$t_rep

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
# ----

# mcnary passage and temp ----
plot(fcs$mca_jul, fcs$ihr_temp/735, pch=19, cex=0.5, ylim=c(0,0.04))
hist(fcs$mca_jul, breaks=30, freq=FALSE, add=TRUE, col=rgb(0,0,0,0.5))
abline(h=20/735, lty=2)

# model selection ----
m1<- glm(gra_det~ jul_sca+ jul2+ temp_sca+ temp2+ dis_sca+ dis2+ mig_his+ as.factor(mca_yr), family= binomial, data= fcs)
m2<- update(m1, .~ .- as.factor(mca_yr))
m3<- update(m2, .~ .- dis2)
m4<- update(m3, .~ .- temp2)
m5<- update(m4, .~ .- jul2)
m6<- update(m5, .~ .- dis_sca)
m7<- update(m6, .~ .- temp_sca)
AIC(m1,m2,m3,m4,m5,m6,m7)
# df      AIC
# m1 22 5425.763
# m2  8 5454.801
# m3  7 5455.241
# m4  6 5453.255
# m5  5 5539.144
# m6  4 5537.868
# m7  3 5552.651
(5454.801-5425.763)/ (22-8)
# [1] 2.074143

m8<- update(m4, .~ .- dis_sca)
AIC(m4,m8)
# df      AIC
# m4  6 5453.255
# m8  5 5451.964
# ----







