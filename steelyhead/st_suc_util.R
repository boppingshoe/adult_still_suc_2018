
# Utility functions for steelyhead analysis

load_dat_st<- function(wd){
  load(file= paste0(wd, "data_compile/st_data/stlsdat.Rdata"))
  k2<- subset(stlsdat, esu=='Snake' & stock!='B')
  sts<- subset(k2, as.numeric(format(mca_obs, '%m')) %in% c(6:9))
  sts$ftt<- with(sts, as.numeric(gra_obs- mca_obs))
  sts<- sts[sts$ftt>0|is.na(sts$ftt),]
  sts$vel<- 225/(sts$ftt) # distance/day (mcn-lgr 522+173- 470)
  
  sts$ftt_sca<- scale(sts$ftt)
  sts$temp_sca<- scale(sts$ihr_temp)
  sts$jul_sca<- scale(sts$mca_jul)
  sts$dis_sca<- scale(sts$ihr_dis)
  sts$temp2<- scale(sts$ihr_temp^2)
  sts$jul2<- scale(sts$mca_jul^2)
  sts$dis2<- scale(sts$ihr_dis^2)
  sts$ftt_mi<- as.numeric(with(sts, difftime(iha_obs, mca_obs, units='days')))
  sts$ftt_ig<- as.numeric(with(sts, difftime(gra_obs, iha_obs, units='days')))
  sts$velrat<- (68/sts$ftt_mi) / (157/sts$ftt_ig)
  sts$mcstray<- apply (sts[,c('pra_obs','ria_obs','wea_obs')],
    1, function(x) as.numeric(any(!is.na(x))))
  sts$stray<- apply (sts[,c('pra_obs','ria_obs','wea_obs')],
    1, function(x) as.numeric(any(!is.na(x))))
  sts$stray<- ifelse((sts$velrat>=0.2 & sts$velrat<=5)|is.na(sts$velrat), sts$stray, 1)
  # sts$stray<- ifelse(sts$ftt< 60|is.na(sts$ftt), sts$stray, 1)
  sts<- sts[!is.na(sts$ihr_temp),]
  return(sts)
}

tempScale_st<- function(x) (x- mean(sts$ihr_temp))/ sd(sts$ihr_temp)
# tempScale2_st<- function(x) (x- mean(sts$ihr_temp^2))/ sd(sts$ihr_temp^2)

julScale_st<- function(x) (x- mean(sts$mca_jul))/ sd(sts$mca_jul)
julScale2_st<- function(x) (x- mean(sts$mca_jul^2))/ sd(sts$mca_jul^2)

# fttScale_st<- function(x) (x- mean(nusts$ftt, na.rm=TRUE))/ sd(nusts$ftt, na.rm=TRUE)

vif_mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

# JAGS stuff
prep_dat<- function(sts){
  n_ind<- nrow(sts)
  juld<- as.vector(sts$jul_sca)
  temp<- as.vector(sts$temp_sca)
  juld2<- as.vector(scale(sts$mca_jul^2))
  temp2<- as.vector(scale(sts$ihr_temp^2))
  dis<- as.vector(sts$dis_sca)
  trans<- ifelse(sts$mig_his=='Trans', 1, 0)
  yr<- sts$mca_yr-2002
  # ftt<- sts$ftt
  vel<- sts$vel
  det<- sts$gra_det
  out_dat<- list(det=det, n_ind=n_ind,
    juld=juld, juld2=juld2, temp=temp, vel=vel,
    trans=trans, yr=yr)#, dis=dis, temp2=temp2)

  return(out_dat)
}
# traceplot
trace<- function(x,i,nc){
  l<- length(x)/nc
  plot(c(1:l), x[1:l], ty='l', ylab=i, xlab='')#, xlim=c(0,1000))
  lines(c(1:l), x[(l+1):(2*l)], col='grey30')
  lines(c(1:l), x[(2*l+1):(3*l)], col='grey50')
  lines(c(1:l), x[(3*l+1):(4*l)], col='grey70')
}
# automate diag plots
plot_pds<- function(pd, lab, nc=4, brks=50, colr=1){
  trace(pd, lab, nc)
  hist(pd, breaks=brks, col=colr, freq=FALSE, xlab=NA, main=NA)
}

# Stan stuff
stan_dat<- function(sts_in, strytime){
  sts<- subset(sts_in, ftt< strytime|is.na(ftt))
  sts<- sts[order(sts$ftt),]
  N<- nrow(sts)
  n_obs<- sum(!is.na(sts$ftt))
  n_mis<- N- n_obs
  # m_ftt<- mean(sts$ftt, na.rm=TRUE)
  # sd_ftt<- sd(sts$ftt, na.rm=TRUE)
  
  juld<- as.vector(sts$jul_sca)
  juld2<- as.vector(scale(sts$mca_jul^2))
  temp<- as.vector(sts$temp_sca)
  vel_obs<- sts[1:n_obs,]$vel
  ftt_obs<- as.vector(sts[1:n_obs,]$ftt)
  # ftt_obs<- as.vector(sts[1:n_obs,]$ftt_sca)
  trans<- ifelse(sts$mig_his=='Trans', 1, 0)
  stray<- sts$stray
  det<- sts$gra_det

  out_dat<- list(N=N, n_obs=n_obs, n_mis=n_mis,
    juld=juld, juld2=juld2, temp=temp,
    vel_obs=vel_obs, ftt_obs=ftt_obs, trans=trans, det=det,
    stray=stray)
  
  return(out_dat)
}


# survival plots
# surv vs. temp 
surv_temp_st<- function(b, juld=mean(sts$mca_jul), ftt=median(sts$ftt, na.rm=TRUE), alpha=14, omega=23, lcol=c('cyan','lightpink'), lw=1){
  curve(plogis(b[1]+ b[2]*tempScale_st(x)+ b[3]*ftt),
    alpha, omega, col=lcol[1], lwd=lw, add=TRUE)
  curve(plogis(b[1]+ b[2]*tempScale_st(x)+ b[3]*ftt+ b[4]),
    alpha, omega, col=lcol[2], lwd=lw, add=TRUE)
}
# surv vs. ftt 
surv_ftt_st<- function(b, temp=mean(sts$ihr_temp), alpha=3, omega=100, lcol=c('chartreuse','aquamarine'), lw=1){
  curve(plogis(b[1]+ b[2]*tempScale_st(temp)+ b[3]*x),
    alpha, omega, col=lcol[1], lwd=lw, add=TRUE)
  curve(plogis(b[1]+ b[2]*tempScale_st(temp+2)+ b[3]*x),
    alpha, omega, col=lcol[2], lwd=lw, add=TRUE)
}








