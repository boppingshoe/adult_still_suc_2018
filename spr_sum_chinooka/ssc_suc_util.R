
# functions for spring/summer chinooka analysis (june 2018)

ssc_load_dat<- function(wd){
  load(file=paste0(wd, "data_compile/ssc_data/ssclsdat.Rdata"))
  sscs<- subset(ssclsdat, esu=='Snake') # limit comparison of transport to snake fish
  
  sscs$ftt<- with(sscs, as.numeric(gra_obs- mca_obs))
  sscs<- sscs[(sscs$ftt>0& sscs$ftt<366)| is.na(sscs$ftt),]
  
  sscs$vel<- 225/(sscs$ftt) # distance/day (mcn-lgr 522+173- 470)
  sscs$iha_det<- ifelse(is.na(sscs$iha_obs), 0, 1)
  sscs$gra_det<- ifelse(is.na(sscs$gra_obs), 0, 1)

  sscs$ftt_sca<- scale(sscs$ftt)
  sscs$temp_sca<- scale(sscs$ihr_temp)
  sscs$jul_sca<- scale(sscs$mca_jul)
  sscs$dis_sca<- scale(sscs$ihr_dis)
  sscs$temp2<- scale(sscs$ihr_temp^2)
  sscs$jul2<- scale(sscs$mca_jul^2)
  sscs$dis2<- scale(sscs$ihr_dis^2)
  sscs<- sscs[!is.na(sscs$ihr_temp),]
  sscs$ftt_mi<- as.numeric(with(sscs, difftime(iha_obs, mca_obs, units='days')))
  sscs$ftt_ig<- as.numeric(with(sscs, difftime(gra_obs, iha_obs, units='days')))
  sscs$velrat<- (68/sscs$ftt_mi) / (157/sscs$ftt_ig)
  sscs$mcstray<- apply (sscs[,c('pra_obs','ria_obs','wea_obs')],
    1, function(x) as.numeric(any(!is.na(x))))
  sscs$stray<- sscs$mcstray
  sscs$stray<- ifelse((sscs$velrat>=0.2&sscs$velrat<=5)|is.na(sscs$velrat),
    sscs$stray, 1)
  
  return(sscs)
}

tempScale<- function(x) (x- mean(sscs$ihr_temp))/ sd(sscs$ihr_temp)
tempScale2<- function(x) (x- mean(sscs$ihr_temp^2))/ sd(sscs$ihr_temp^2)
julScale<- function(x) (x- mean(sscs$mca_jul))/ sd(sscs$mca_jul)
disScale<- function(x) (x- mean(sscs$ihr_dis))/ sd(sscs$ihr_dis)
# fttScale<- function(x) (x- mean(sscs$ftt, na.rm=TRUE))/ sd(sscs$ftt, na.rm=TRUE)

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

# JAGS/Stan stuff
ssc_prep_dat<- function(dat, typ='stan'){
  dat<- dat[order(dat$ftt),]
  
  summer<- ifelse(dat$run=='summer', 1, 0)
  temp<- as.vector(dat$temp_sca)
  temp2<- as.vector(scale(dat$ihr_temp^2))
  dis<- as.vector(dat$dis_sca)
  trans<- ifelse(dat$mig_his=='trans', 1, 0)
  yr<- dat$mca_yr-2002
  det<- dat$gra_det
  stray<- dat$stray
  
  if(typ=='stan') {
    N<- nrow(dat)
    n_obs<- sum(!is.na(dat$ftt))
    n_mis<- N- n_obs
    vel_obs<- dat[1:n_obs,]$vel
    ftt_obs<- as.vector(dat[1:n_obs,]$ftt)
    out_dat<- list(N=N, n_obs=n_obs, n_mis=n_mis,
      summer=summer, temp=temp, temp2=temp2, dis=dis,
      vel_obs=vel_obs, ftt_obs=ftt_obs, trans=trans, yr=yr,
      det=det, stray=stray)
  } else {
    vel<- dat$vel
    n_ind<- nrow(dat)
    out_dat<- list(det=det, n_ind=n_ind, summer=summer,
      temp=temp, temp2=temp2, dis=dis, trans=trans, yr=yr, vel=vel)
  }
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


# survival plots
# surv vs. temp
surv_temp<- function(b, alpha=8, omega=22, lcol=c('cyan','lightpink'), lw=1){
  curve(plogis(b[1]+ b[2]*tempScale(x)+ b[3]*tempScale2(x^2)),
    alpha, omega, col=lcol[1], lwd=lw, add=TRUE)
  curve(plogis(b[1]+ b[2]*tempScale(x)+ b[3]*tempScale2(x^2)+ b[4]),
    alpha, omega, col=lcol[2], lwd=lw, add=TRUE)
} 
# surv vs. ftt
surv_ftt<- function(b, temp=mean(sscs$ihr_temp), alpha=3, omega=50, lcol=c('cyan','lightpink'), lw=1){
  curve(plogis(b[1]+b[2]*tempScale(temp)+ b[3]*tempScale2(temp^2)+ b[4]*x), alpha, omega, col=lcol[1], lwd=lw, add=TRUE)
  curve(plogis(b[1]+b[2]*tempScale(temp+2)+ b[3]*tempScale2((temp+2)^2)+ b[4]*x), alpha, omega, col=lcol[2], lwd=lw, add=TRUE)
}









