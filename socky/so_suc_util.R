
# functions for socky eye analysis (june 2018)

so_load_dat<- function(wd){
  load(file=paste0(wd, "data_compile/so_data/solsdat.Rdata"))
  sos<- subset(solsdat, esu=='snake') # limit comparison of transport to snake fish
  sos<- sos[sos$mca_jul<240,]
  
  sos$ftt<- with(sos, as.numeric(gra_obs- mca_obs))
  sos<- sos[(sos$ftt>0& sos$ftt<366)| is.na(sos$ftt),]
  
  sos$vel<- 225/(sos$ftt) # distance/day (mcn-lgr 522+173- 470)
  sos$iha_det<- ifelse(is.na(sos$iha_obs), 0, 1)
  sos$gra_det<- ifelse(is.na(sos$gra_obs), 0, 1)
  sos$mig_yr<- ifelse(sos$mca_yr<=2010, '<=2010', sos$mca_yr)

  sos$ftt_sca<- scale(sos$ftt)
  sos$temp_sca<- scale(sos$ihr_temp)
  sos$jul_sca<- scale(sos$mca_jul)
  sos$dis_sca<- scale(sos$ihr_dis)
  sos$temp2<- scale(sos$ihr_temp^2)
  sos$jul2<- scale(sos$mca_jul^2)
  sos$dis2<- scale(sos$ihr_dis^2)
  sos$ftt_mi<- as.numeric(with(sos, difftime(iha_obs, mca_obs, units='days')))
  sos$ftt_ig<- as.numeric(with(sos, difftime(gra_obs, iha_obs, units='days')))
  sos$velrat<- (68/sos$ftt_mi) / (157/sos$ftt_ig)
  sos$mcstray<- apply (sos[,c('pra_obs','ria_obs','wea_obs')],
    1, function(x) as.numeric(any(!is.na(x))))
  sos$stray<- apply (sos[,c('pra_obs','ria_obs','wea_obs')],
    1, function(x) as.numeric(any(!is.na(x))))
  sos$stray<- ifelse((sos$velrat>=0.2&sos$velrat<=5)|is.na(sos$velrat), sos$stray, 1)
  # sos$stray<- ifelse(sos$ftt< 21|is.na(sos$ftt), sos$stray, 1)
  sos<- sos[!is.na(sos$ihr_temp),]
  
  return(sos)
}

tempScale_so<- function(x) (x- mean(nusos$ihr_temp))/ sd(nusos$ihr_temp)

# fttScale_so<- function(x) (x- mean(nusos$ftt, na.rm=TRUE))/ sd(nusos$ftt, na.rm=TRUE)

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
so_prep_dat<- function(sos_in, strytime, typ='stan'){
  sos<- subset(sos_in, ftt< strytime|is.na(ftt))
  sos<- sos[order(sos$ftt),]

  temp<- as.vector(sos$temp_sca)
  dis<- as.vector(sos$dis_sca)
  trans<- ifelse(sos$mig_his=='trans', 1, 0)
  yr<- as.numeric(as.factor(sos$mig_yr)) # NOT mca_yr
  vel<- sos$vel
  det<- sos$gra_det

  if(typ=='stan') {
    N<- nrow(sos)
    n_obs<- sum(!is.na(sos$ftt))
    n_mis<- N- n_obs
    vel_obs<- sos[1:n_obs,]$vel
    ftt_obs<- as.vector(sos[1:n_obs,]$ftt)
    # ftt_obs<- as.vector(sos[1:n_obs,]$ftt_sca)
    # m_ftt<- mean(sos$ftt, na.rm=TRUE)
    # sd_ftt<- sd(sos$ftt, na.rm=TRUE)
    stray<- sos$stray
    out_dat<- list(N=N, n_obs=n_obs, n_mis=n_mis,
      temp=temp, dis=dis, vel_obs=vel_obs,
      ftt_obs=ftt_obs, trans=trans, yr=yr, det=det,
      stray=stray)#, m_ftt=m_ftt, sd_ftt=sd_ftt)
  } else {
    n_ind<- nrow(sos)
    out_dat<- list(det=det, n_ind=n_ind,
      temp=temp, dis=dis, vel=vel, trans=trans, yr=yr)
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
surv_temp_so<- function(b, alpha=13, omega=23, lcol=c('cyan','lightpink'), lw=1){
  curve(plogis(b[1]+ b[2]*tempScale_so(x)),
    alpha, omega, col=lcol[1], lwd=lw, add=TRUE)
  curve(plogis(b[1]+ b[2]*tempScale_so(x)+ b[4]),
    alpha, omega, col=lcol[2], lwd=lw, add=TRUE)
}
# surv vs. ftt
surv_ftt_so<- function(b, temp=mean(sos$ihr_temp), alpha=3, omega=30, lcol=c('chartreuse','aquamarine'), lw=1){
  curve(plogis(b[1]+ b[2]*tempScale_so(temp)+ b[3]*x),
      alpha, omega, col=lcol[1], lwd=lw, add=TRUE)
  curve(plogis(b[1]+ b[2]*tempScale_so(temp+2)+ b[3]*x),
      alpha, omega, col=lcol[2], lwd=lw, add=TRUE)
}








