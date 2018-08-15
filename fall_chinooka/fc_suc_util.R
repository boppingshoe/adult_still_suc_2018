
# functions for fall chinooka analysis (mar 2018)

load_dat_fc<- function(wd){
  load(file=paste0(wd, "data_compile/fc_data/fclsdat.Rdata"))
  fcs<- subset(fclsdat, esu=='Snake') # limit comparison of transport to snake fish
  
  fcs$ftt<- with(fcs, as.numeric(gra_obs- mca_obs))
  fcs<- fcs[fcs$ftt>0|is.na(fcs$ftt),]
  
  fcs$vel<- 225/(fcs$ftt) # distance/day (mcn-lgr 522+173- 470)
  fcs$iha_det<- ifelse(is.na(fcs$iha_obs), 0, 1)
  fcs$gra_det<- ifelse(is.na(fcs$gra_obs), 0, 1)

  fcs$ftt_sca<- scale(fcs$ftt)
  fcs$temp_sca<- scale(fcs$ihr_temp)
  fcs$jul_sca<- scale(fcs$mca_jul)
  fcs$dis_sca<- scale(fcs$ihr_dis)
  fcs$temp2<- scale(fcs$ihr_temp^2)
  fcs$jul2<- scale(fcs$mca_jul^2)
  fcs$dis2<- scale(fcs$ihr_dis^2)
  fcs$ftt_mi<- as.numeric(with(fcs, difftime(iha_obs, mca_obs, units='days')))
  fcs$ftt_ig<- as.numeric(with(fcs, difftime(gra_obs, iha_obs, units='days')))
  fcs$velrat<- (68/fcs$ftt_mi) / (157/fcs$ftt_ig)
  fcs$mcstray<- apply (fcs[,c('pra_obs','ria_obs','wea_obs')],
    1, function(x) as.numeric(any(!is.na(x))))
  fcs$stray<- apply (fcs[,c('pra_obs','ria_obs','wea_obs')],
    1, function(x) as.numeric(any(!is.na(x))))
  fcs$stray<- ifelse((fcs$velrat>=0.2&fcs$velrat<=5)|is.na(fcs$velrat), fcs$stray, 1)
  # fcs$stray<- ifelse(fcs$ftt< 21|is.na(fcs$ftt), fcs$stray, 1)
  fcs<- fcs[!is.na(fcs$ihr_temp),]
  
  return(fcs)
}

tempScale<- function(x) (x- mean(fcs$ihr_temp))/ sd(fcs$ihr_temp)

julScale<- function(x) (x- mean(fcs$mca_jul))/ sd(fcs$mca_jul)
julScale2<- function(x) (x- mean(fcs$mca_jul^2))/ sd(fcs$mca_jul^2)

# fttScale<- function(x) (x- mean(nufcs$ftt, na.rm=TRUE))/ sd(nufcs$ftt, na.rm=TRUE)

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
prep_dat_fc<- function(fcs_in, strytime, typ='stan'){
  fcs<- subset(fcs_in, ftt< strytime|is.na(ftt))
  fcs<- fcs[order(fcs$ftt),]

  juld<- as.vector(fcs$jul_sca)
  temp<- as.vector(fcs$temp_sca)
  juld2<- as.vector(scale(fcs$mca_jul^2))
  # temp2<- as.vector(scale(fcs$ihr_temp^2))
  # dis<- as.vector(fcs$dis_sca)
  trans<- ifelse(fcs$mig_his=='trans', 1, 0)
  # yr<- fcs$boa_yr-2002
  vel<- fcs$vel
  det<- fcs$gra_det
  stray<- fcs$stray

  if(typ=='stan') {
    N<- nrow(fcs)
    n_obs<- sum(!is.na(fcs$ftt))
    n_mis<- N- n_obs
    vel_obs<- fcs[1:n_obs,]$vel
    ftt_obs<- as.vector(fcs[1:n_obs,]$ftt)
    # ftt_obs<- as.vector(fcs[1:n_obs,]$ftt_sca)
    # m_ftt<- mean(fcs$ftt, na.rm=TRUE)
    # sd_ftt<- sd(fcs$ftt, na.rm=TRUE)
    out_dat<- list(N=N, n_obs=n_obs, n_mis=n_mis,
      juld=juld, juld2=juld2, temp=temp, vel_obs=vel_obs,
      ftt_obs=ftt_obs, trans=trans, det=det, stray=stray)#, m_ftt=m_ftt, sd_ftt=sd_ftt)
  } else {
    n_ind<- nrow(fcs)
    out_dat<- list(det=det, n_ind=n_ind,
      juld=juld, juld2=juld2, temp=temp,
      vel=vel, trans=trans)
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
# surv vs. temp # temp range 10.97685 22.36957
surv_temp<- function(b, juld=mean(fcs$mca_jul), alpha=13, omega=23, lcol=c('cyan','lightpink'), lw=1){
  curve(plogis(b[1]+ b[2]*julScale(juld)+ b[3]*julScale2(juld^2)+ b[4]*tempScale(x)), alpha, omega, col=lcol[1], lwd=lw, add=TRUE)
  curve(plogis(b[1]+ b[2]*julScale(juld)+ b[3]*julScale2(juld^2)+ b[4]*tempScale(x)+ b[6]), alpha, omega, col=lcol[2], lwd=lw, add=TRUE)
}
# surv vs. ftt # ftt range 3.673646 81.597697
surv_ftt<- function(b, temp=mean(fcs$ihr_temp), alpha=3, omega=50, lcol=c('chartreuse','aquamarine'), lw=1){
  curve(plogis(b[1]+ b[4]*tempScale(temp)+ b[5]*x), alpha, omega, col=lcol[1], lwd=lw, add=TRUE)
  curve(plogis(b[1]+ b[4]*tempScale(temp+2)+ b[5]*x), alpha, omega, col=lcol[2], lwd=lw, add=TRUE)
}










