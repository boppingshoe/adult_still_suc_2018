
# functions for socky eye analysis (june 2018)

so_load_dat<- function(wd){
  load(file=paste0(wd, "data_compile/so_data/solsdat.Rdata"))
  sos<- subset(solsdat, esu=='snake') # limit comparison of transport to snake fish
  
  sos$ftt<- with(sos, as.numeric(gra_obs- mca_obs))
  sos<- sos[(sos$ftt>0& sos$ftt<366)| is.na(sos$ftt),]
  
  sos$vel<- 225/(sos$ftt) # distance/day (mcn-lgr 522+173- 470)
  sos$iha_det<- ifelse(is.na(sos$iha_obs), 0, 1)
  sos$gra_det<- ifelse(is.na(sos$gra_obs), 0, 1)

  sos$temp_sca<- scale(sos$ihr_temp)
  sos$jul_sca<- scale(sos$mca_jul)
  sos$dis_sca<- scale(sos$ihr_dis)
  sos$temp2<- scale(sos$ihr_temp^2)
  sos$jul2<- scale(sos$mca_jul^2)
  sos$dis2<- scale(sos$ihr_dis^2)
  sos<- sos[!is.na(sos$ihr_temp),]
  
  return(sos)
}

tempScale<- function(x) (x- mean(sos$ihr_temp))/ sd(sos$ihr_temp)
tempScale2<- function(x) (x- mean(sos$ihr_temp^2))/ sd(sos$ihr_temp^2)

julScale<- function(x) (x- mean(sos$mca_jul))/ sd(sos$mca_jul)
julScale2<- function(x) (x -mean(sos$mca_jul^2))/ sd(sos$mca_jul^2)

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
prep_dat<- function(sos){
  n_ind<- nrow(sos)
  juld<- as.vector(sos$jul_sca)
  temp<- as.vector(sos$temp_sca)
  temp2<- as.vector(scale(sos$ihr_temp^2))
  dis<- as.vector(sos$dis_sca)
  trans<- ifelse(sos$mig_his=='trans', 1, 0)
  yr<- sos$mca_yr-2002
  vel<- sos$vel
  det<- sos$gra_det
  out_dat<- list(det=det, n_ind=n_ind, juld=juld,
    temp=temp, temp2=temp2, dis=dis, trans=trans, yr=yr, vel=vel)

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
surv_temp<- function(b, alpha=13, omega=23, lcol=c('cyan','lightpink'), lw=1){
  curve(plogis(b[1]+ b[2]*tempScale(x)+ b[3]*tempScale2(x^2)),
    alpha, omega, col=lcol[1], lwd=lw, add=TRUE)
  curve(plogis(b[1]+ b[2]*tempScale(x)+ b[3]*tempScale2(x^2)+ b[4]),
    alpha, omega, col=lcol[2], lwd=lw, add=TRUE)
} 
# surv vs. ftt
surv_ftt<- function(b, alpha=4, omega=50, lcol=c('cyan','lightpink'), lw=1){
  curve(plogis(b[1]+ b[2]*tempScale2(mean(sos$ihr_temp)^2)+ b[3]*x), alpha, omega, col=lcol[1], lwd=lw, add=TRUE)
  curve(plogis(b[1]+ b[2]*tempScale2(mean(sos$ihr_temp)^2)+ b[3]*x+ b[4]), alpha, omega, col=lcol[2], lwd=lw, add=TRUE)
} 










