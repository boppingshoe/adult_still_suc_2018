
# functions for spring/summer chinooka analysis (june 2018)

ssc_load_dat<- function(wd){
  load(file=paste0(wd, "data_compile/ssc_data/ssclsdat.Rdata"))
  sscs<- subset(ssclsdat, esu=='Snake') # limit comparison of transport to snake fish
  
  sscs$ftt<- with(sscs, as.numeric(gra_obs- mca_obs))
  sscs<- sscs[(sscs$ftt>0& sscs$ftt<366)| is.na(sscs$ftt),]
  
  sscs$vel<- 225/(sscs$ftt) # distance/day (mcn-lgr 522+173- 470)
  sscs$iha_det<- ifelse(is.na(sscs$iha_obs), 0, 1)
  sscs$gra_det<- ifelse(is.na(sscs$gra_obs), 0, 1)

  sscs$temp_sca<- scale(sscs$ihr_temp)
  sscs$jul_sca<- scale(sscs$mca_jul)
  sscs$dis_sca<- scale(sscs$ihr_dis)
  sscs$temp2<- scale(sscs$ihr_temp^2)
  sscs$jul2<- scale(sscs$mca_jul^2)
  sscs$dis2<- scale(sscs$ihr_dis^2)
  sscs<- sscs[!is.na(sscs$ihr_temp),]
  
  return(sscs)
}

tempScale<- function(x) (x- mean(sscs$ihr_temp))/ sd(sscs$ihr_temp)
tempScale2<- function(x) (x- mean(sscs$ihr_temp^2))/ sd(sscs$ihr_temp^2)

julScale<- function(x) (x- mean(sscs$mca_jul))/ sd(sscs$mca_jul)
julScale2<- function(x) (x -mean(sscs$mca_jul^2))/ sd(sscs$mca_jul^2)

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
prep_dat<- function(sscs){
  n_ind<- nrow(sscs)
  # juld<- as.vector(sscs$jul_sca)
  summer<- ifelse(sscs$run=='summer', 1, 0)
  temp<- as.vector(sscs$temp_sca)
  temp2<- as.vector(scale(sscs$ihr_temp^2))
  dis<- as.vector(sscs$dis_sca)
  trans<- ifelse(sscs$mig_his=='trans', 1, 0)
  yr<- sscs$mca_yr-2002
  vel<- sscs$vel
  det<- sscs$gra_det
  out_dat<- list(det=det, n_ind=n_ind, summer=summer,# juld=juld,
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
surv_temp<- function(b, alpha=8, omega=22, lcol=c('cyan','lightpink'), lw=1){
  curve(plogis(b[1]+ b[2]*tempScale(x)+ b[3]*tempScale2(x^2)),
    alpha, omega, col=lcol[1], lwd=lw, add=TRUE)
  curve(plogis(b[1]+ b[2]*tempScale(x)+ b[3]*tempScale2(x^2)+ b[4]),
    alpha, omega, col=lcol[2], lwd=lw, add=TRUE)
} 
# surv vs. ftt
surv_ftt<- function(b, alpha=3, omega=50, lcol=c('cyan','lightpink'), lw=1){
  curve(plogis(b[1]+ b[2]*tempScale2(mean(sscs$ihr_temp)^2)+ b[3]*x), alpha, omega, col=lcol[1], lwd=lw, add=TRUE)
  curve(plogis(b[1]+ b[2]*tempScale2(mean(sscs$ihr_temp)^2)+ b[3]*x+ b[4]), alpha, omega, col=lcol[2], lwd=lw, add=TRUE)
} 









