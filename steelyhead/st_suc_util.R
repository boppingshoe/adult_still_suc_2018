
# Utility functions for steelyhead analysis

load_dat_st<- function(wd){
  load(file= paste0(wd, "data_compile/st_data/stlsdat.Rdata"))
  k2<- subset(stlsdat, esu=='Snake' & stock!='B')
  sts<- subset(k2, as.numeric(format(mca_obs, '%m')) %in% c(6:9))
  sts$ftt<- with(sts, as.numeric(gra_obs- mca_obs))
  sts<- sts[sts$ftt>0|is.na(sts$ftt),]
  sts$vel<- 225/(sts$ftt) # distance/day (mcn-lgr 522+173- 470)
  
  sts$temp_sca<- scale(sts$ihr_temp)
  sts$jul_sca<- scale(sts$mca_jul)
  sts$dis_sca<- scale(sts$ihr_dis)
  sts$temp2<- scale(sts$ihr_temp^2)
  sts$jul2<- scale(sts$mca_jul^2)
  sts$dis2<- scale(sts$ihr_dis^2)
  sts<- sts[!is.na(sts$ihr_temp),]
  return(sts)
}

tempScale_st<- function(x) (x- mean(sts$ihr_temp))/ sd(sts$ihr_temp)
tempScale2_st<- function(x) (x- mean(sts$ihr_temp^2))/ sd(sts$ihr_temp^2)

julScale_st<- function(x) (x- mean(sts$mca_jul))/ sd(sts$mca_jul)
julScale2_st<- function(x) (x- mean(sts$mca_jul^2))/ sd(sts$mca_jul^2)

fttScale_st<- function(x) (x- 23.04)/ 35.04

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

# for JAGS stuff
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
    juld=juld, temp=temp, dis=dis, trans=trans,
    yr=yr, vel=vel, juld2=juld2, temp2=temp2)

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
surv_temp_st<- function(b, juld=mean(sts$mca_jul), alpha=13, omega=23, lcol=c('cyan','lightpink'), lw=1){
  curve(plogis(b[1]+ b[2]*julScale_st(juld)+ b[3]*julScale2_st(juld^2)+ b[4]*tempScale_st(x)), alpha, omega, col=lcol[1], lwd=lw, add=TRUE)
  curve(plogis(b[1]+ b[2]*julScale_st(juld)+ b[3]*julScale2_st(juld^2)+ b[4]*tempScale_st(x)+ b[5]), alpha, omega, col=lcol[2], lwd=lw, add=TRUE)
}
# surv vs. ftt # ftt range 3.673646 81.597697
surv_ftt_st<- function(b, temp=mean(sts$ihr_temp), alpha=3, omega=50, lcol=c('chartreuse','aquamarine'), lw=1){
  curve(plogis(b[1]+ b[2]*tempScale_st(temp)+ b[3]*fttScale_st(x)+ b[4]*tempScale_st(temp)*fttScale_st(x)), alpha, omega, col=lcol[1], lwd=lw, add=TRUE)
  curve(plogis(b[1]+ b[2]*tempScale_st(temp+2)+ b[3]*fttScale_st(x)+ b[4]*tempScale_st(temp+2)*fttScale_st(x)), alpha, omega, col=lcol[2], lwd=lw, add=TRUE)
}

