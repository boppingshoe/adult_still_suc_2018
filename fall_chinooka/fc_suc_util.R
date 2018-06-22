
# functions for fall chinooka analysis (mar 2018)

fc_load_dat<- function(wd){
  load(file=paste0(wd, "data_compile/fc_data/fclsdat.Rdata"))
  fcs<- subset(fclsdat, esu=='Snake') # limit comparison of transport to snake fish
  
  # fcs$ftt<- with(fcs, as.numeric(mca_obs- boa_obs))
  fcs$ftt<- with(fcs, as.numeric(gra_obs- mca_obs))
  fcs<- fcs[fcs$ftt>0|is.na(fcs$ftt),]
  
  # fcs$vel<- 236/(fcs$ftt) # distance/day (bon-mcn=236, bon-ihr=304)
  # fcs$mca_det<- ifelse(is.na(fcs$mca_obs), 0, 1)
  # fcs$upp_det<- ifelse(!is.na(fcs$iha_obs), 1,
  #   ifelse(is.na(fcs$gra_obs), 0, 1))
  
  fcs$vel<- 225/(fcs$ftt) # distance/day (mcn-lgr 522+173- 470)
  # fcs$low_det<- ifelse(!is.na(fcs$mca_obs), 1,
  #   ifelse(is.na(fcs$iha_obs), 0, 1))
  fcs$iha_det<- ifelse(is.na(fcs$iha_obs), 0, 1)
  fcs$gra_det<- ifelse(is.na(fcs$gra_obs), 0, 1)

  fcs$temp_sca<- scale(fcs$ihr_temp)
  fcs$jul_sca<- scale(fcs$mca_jul)
  fcs$dis_sca<- scale(fcs$ihr_dis)
  fcs$temp2<- scale(fcs$ihr_temp^2)
  fcs$jul2<- scale(fcs$mca_jul^2)
  fcs$dis2<- scale(fcs$ihr_dis^2)
  fcs<- fcs[!is.na(fcs$ihr_temp),]
  
  return(fcs)
}

tempScale<- function(x) (x- mean(fcs$ihr_temp))/ sd(fcs$ihr_temp)
tempScale2<- function(x) (x- mean(fcs$ihr_temp^2))/ sd(fcs$ihr_temp^2)

julScale<- function(x) (x- mean(fcs$mca_jul))/ sd(fcs$mca_jul)

fttScale<- function(x) (x- 7.88)/ 4.29

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

# CJS stuff
known_state_cjs<- function(ch){
  state<- ch
  for (i in 1:dim(ch)[1]){
    n1<- min(which(ch[i,]==1))
    n2<- max(which(ch[i,]==1))
    state[i,n1:n2]<- 1
    state[i,n1]<- NA
  }
  state[state==0]<- NA
  return(state)
}

cjs_init_z<- function(ch){
  for(i in 1:dim(ch)[1]){
    if(sum(ch[i,])==1) next
    n2<- max(which(ch[i,]==1))
    ch[i,1:n2]<- NA
  }
  for (i in 1:dim(ch)[1]){
    ch[i,1]<- NA
  }
  return(ch)
}

prep_dat<- function(fcs, typ='w/cjs'){
  n_ind<- nrow(fcs)
  juld<- as.vector(fcs$jul_sca)
  temp<- as.vector(fcs$temp_sca)
  juld2<- as.vector(scale(fcs$mca_jul^2))
  temp2<- as.vector(scale(fcs$ihr_temp^2))
  dis<- as.vector(fcs$dis_sca)
  trans<- ifelse(fcs$mig_his=='trans', 1, 0)
  yr<- fcs$boa_yr-2002
  # ftt<- fcs$ftt
  vel<- fcs$vel
  if(typ=='w/cjs') {
    # CH<- with(fcs, cbind(rep(1,nrow(fcs)), mca_det, upp_det))
    CH<- with(fcs, cbind(rep(1,nrow(fcs)), iha_det, gra_det))
    n_occ<- ncol(CH)
    out_dat<- list(y=CH, n_occ=n_occ, n_ind=n_ind, z=known_state_cjs(CH),
      juld=juld, temp=temp, dis=dis, trans=trans,
      yr=yr, vel=vel, juld2=juld2, temp2=temp2)
  } else {
    det<- fcs$gra_det
    out_dat<- list(det=det, n_ind=n_ind,
      juld=juld, temp=temp, dis=dis, trans=trans,
      yr=yr, vel=vel)#, juld2=juld2, temp2=temp2)
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
surv_temp<- function(b, alpha=13, omega=23, lcol=c('cyan','lightpink'), lw=1){
  curve(plogis(b[1]+ b[2]*tempScale(x)),
    alpha, omega, col=lcol[1], lwd=lw, add=TRUE)
  curve(plogis(b[1]+ b[2]*tempScale(x)+ b[3]),
    alpha, omega, col=lcol[2], lwd=lw, add=TRUE)
}
# surv vs. ftt
surv_ftt<- function(b, temp=mean(fcs$ihr_temp), alpha=3, omega=50, lcol=c('cyan','lightpink'), lw=1){
  curve(plogis(b[1]+ b[2]*tempScale(temp)+ b[3]*tempScale(temp)*fttScale(x)+ b[4]*fttScale(x)),
    alpha, omega, col=lcol[1], lwd=lw, add=TRUE)
  curve(plogis(b[1]+ b[2]*tempScale(temp)+ b[3]*tempScale(temp)*fttScale(x)+ b[4]*fttScale(x)+ b[5]), alpha, omega, col=lcol[2], lwd=lw, add=TRUE)
}

# plot results (intergrated model)
# surv vs. ftt
# surv_ftt<- function(g, lcol='grey50', lw=1, alpha=3, omega=70){
#   curve(exp(-(g[1]+ g[2]*x) ),
#     alpha, omega, col=lcol, lwd=lw, add=TRUE)
# } 
# # surv vs. temp
# surv_temp<- function(b, lcol=c('cyan','lightpink'), lw=1, alpha=18.62, omega=21.87, sig_v=10){
#   eps_v<- rnorm(1, 0, sig_v)
#   curve(b[1]*exp(-b[2]*(236/(b[3]+ b[4]*tempScale(x)+ b[5]*tempScale2(x^2)+ eps_v))), alpha, omega, col=lcol[1],lwd=lw, add=TRUE)
#   curve(b[1]*exp(-b[2]*(236/(b[3]+ b[4]*tempScale(x)+ b[5]*tempScale2(x^2)+ b[6]+ eps_v))), alpha, omega, col=lcol[2], lwd=lw, add=TRUE)
# } 
# # ftt vs. temp (not in use)
# ftt_temp<- function(b, lcol=c('cyan','lightpink'), lw=1, alpha=18.62, omega=21.87){
#   curve(236/(b[1]+ b[2]*tempScale(x)+ b[3]*tempScale2(x^2)), alpha, omega, col=lcol[1],lwd=lw, add=TRUE)
#   curve(236/(b[1]+ b[2]*tempScale(x)+ b[3]*tempScale2(x^2)+ b[4]), alpha, omega, col=lcol[2], lwd=lw, add=TRUE)
# } 









