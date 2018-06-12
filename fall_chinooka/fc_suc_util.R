
# functions for fall chinooka analysis (mar 2018)

fc_load_dat<- function(wd){
  load(file=paste0(wd, "data_compile/fc_data/fcdat.Rdata"))
  fcs<- subset(fcdat, esu=='Snake') # limit comparison of transport to snake fish
  
  fcs$ftt<- with(fcs, as.numeric(mca_obs- boa_obs))
  fcs<- fcs[fcs$ftt>0|is.na(fcs$ftt),]
  fcs$vel<- 236/(fcs$ftt) # distance/day
  fcs$mca_det<- ifelse(is.na(fcs$mca_obs), 0, 1)
  fcs$upp_det<- ifelse(!is.na(fcs$iha_obs), 1,
    ifelse(is.na(fcs$gra_obs), 0, 1))
  fcs$tempbin<- cut(fcs$temp, breaks = c(7,17:24))
  fcs$tempbin2<- cut(fcs$temp, breaks = seq(7,24, by=1))
  
  fcs$temp_sca<- scale(fcs$temp)
  fcs$jul_sca<- scale(fcs$boa_jul)
  fcs$dis_sca<- scale(fcs$dis)
  fcs$temp2<- scale(fcs$temp^2)
  fcs$jul2<- scale(fcs$boa_jul^2)
  fcs$dis2<- scale(fcs$dis^2)
  fcs<- fcs[!is.na(fcs$temp),]
  
  return(fcs)
}

tempScale<- function(x) (x-mean(fcs$temp))/sd(fcs$temp)
tempScale2<- function(x) (x-mean(fcs$temp^2))/sd(fcs$temp^2)

julScale<- function(x) (x-mean(fcs$boa_jul))/sd(fcs$boa_jul)
julScale2<- function(x) (x-mean(fcs$boa_jul^2))/sd(fcs$boa_jul^2)

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

prep_dat<- function(fcs, typ='qua'){
  CH<- with(fcs, cbind(rep(1,nrow(fcs)), mca_det, upp_det))
  n_occ<- ncol(CH)
  n_ind<- nrow(CH)
  juld<- fcs$jul_sca
  temp<- fcs$temp_sca
  dis<- fcs$dis_sca
  trans<- ifelse(fcs$mig_his=='trans', 1, 0)
  yr<- fcs$boa_yr-2002
  # ftt<- fcs$ftt
  vel<- fcs$vel
  below20<- ifelse(fcs$temp<20, 1, 0)
  above21<- ifelse(fcs$temp>21, 1, 0)
  if(typ=='qua') {
    juld2<- scale(fcs$boa_jul^2)
    temp2<- scale(fcs$temp^2)
    out_dat<- list(y=CH, n_occ=n_occ, n_ind=n_ind, z=known_state_cjs(CH),
      juld=juld, temp=temp, dis=dis, trans=trans,
      yr=yr, vel=vel, juld2=juld2, temp2=temp2)
  } else {
    out_dat<- list(y=CH, n_occ=n_occ, n_ind=n_ind, z=known_state_cjs(CH),
      juld=juld, temp=temp, dis=dis, trans=trans, yr=yr, vel=vel)
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

# plot results
# surv vs. ftt
surv_ftt<- function(g, lcol='grey50', lw=1, alpha=3, omega=70){
  curve(exp(-(g[1]+ g[2]*x+ rnorm(1, 0, g[3])) ),
    alpha, omega, col=lcol, lwd=lw, add=TRUE)
} 
# surv vs. temp
surv_temp<- function(b, lcol=c('cyan','lightpink'), lw=1, alpha=18.62, omega=21.87){
  curve(exp(-b[1]-b[2]*(236/(b[3]+ b[4]*tempScale(x)+ b[5]*tempScale2(x^2)+ b[7]))), alpha, omega, col=lcol[1],lwd=lw, add=TRUE)
  curve(exp(-b[1]-b[2]*(236/(b[3]+ b[4]*tempScale(x)+ b[5]*tempScale2(x^2)+ b[6]+ b[7]))), alpha, omega, col=lcol[2], lwd=lw, add=TRUE)
} 
# ftt vs. temp (not in use)
ftt_temp<- function(b, lcol=c('cyan','lightpink'), lw=1, alpha=18.62, omega=21.87){
  curve(236/(b[1]+ b[2]*tempScale(x)+ b[3]*tempScale2(x^2)), alpha, omega, col=lcol[1],lwd=lw, add=TRUE)
  curve(236/(b[1]+ b[2]*tempScale(x)+ b[3]*tempScale2(x^2)+ b[4]), alpha, omega, col=lcol[2], lwd=lw, add=TRUE)
} 









