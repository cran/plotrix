dispbars<-function(x,y,ulim,llim=ulim,ddir="y",arrow.cap=0.02,
 arrow.gap=0.02,...) {

 length<-arrow.cap*par("pin")[ifelse(ddir=="y",2,1)]
 if(ddir=="y") {
  x0<-x1<-rep(x,2)
  arrow.gap<-arrow.gap*diff(par("usr")[3:4])
  y0<-c(y+arrow.gap,y-arrow.gap)
  y1<-c(y+ulim,y-llim)
 }
 else {
  y0<-y1<-rep(y,2)
  arrow.gap<-arrow.gap*diff(par("usr")[1:2])
  x0<-c(x+arrow.gap,x-arrow.gap)
  x1<-c(x+ulim,x-llim)
 }
 arrows(x0,y0,x1,y1,length=length,angle=90,...)
}

brkdn.plot<-function(vars,groups=NA,obs=NA,data,mct="mean",md="std.error",
 stagger=NA,dispbar=TRUE,main="Breakdown plot",xlab=NA,ylab=NA,xaxlab=NA,
 ylim=NA,type="b",pch=1,lty=1,col=par("fg"),...) {

 if(is.na(obs)) {
  if(is.na(groups))
   stop("Must have at least one factor to subset data")
  bygroup<-as.factor(data[[groups]])
  grouplevels<-levels(bygroup)
  ngroups<-length(grouplevels)
  nobs<-length(vars)
  obslevels<-1:nobs
 }
 else {
  byobs<-as.factor(data[[obs]])
  obslevels<-levels(byobs)
  nobs<-length(obslevels)
  if(is.na(groups)) {
   ngroups<-length(vars)
   grouplevels<-1:ngroups
  }
  else {
   bygroup<-as.factor(data[[groups]])
   grouplevels<-levels(bygroup)
   ngroups<-length(grouplevels)
   if(length(vars) > 1) {
    warning("Group and observation factors are present, only vars[1] is plotted")
    vars<-vars[1]
   }
  }
 }
 brkdn<-list(matrix(NA,nrow=ngroups,ncol=nobs),
  matrix(NA,nrow=ngroups,ncol=nobs))
 if(is.na(groups)) {
  if(is.na(xlab)) xlab<-"Observation"
  xat<-1:nobs
  if(is.na(xaxlab[1])) xaxlab<-obslevels
  for(group in 1:ngroups) {
   for(ob in 1:nobs) {
    brkdn[[1]][group,ob]<-
     do.call(mct,list(unlist(subset(data[[vars[group]]],
      data[[obs]] == obslevels[ob],vars[group])),na.rm=TRUE))
    if(!is.na(md))
     brkdn[[2]][group,ob]<-
      do.call(md,list(unlist(subset(data[[vars[group]]],
       data[[obs]] == obslevels[ob],vars[group])),na.rm=TRUE))
   }
  }
 }
 else {
  if(is.na(obs)) {
   if(is.na(xlab)) xlab<-"Variable"
   xat<-1:length(vars)
   if(is.na(xaxlab[1])) xaxlab<-vars
   for(group in 1:ngroups) {
    for(ob in 1:nobs) {
     brkdn[[1]][group,ob]<-
      do.call(mct,list(unlist(subset(data[[vars[ob]]],
       data[[groups]] == grouplevels[group],vars[ob])),na.rm=TRUE))
     if(!is.na(md))
      brkdn[[2]][group,ob]<-
       do.call(md,list(unlist(subset(data[[vars[ob]]],
        data[[groups]] == grouplevels[group],vars[ob])),na.rm=TRUE))
    }
   }
  }
  else {
   if(is.na(xlab)) xlab<-"Observation"
   xat<-1:nobs
   if(is.na(xaxlab[1])) xaxlab<-obslevels
   for(group in 1:ngroups) {
    for(ob in 1:nobs) {
     brkdn[[1]][group,ob]<-
      do.call(mct,list(unlist(subset(data,data[[groups]] == grouplevels[group] &
       data[[obs]] == obslevels[ob],vars)),na.rm=TRUE))
     if(!is.na(md))
      brkdn[[2]][group,ob]<-
       do.call(md,list(unlist(subset(data,data[[groups]] == grouplevels[group] &
        data[[obs]] == obslevels[ob],vars)),na.rm=TRUE))
    }
   }
  }
 }
 if(is.na(ylim[1])) {
  if(is.na(md)) ylim<-range(brkdn[[1]])
  else
   ylim<-c(min(brkdn[[1]]-brkdn[[2]],na.rm=TRUE),
    max(brkdn[[1]]+brkdn[[2]],na.rm=TRUE))
 }
 groupdiv<-ifelse(ngroups < 3,1,ngroups-2)
 if(is.na(stagger)) stagger<-0.025-groupdiv*0.0025
 if(is.na(ylab)) {
  if(length(vars) == 1) ylab<-vars[1]
  else ylab<-paste(vars,collapse=" and ")
 }
 plot(0,xlim=c(0.5,nobs+0.5),main=main,xlab=xlab,ylab=ylab,ylim=ylim,
  type="n",axes=FALSE,...)
 box()
 axis(1,at=xat,labels=xaxlab)
 axis(2)
 if(length(pch) < ngroups) pch<-rep(pch,length.out=ngroups)
 if(length(col) < ngroups) col<-rep(col,length.out=ngroups)
 offinc<-stagger*diff(par("usr")[c(1,2)])
 offset<-0
 arrow.cap<-0.01-(groupdiv*0.001)
 for(group in 1:ngroups) {
  points(1:nobs+offset,brkdn[[1]][group,],type=type,col=col[group],
   pch=pch[group])
  if(dispbar)
   dispbars(1:nobs+offset,brkdn[[1]][group,],brkdn[[2]][group,],
    arrow.cap=arrow.cap,col=col[group])
  offset<-ifelse(offset<0,-offset,-offset-offinc)
 }
 names(brkdn)<-c(mct,md)
 return(brkdn)
}
