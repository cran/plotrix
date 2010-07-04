kiteChart<-function(x,xlim=NA,ylim=NA,main="Kite chart",xlab="Time",ylab="Groups",
 fill=NA,border=par("fg"),varlabels=NA,timepos=NA,timelabels=NA,
 mar=c(5,4,4,4),normalize=TRUE,...) {

 oldmar<-par(mar=mar)
 dimx<-dim(x)
 if(is.na(xlim[1])) xlim=c(1,dimx[2])
 if(is.na(ylim[1])) ylim=c(0.5,dimx[1]+0.5)
 plot(0,xlim=xlim,ylim=ylim,main=main,xlab=xlab,ylab=ylab,type="n",axes=FALSE,...)
 if(is.na(varlabels[1])) {
  if(is.null(rownames(x))) varlabels<-1:dimx[1]
  else varlabels<-rownames(x)
 }
 axis(2,at=1:dimx[1],labels=varlabels)
 if(is.na(timepos[1])) timepos<-1:dimx[2]
 if(is.na(timelabels[1])) {
  if(is.null(colnames(x))) timelabels<-timepos
  else timelabels<-colnames(x)
 }
 axis(1,at=timepos,labels=timelabels)
 box()
 if(is.na(fill[1])) fill<-rainbow(dimx[1])
 for(krow in 1:dimx[1]) {
  if(normalize) {
   mtext(paste("*",signif(1/max(x[krow,]),digits=3)),4,at=krow,las=1)
   x[krow,]<-x[krow,]/(max(x[krow,])*2)
  }
  dispersion(1:dimx[2],rep(krow,dimx[2]),x[krow,],type="l",fill=fill[krow])
 }
 invisible(oldmar)
}