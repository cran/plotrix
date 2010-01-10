kiteChart<-function(x,xlim=NA,ylim=NA,timex=TRUE,main="Kite chart",
 xlab=ifelse(timex,"Time","Groups"),ylab=ifelse(timex,"Groups","Time"),
 fill=NULL,border=par("fg"),varlabels=NA,timepos=NA,timelabels=NA,
 mar=c(5,4,4,4),axlab=c(1,2,3,4),normalize=TRUE,shownorm=TRUE,...) {

 oldmar<-par(mar=mar)
 dimx<-dim(x)
 if(is.na(xlim[1])) {
  if(timex) xlim<-c(1,dimx[2])
  else xlim<-c(0.5,dimx[1]+0.5)
 }
 if(is.na(ylim[1])) {
  if(timex) ylim<-c(0.5,dimx[1]+0.5)
  else ylim<-c(1,dimx[2])
 }
 plot(0,xlim=xlim,ylim=ylim,main=main,xlab=xlab,ylab=ylab,type="n",axes=FALSE,...)
 if(is.na(varlabels[1])) {
  if(is.null(rownames(x))) varlabels<-1:dimx[1]
  else varlabels<-rownames(x)
 }
 axis(ifelse(timex,axlab[2],axlab[1]),at=1:dimx[1],labels=varlabels)
 if(is.na(timepos[1])) timepos<-1:dimx[2]
 if(is.na(timelabels[1])) {
  if(is.null(colnames(x))) timelabels<-timepos
  else timelabels<-colnames(x)
 }
 axis(ifelse(timex,axlab[1],axlab[2]),at=timepos,labels=timelabels)
 box()
 if(is.null(fill[1])) fill<-rainbow(dimx[1])
 if(length(fill) < dimx[1]) fill<-rep(fill,length.out=dimx[1])
 for(krow in 1:dimx[1]) {
  if(normalize) {
   if(shownorm)
    mtext(paste("*",signif(1/max(x[krow,]),digits=3)),ifelse(timex,axlab[4],axlab[3]),
     at=krow,las=1)
   x[krow,]<-x[krow,]/(max(x[krow,])*2)
  }
  xpos<-1:length(x[krow,])
  if(timex) polygon(c(xpos,rev(xpos)),c(krow+x[krow,],krow-rev(x[krow,])),
   col=fill[krow],border=border)
  else polygon(c(krow+x[krow,],krow-rev(x[krow,])),c(xpos,rev(xpos)),
   col=fill[krow],border=border)
 }
 invisible(oldmar)
}
