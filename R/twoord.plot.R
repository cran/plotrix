twoord.plot<-function (lx,ly,rx,ry,data=NULL,xlim=NULL,lylim=NULL,rylim=NULL,
 mar=c(5,4,4,4),lcol=1,rcol=2,xlab="",ylab="",rylab="",lpch=1,rpch=2,type="b",
 xtickpos=NULL,xticklab=NULL,halfwidth=0.4,axislab.cex=1,...) {

 if(!is.null(data)) {
  ly<-data[ly]
  ry<-data[ry]
  if(missing(lx)) lx<-1:length(ly)
  else lx<-data[lx]
  if(missing(rx)) rx <- 1:length(ry)
  else rx<-data[rx]
 }
 if(missing(ry)) {
  if(missing(rx)) {
   rx<-1:length(ly)
   ry<-ly
   ly<-lx
   lx<-1:length(ly)
  }
  else {
   ry<-rx
   rx<-1:length(ry)
  }
 }
 oldmar<-par("mar")
 par(mar=mar)
 if(is.null(xlim)) xlim<-range(c(lx, rx))
 if(missing(lx)) lx<-1:length(ly)
 if(is.null(lylim)) lylim<-range(ly,na.rm=TRUE)
 if(length(type) < 2) type<-rep(type,2)
 if(match(type[1],"bar",0)) {
  plot(0,type="n",xlab=xlab,ylab="",yaxs="i",axes=FALSE,xlim=xlim,ylim=lylim,...)
  ybottom<-par("usr")[3]
  if(lylim[1] < 0) abline(h=0,lty=2)
  rect(lx-halfwidth,ifelse(ly<0,ly,ybottom),lx+halfwidth,ifelse(ly>0,ly,0),
   col=lcol)
 }
 else
  plot(lx,ly,xlim=xlim,ylim=lylim,xlab=xlab,ylab="",yaxs="i",col=lcol,pch=lpch,
   type=type[1],axes=FALSE,...)
 xylim<-par("usr")
 mtext(ylab,2,2,col=lcol,cex=axislab.cex)
 box()
 if(is.null(xticklab)) axis(1,cex=axislab.cex)
 else {
  if(is.null(xtickpos)) xtickpos<-1:length(xticklab)
  axis(1,at=xtickpos,labels=xticklab,cex=axislab.cex)
 }
 axat<-axis(2,col=ifelse(is.na(lcol),1,lcol),labels=FALSE)
 abline(v=xylim[1],col=lcol)
 mtext(axat,2,1,at=axat,col=lcol,cex=axislab.cex)
 par(new=TRUE)
 if(is.null(rylim)) rylim<-range(ry,na.rm=TRUE)
 if(match(type[2],"bar",0)) {
  plot(0,type="n",xlab=xlab,ylab="",yaxs="i",axes=FALSE,xlim=xlim,ylim=rylim,...)
  ybottom<-par("usr")[3]
  if(rylim[1] < 0) abline("h",0)
  rect(rx-halfwidth,ifelse(ry < 0,ry,ybottom),rx+halfwidth,ifelse(ry > 0,ry,0),
   col=rcol)
 }
 else
  plot(rx,ry,xlim=xlim,ylim=rylim,xlab="",ylab="",yaxs="i",col=rcol,pch=rpch,
   type=type[2],axes=FALSE,...)
 axat<-axis(4,col=ifelse(is.na(rcol),1,rcol),labels=FALSE)
 abline(v=xylim[2],col=rcol)
 mtext(axat,4,1,at=axat,col=rcol,cex=axislab.cex)
 mtext(rylab,4,2,col=rcol,cex=axislab.cex)
 par(mar=oldmar,new=FALSE)
}
