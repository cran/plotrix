twoord.plot<-function (lx,ly,rx,ry,data=NULL,xlim=NULL,lylim=NULL,rylim=NULL,
 mar=c(5,4,4,4),lcol=1,rcol=2,xlab="",ylab="",rylab="",lpch=1,rpch=2,type="b",
 halfwidth=0.4,...) {

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
 if(is.null(lylim)) lylim<-range(ly)
 if(length(type) < 2) type<-rep(type,2)
 if(match(type[1],"bar",0)) {
  plot(0,type="n",xlab=xlab,ylab="",axes=FALSE,xlim=xlim,ylim=lylim,...)
  xbottom<-par("usr")[1]
  if(lylim[1] < 0) abline(h=0,lty=2)
  rect(lx-halfwidth,ifelse(ly<0,ly,xbottom),lx+halfwidth,ifelse(ly>0,ly,0),
   col=lcol)
 }
 else
  plot(lx,ly,xlim=xlim,ylim=lylim,xlab=xlab,ylab="",col=lcol,pch=lpch,
   type=type[1],axes=FALSE,...)
 xylim<-par("usr")
 mtext(ylab,2,2,col=lcol)
 box()
 axis(1)
 axat<-axis(2,col=ifelse(is.na(lcol),1,lcol),labels=FALSE)
 abline(v=xylim[1],col=rcol)
 mtext(axat,2,1,at=axat,col=lcol)
 par(new=TRUE)
 if(is.null(rylim)) rylim<-range(ry)
 if(match(type[2],"bar",0)) {
  plot(0,type="n",xlab=xlab,ylab="",axes=FALSE,xlim=xlim,ylim=rylim,...)
  xbottom<-par("usr")[1]
  if(rylim[1] < 0) abline("h",0)
  rect(rx-halfwidth,ifelse(ry < 0,ry,xbottom),rx+halfwidth,ifelse(ry > 0,ry,0),
   col=rcol)
 }
 else
  plot(rx,ry,xlim=xlim,ylim=rylim,xlab="",ylab="",col=rcol,pch=rpch,
   type=type[2],axes=FALSE,...)
 axat<-axis(4,col=ifelse(is.na(rcol),1,rcol),labels=FALSE)
 abline(v=xylim[2],col=rcol)
 mtext(axat,4,1,at=axat,col=rcol)
 mtext(rylab,4,2,col=rcol)
 par(mar=oldmar,new=FALSE)
}
