twoord.plot<-function(lx,ly,rx,ry,data=NULL,xlim=NULL,lylim=NULL,rylim=NULL,
 mar=NULL,lcol=1,rcol=2,xlab="",ylab="",rylab="",lpch=1,rpch=2,
 type="b",...) {

 if(!is.null(data)) {
  ly<-data[ly]
  ry<-data[ry]
  if(missing(lx)) lx<-1:length(ly)
  else lx<-data[lx]
  if(missing(rx)) rx<-1:length(ry)
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
 if(is.null(mar)) mar<-c(5,4,4,4)
 oldmar<-par("mar")
 par(mar=mar)
 if(is.null(xlim)) xlim<-range(c(lx,rx))
 if(missing(lx)) lx<-1:length(ly)
 if(is.null(lylim)) lylim<-range(ly)
 plot(lx,ly,xlim=xlim,ylim=lylim,xlab=xlab,ylab="",col=lcol,pch=lpch,
  type=type,axes=FALSE,...)
 mtext(ylab,2,2,col=lcol)
 box()
 axis(1)
 axat<-axis(2,col=lcol,labels=FALSE)
 mtext(axat,2,1,at=axat,col=lcol)
 par(new=TRUE)
 if(is.null(rylim)) rylim<-range(ry)
 plot(rx,ry,xlim=xlim,ylim=rylim,xlab="",ylab="",col=rcol,pch=rpch,
  type=type,axes=FALSE,...)
 axat<-axis(4,col=rcol,labels=FALSE)
 mtext(axat,4,1,at=axat,col=rcol)
 mtext(rylab,4,2,col=rcol)
 par(mar=oldmar)
}
