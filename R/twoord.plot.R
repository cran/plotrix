twoord.plot<-function(lx,ly,rx,ry,data=NULL,xlim=NULL,lylim=NULL, 
 rylim=NULL,mar=c(5,4,4,4),lcol=1,rcol=2,xlab="",ylab="",rylab="",
 lpch=1,rpch=2,type="b",xtickpos=NULL,xticklab=NULL,halfwidth=0.4,
 axislab.cex=1,do.first=NULL,...) {

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
 if(is.null(xlim)) xlim<-range(c(lx,rx))
 if(missing(lx)) lx<-1:length(ly)
 if(is.null(lylim)) {
  lylim<-range(ly,na.rm=TRUE)
  lyspan<-diff(lylim)
  lylim[2]<-lylim[2]+lyspan*0.04
  if(lylim[1] != 0) lylim[1]<-lylim[1]-lyspan*0.04
 }
 if(length(type) < 2) type<-rep(type,2)
 # first display the "left" plot
 if(match(type[1],"bar",0)) {
  plot(lx,ly,xlim=xlim,ylim=lylim,xlab=xlab,ylab="",yaxs="i",type="n", 
   axes=FALSE,...)
  if(!is.null(do.first)) eval(parse(text=do.first))
  ybottom<-par("usr")[3]
  if (lylim[1] < 0) abline(h=0,lty=2)
  rect(lx-halfwidth,ifelse(ly<0,ly,ybottom),lx+halfwidth,
   ifelse(ly>0,ly,0),col=lcol)
 }
 else {
  plot(lx,ly,xlim=xlim,ylim=lylim,xlab=xlab,ylab="",yaxs="i",type="n", 
   axes=FALSE,...)
  if(!is.null(do.first)) eval(parse(text=do.first))
  points(lx,ly,col=lcol,pch=lpch,type=type[1])
 }
 xylim<-par("usr")
 mtext(ylab,2,2,col=lcol,cex=axislab.cex)
 box()
 if(is.null(xticklab)) axis(1,cex=axislab.cex)
 else {
  if(is.null(xtickpos)) xtickpos<-1:length(xticklab)
  if(is.null(xticklab)) xticklab<-xtickpos
  axis(1,at=xtickpos,labels=xticklab,cex=axislab.cex)
 }
 # display the left axis
 axat<-axis(2,col=ifelse(is.na(lcol),1,lcol),labels=FALSE)
 abline(v=xylim[1],col=lcol)
 mtext(axat,2,1,at=axat,col=lcol,cex=axislab.cex)
 # get the "right" y limits
 if(is.null(rylim)) {
  rylim<-range(ry,na.rm=TRUE)
  ryspan<-diff(rylim)
  rylim[2]<-rylim[2]+ryspan*0.04
  if(rylim[1] != 0) rylim[1]<-rylim[1]-ryspan*0.04
 }
 # multiplier for the "right" y values
 ymult<-diff(lylim)/diff(rylim)
# offset for the "right" y values
 yoff<-lylim[1]-rylim[1]*ymult
 if(match(type[2],"bar",0)) {
  if(rylim[1] < 0) abline("h", 0)
  rect(rx-halfwidth,ifelse(ry<0,ry,rylim[1]*ymult+yoff),rx+halfwidth,
   ifelse(ry>0,ry*ymult+yoff,0),col=rcol)
 }
 else points(rx,ry*ymult+yoff,col=rcol,pch=rpch,type=type[2])
 axat<-pretty(rylim)
 if(min(axat) < rylim[1]) axat<-axat[-1]
 if(max(axat) > rylim[2]) axat<-axat[-length(axat)]
 abline(v=xylim[2],col=rcol)
 axis(4,at=axat*ymult+yoff,labels=rep("",length(axat)),col=rcol,
  cex=axislab.cex)
 mtext(axat,4,1,at = axat*ymult+yoff,col = rcol, cex = axislab.cex)
 mtext(rylab,4,2,col=rcol,cex=axislab.cex)
 par(mar=oldmar,new=FALSE)
}

