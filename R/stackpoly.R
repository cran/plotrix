stackpoly<-function(x,main="",xlab="",ylab="",xat=NA,xaxlab=NA,ylim=NA,
 lty=1,border=NA,col=NA,staxx=FALSE,axis4=TRUE,...) {

 if(is.matrix(x) || is.list(x)) {
  xdim<-dim(x)
  if(is.na(ylim)) ylim<-c(0,max(x))
  plot(0,main=main,xlab=xlab,ylab=ylab,xlim=c(1,xdim[1]),ylim=ylim,type="n",
   xaxs="i",yaxs="i",axes=FALSE,...)
  box()
  plotlim<-par("usr")
  if(is.na(xat)) xat<-1:xdim[1]
  if(staxx) staxlab(at=xat,labels=xaxlab)
  else axis(1)
  axis(2)
  if(is.na(col[1])) col=rainbow(xdim[2])
  else if(length(col)<xdim[2]) col<-rep(col,length.out=xdim[2])
  if(length(lty)<xdim[2]) lty<-rep(lty,length.out=xdim[2])
  for(pline in seq(xdim[2],1,by=-1)) {
   if(pline==1) {
    polygon(c(xat[1],xat,xat[xdim[1]]),
     c(plotlim[3],x[,pline],plotlim[3]),
     border=border,col=col[pline],lty=lty[pline])
   }
   else
    polygon(c(xat,rev(xat)),c(x[,pline],rev(x[,pline-1])),
     border=border,col=col[pline],lty=lty[pline])
  }
 }
}
