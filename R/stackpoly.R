stackpoly<-function(x,y=NULL,main="",xlab="",ylab="",xat=NA,xaxlab=NA,
 xlim=NA,ylim=NA,lty=1,border=NA,col=NA,staxx=FALSE,axis4=TRUE,...) {

 ydim<-dim(y)
 if(is.null(y[1])) {
  y<-x
  ydim<-dim(y)
  x<-matrix(rep(1:ydim[1],ydim[2]),ncol=ydim[2])
 }
 if(is.matrix(y) || is.list(y)) {
  if(is.na(xlim[1])) xlim<-range(x)
  if(is.na(ylim[1])) ylim<-c(0,max(y))
  plot(0,main=main,xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim,type="n",
   xaxs="i",yaxs="i",axes=FALSE,...)
  box()
  plotlim<-par("usr")
  if(is.na(xat[1])) {
   xat<-x[,1]
   if(is.na(xaxlab[1])) xaxlab<-xat
  }
  if(staxx) staxlab(at=xat,labels=xaxlab)
  else axis(1,at=xat,labels=xaxlab)
  axis(2)
  if(axis4) axis(4)
  if(is.na(col[1])) col=rainbow(ydim[2])
  else if(length(col)<ydim[2]) col<-rep(col,length.out=ydim[2])
  if(length(lty)<ydim[2]) lty<-rep(lty,length.out=ydim[2])
  for(pline in seq(ydim[2],1,by=-1)) {
   if(pline==1) {
    polygon(c(x[1],x[,pline],x[ydim[1]]),
     c(plotlim[3],y[,pline],plotlim[3]),
     border=border,col=col[pline],lty=lty[pline])
   }
   else
    polygon(c(x[,pline],rev(x[,pline-1])),c(y[,pline],rev(y[,pline-1])),
     border=border,col=col[pline],lty=lty[pline])
  }
 }
}
