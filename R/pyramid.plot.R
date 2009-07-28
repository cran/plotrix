pyramid.plot<-function(xy,xx,labels=NA,top.labels=c("Male","Age","Female"),
 main="",laxlab=NULL,raxlab=NULL,unit="%",xycol,xxcol,gap=1,
 labelcex=1,mark.cat=NA,add=FALSE) {

 if(any(c(xy,xx)<0)) stop("Negative quantities not allowed")
 xydim<-dim(xy)
 if(is.na(labels[1])) labels<-1:xydim[1]
 ncats<-length(labels)
 if(is.null(xydim)) {
  if(length(xy) != ncats || length(xx) != ncats)
   stop("xy, xx and labels must all be the same length")
  halfwidth<-ceiling(max(c(xy,xx)))+gap
 }
 else {
  if(length(xy[,1]) != ncats || length(xx[,1]) != ncats)
   stop("xy, xx and labels must all be the same length")
  halfwidth<-ceiling(max(c(rowSums(xy),rowSums(xx))))+gap
 }
 oldmar<-par("mar")
 if(!add) {
  par(mar=c(4,2,4,2))
  plot(0,xlim=c(-halfwidth,halfwidth),ylim=c(0,ncats+1),
  type="n",axes=FALSE,xlab="",ylab="",xaxs="i",yaxs="i",main=main)
  if(is.null(laxlab)) laxlab<-seq(halfwidth-gap,0,by=-1)
  axis(1,at=-halfwidth:-gap,labels=laxlab)
  if(is.null(raxlab)) raxlab<-0:(halfwidth-gap)
  axis(1,at=gap:halfwidth,labels=raxlab)
  axis(2,at=1:ncats,labels=rep("",ncats),pos=gap,tcl=-0.25)
  axis(4,at=1:ncats,labels=rep("",ncats),pos=-gap,tcl=-0.25)
  if(!is.na(mark.cat)) boxed.labels(0,mark.cat,labels[mark.cat])
  text(0,1:ncats,labels,cex=labelcex)
  mtext(top.labels,3,0,at=c(-halfwidth/2,0,halfwidth/2),
   adj=0.5,cex=labelcex)
  mtext(c(unit,unit),1,2,at=c(-halfwidth/2,halfwidth/2))
 }
 if(is.null(xydim)) {
  if(missing(xycol)) xycol<-rainbow(ncats)
  if(missing(xxcol)) xxcol<-rainbow(ncats)
  rect(-(xy+gap),1:ncats-0.4,rep(-gap,ncats),1:ncats+0.4,col=xycol)
  rect(rep(gap,ncats),1:ncats-0.4,(xx+gap),1:ncats+0.4,col=xxcol)
 }
 else {
  if(missing(xycol)) xycol<-rainbow(xydim[2])
  if(missing(xxcol)) xxcol<-rainbow(xydim[2])
  xystart<-xxstart<-rep(gap,ncats)
  for(i in 1:xydim[2]) {
   xycolor<-rep(xycol[i],ncats)
   xxcolor<-rep(xxcol[i],ncats)
   rect(-(xy[,i]+xystart),1:ncats-0.4,-xystart,1:ncats+0.4,
    col=xycolor)
   rect(xxstart,1:ncats-0.4,xx[,i]+xxstart,1:ncats+0.4,
    col=xxcolor)
   xystart<-xy[,i]+xystart
   xxstart<-xx[,i]+xxstart
  }
 }
 return(oldmar)
}
