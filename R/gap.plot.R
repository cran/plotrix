gap.plot<-function(x,y,gap,gap.axis="y",xaxlab,xtics=NA,yaxlab,ytics=NA,
 col=par("col"),xlim,ylim,axes=FALSE,...) {
 
 if(missing(y) && !missing(x)) {
  y<-x
  x<-1:length(y)
 }
 if(missing(gap)) stop("gap must be specified")
 gapsize<-gap[2]-gap[1]
 if(missing(xtics)) xtics<-pretty(x)
 if(missing(ytics)) ytics<-pretty(y)
 if(missing(xaxlab)) xaxlab<-xtics
 if(missing(yaxlab)) yaxlab<-ytics
 if(length(col) < length(y)) col<-rep(col,length.out=length(y))
 if(gap.axis == "y") {
  littleones<-which(y<=gap[1])
  bigones<-which(y>=gap[2])
  if(any(y > gap[1] & y < gap[2]))
   warning("gap includes some values of y")
  if(missing(xlim)) xlim<-range(x)
  if(missing(ylim)) ylim<-c(min(y),max(y)-gapsize)
 }
 else {
  littleones<-which(x<=gap[1])
  bigones<-which(x>=gap[2])
  if(any(x > gap[1] & x < gap[2]))
   warning("gap includes some values of x")
  if(missing(ylim)) ylim<-range(y)
  if(missing(xlim)) xlim<-c(min(x),max(x)-gapsize)
 }
 plot(x[littleones],y[littleones],xlim=xlim,ylim=ylim,axes=axes,
  col=col[littleones],...)
 box()
 if(gap.axis=="y") {
  if(!is.na(xtics[1])) axis(1,at=xtics,labels=xaxlab)
  littletics<-which(ytics<gap[1])
  bigtics<-which(ytics>=gap[2])
  axis(2,at=c(ytics[littletics],ytics[bigtics]-gapsize),
   labels=c(yaxlab[littletics],yaxlab[bigtics]))
  axis.break(2,gap[1],style="gap")
  points(x[bigones],y[bigones]-gapsize,col=col[bigones])
 }
 else {
  if(!is.na(ytics[1])) axis(2,at=ytics,labels=yaxlab)
  littletics<-which(xtics<gap[1])
  bigtics<-which(xtics>=gap[2])
  axis(1,at=c(xtics[littletics],xtics[bigtics]-gapsize),
   labels=c(xaxlab[littletics],xaxlab[bigtics]))
  axis.break(1,gap[1],style="gap")
  points(x[bigones]-gapsize,y[bigones],col=col[bigones],...)
 }
}
