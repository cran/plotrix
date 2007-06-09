gap.plot<-function(x,y,gap,gap.axis="y",xticlab,xtics=NA,yticlab,ytics=NA,
 col=rep(par("col"),length(x)),xlim,ylim,pch=rep(1,length(x)),...) {

 if(missing(y) && !missing(x)) {
  y <- x
  x <- 1:length(y)
 }
 if (missing(gap)) stop("gap must be specified")
 gapsize <- diff(gap)
 if(missing(xtics)) xtics<-pretty(x)
 if(missing(ytics)) ytics<-pretty(y)
 if(missing(xticlab)) xticlab<-xtics
 if (missing(yticlab)) yticlab <- ytics
 if(length(col)<length(y)) col<-rep(col,length.out=length(y))
 if(gap.axis == "y") {
  littleones<-which(y <= gap[1])
  if(length(gapsize) > 2) {
   middleones<-which(y >= gap[2] & y <= gap[3])
   bigones<-which(y >= gap[4])
   lostones<-sum(c(y > gap[1] & y < gap[2],y > gap[3] & y < gap[4]))
   if(missing(ylim)) ylim<-c(min(y),max(y)-(gapsize[1]+gapsize[3]))
  }
  else {
   middleones<-NA
   bigones<-which(y >= gap[2])
   lostones<-sum(y > gap[1] & y < gap[2])
   if(missing(ylim)) ylim<-c(min(y),max(y)-gapsize[1])
  }
  if(lostones) warning("some values of y will not be displayed")
  if(missing(xlim)) xlim<-range(x)
 }
 else {
  littleones<-which(x <= gap[1])
  if(length(gapsize) > 2) {
   middleones<-which(x >= gap[2] & x <= gap[3])
   bigones<-which(x >= gap[4])
   lostones<-sum(c(x > gap[1] & x < gap[2],x > gap[3] & x < gap[4]))
   if(missing(xlim)) xlim<-c(min(x),max(x)-(gapsize[1]+gapsize[3]))
  }
  else {
   middleones<-NA
   bigones<-which(x >= gap[2])
   lostones<-sum(x > gap[1] & x < gap[2])
   if(missing(xlim)) xlim<-c(min(x),max(x)-gapsize[1])
  }
  if(lostones) warning("some values of x will not be displayed")
  if(missing(ylim)) ylim<-range(y)
 }
 if(length(pch) < length(x)) pch<-rep(pch,length.out=length(x))
 plot(x[littleones],y[littleones],xlim=xlim,ylim=ylim,axes=FALSE,
  col=col[littleones],pch=pch[littleones],...)
 box()
 if(gap.axis == "y") {
  if(!is.na(xtics[1])) axis(1,at=xtics,labels=xticlab)
  littletics<-which(ytics < gap[1])
  if(length(gapsize) > 2) {
   middletics<-which(ytics >= gap[2] & ytics <= gap[3])
   bigtics<-which(ytics >= gap[4])
   show.at<-c(ytics[littletics],ytics[middletics]-gapsize[1],
    ytics[bigtics]-(gapsize[1]+gapsize[3]))
   show.labels<-c(yticlab[littletics],yticlab[middletics],yticlab[bigtics])
  }
  else {
   bigtics<-which(ytics >= gap[2])
   show.at<-c(ytics[littletics],ytics[bigtics]-gapsize[1])
   show.labels<-c(ytics[littletics],yticlab[bigtics])
  }
  axis(2,at=show.at,labels=show.labels)
  axis.break(2,gap[1],style="gap")
  if(length(gapsize) > 2) {
   axis.break(2,gap[3]-gapsize[1],style="gap")
   points(x[middleones],y[middleones]-gapsize[1],col=col[middleones],
    pch=pch[middleones])
   points(x[bigones],y[bigones]-(gapsize[1]+gapsize[3]),col=col[bigones],
    pch=pch[bigones])
  }
  else
   points(x[bigones],y[bigones]-gapsize[1],col=col[bigones],pch=pch[bigones])
 }
 else {
  if(!is.na(ytics[1])) axis(2,at=ytics,labels=yticlab)
   littletics<-which(xtics < gap[1])
  if(length(gapsize) > 2) {
   middletics<-which(xtics >= gap[2] & xtics <= gap[3])
   bigtics<-which(xtics >= gap[4])
   show.at<-c(xtics[littletics],xtics[middletics]-gapsize[1],
    xtics[bigtics]-(gapsize[1]+gapsize[3]))
   show.labels<-c(xticlab[littletics],xticlab[middletics],xticlab[bigtics])
  }
  else {
   bigtics<-which(xtics >= gap[2])
   show.at<-c(xtics[littletics],xtics[bigtics]-gapsize[1])
   show.labels<-c(xticlab[littletics],xticlab[bigtics])
  }
  axis(1,at=show.at,labels=show.labels)
  axis.break(1,gap[1],style="gap")
  if(length(gapsize) > 2) {
   axis.break(1,gap[3]-gapsize[1],style="gap")
   points(x[middleones]-gapsize[1],y[middleones],col=col[middleones],
    pch=pch[middleones])
   points(x[bigones]-(gapsize[1]+gapsize[3]),y[bigones],col=col[bigones],
    pch=pch[bigones])
  }
  else
   points(x[bigones]-gapsize[1],y[bigones],col=col[bigones],pch=pch[bigones])
 }
}
