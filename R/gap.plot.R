# Try to rewrite this for an arbitrary number of gaps

gap.plot<-function(x,y,gap,gap.axis="y",bgcol="white",breakcol="black",
 brw=0.02,xlim,ylim,xticlab,xtics=NA,yticlab,ytics=NA,lty=rep(1,length(x)),
 col=rep(par("col"),length(x)),pch=rep(1,length(x)),add=FALSE,...) {

 if(missing(y) && !missing(x)) {
  y<-x
  x<-1:length(y)
 }
 if(missing(gap)) stop("gap must be specified")
 gapsize<-diff(gap)
 figxy <- par("usr")
 xaxl<-par("xlog")
 yaxl<-par("ylog")
 xgw<-(figxy[2]-figxy[1])*brw
 ygw<-(figxy[4]-figxy[3])*brw
 if(is.na(xtics[1])) xtics<-pretty(x)
 if(is.na(ytics[1])) ytics<-pretty(y)
 if(missing(xticlab)) xticlab<-xtics
 if(missing(yticlab)) yticlab<-ytics
 if(length(col) < length(y)) col<-rep(col,length.out=length(y))
 if(gap.axis == "y") {
  littleones<-which(y < gap[1])
  if(length(gapsize) > 2) {
   middleones<-which(y >= gap[2] & y < gap[3])
   bigones<-which(y >= gap[4])
   lostones<-sum(c(y > gap[1] & y < gap[2], y > gap[3] & y < gap[4]))
   if(missing(ylim)) ylim<-c(min(y),ygw*2+max(y)-(gapsize[1]+gapsize[3]))
   else ylim[2]<-ygw*2+ylim[2]-(gapsize[1]+gapsize[3])
  }
  else {
   middleones<-NA
   bigones<-which(y >= gap[2])
   lostones<-sum(y > gap[1] & y < gap[2])
   if(missing(ylim)) ylim<-c(min(y),max(y)-gapsize[1])
   else ylim[2]<-ygw+ylim[2]-gapsize[1]
  }
  if(lostones) warning("some values of y will not be displayed")
  if(missing(xlim)) xlim<-range(x)
 }
 else {
  littleones<-which(x < gap[1])
  if(length(gapsize) > 2) {
   middleones<-which(x >= gap[2] & x < gap[3])
   bigones<-which(x >= gap[4])
   lostones<-sum(c(x > gap[1] & x < gap[2], x > gap[3] & x < gap[4]))
   if(missing(xlim)) xlim<-c(min(x),xgw*2+max(x)-(gapsize[1]+gapsize[3]))
   else xlim[2]<-xgw*2+xlim[2]-(gapsize[1]+gapsize[3])
  }
  else {
   middleones<-NA
   bigones<-which(x >= gap[2])
   lostones<-sum(x > gap[1] & x < gap[2])
   if(missing(xlim)) xlim<-c(min(x),max(x)-gapsize[1])
   else xlim[2]<-xgw+xlim[2]-gapsize[1]
  }
  if(lostones) warning("some values of x will not be displayed")
  if(missing(ylim)) ylim<-range(y)
 }
 if(length(lty) < length(x)) lty<-rep(lty,length.out=length(x))
 if(length(col) < length(x)) col<-rep(col,length.out=length(x))
 if(length(pch) < length(x)) pch<-rep(pch,length.out=length(x))
 if(add) {
  points(x[littleones],y[littleones],lty=lty[littleones],
   col=col[littleones],pch=pch[littleones],...)
  if(gap.axis == "y") {
   if(length(gapsize) > 2) {
    points(x[middleones],y[middleones]-gapsize[1],
     lty=lty[middleones],col=col[middleones],pch=pch[middleones],...)
    points(x[bigones],y[bigones]-(gapsize[1]+gapsize[3]),
     lty=lty[bigones],col=col[bigones],pch=pch[bigones],...)
   }
   else points(x[bigones],y[bigones]-gapsize[1],
    lty=lty[bigones],col=col[bigones],pch=pch[bigones],...)
  }
  else {
   if(length(gapsize) > 2) {
    points(xgw+x[middleones]-gapsize[1],y[middleones],
     lty=lty[middleones],col=col[middleones],pch=pch[middleones],...)
    points(x[bigones]-(gapsize[1]+gapsize[3]),y[bigones],
     lty=lty[bigones],col=col[bigones],pch=pch[bigones],...)
   }
   else points(x[bigones]-gapsize[1],y[bigones],
    lty=lty[bigones],col=col[bigones],pch=pch[bigones],...)
  }
 }
 else {
  plot(x[littleones],y[littleones],xlim=xlim,ylim=ylim,axes=FALSE,
   lty=lty[littleones],col=col[littleones],pch=pch[littleones],...)
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
   axis.break(2,gap[1]-ygw,style="gap",bgcol=bgcol,
    breakcol=breakcol,brw=brw)
   if(length(gapsize) > 2) {
    axis.break(2,gap[3]-(gapsize[1]+ygw),style="gap",bgcol=bgcol,
     breakcol=breakcol,brw=brw)
    points(x[middleones],y[middleones]-gapsize[1],
     lty=lty[middleones],col=col[middleones],pch=pch[middleones],...)
    points(x[bigones],y[bigones]-(gapsize[1]+gapsize[3]),
     lty=lty[bigones],col=col[bigones],pch=pch[bigones],...)
   }
   else points(x[bigones],y[bigones]-gapsize[1],
    lty=lty[bigones],col=col[bigones],pch=pch[bigones],...)
  }
  else {
   if(!is.na(ytics[1])) axis(2,at=ytics,labels=yticlab)
   littletics<-which(xtics<gap[1])
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
   axis.break(1,gap[1]-xgw,style="gap")
   if(length(gapsize) > 2) {
    axis.break(1,gap[3]-(gapsize[1]+xgw),style="gap")
    points(xgw+x[middleones]-gapsize[1],y[middleones],
     lty=lty[middleones],col=col[middleones],pch=pch[middleones],...)
    points(x[bigones]-(gapsize[1]+gapsize[3]),y[bigones],
     lty=lty[bigones],col=col[bigones],pch=pch[bigones],...)
   }
   else points(x[bigones]-gapsize[1],y[bigones],
    lty=lty[bigones],col=col[bigones],pch=pch[bigones],...)
  }
 }
}
