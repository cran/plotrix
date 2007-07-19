gap.barplot<-function(y,gap,xaxlab,xtics,yaxlab,ytics,ylim=NA,ylab,col,...) {
 if(missing(y)) stop("y values required")
 x<-1:length(y)
 if(missing(gap)) stop("gap must be specified")
 if(missing(ylab)) ylab<-deparse(substitute(y))
 if(missing(col)) col<-color.gradient(c(0,1),c(0,1,0),c(1,0),length(y))
 littleones<-which(y<=gap[1])
 bigones<-which(y>=gap[2])
 if(any(y > gap[1] & y < gap[2]))
  warning("gap includes some values of y")
 gapsize<-gap[2]-gap[1]
 if(missing(xaxlab)) xaxlab<-as.character(x)
 xlim<-range(x)
 if(is.na(ylim[1])) ylim<-c(min(y),max(y)-gapsize)
 plot(0,xlim=xlim,ylim=ylim,ylab=ylab,axes=FALSE,type="n",...)
 box()
 axis(1,at=x,labels=xaxlab)
 if(missing(ytics)) ytics<-pretty(y)
 if(missing(yaxlab)) yaxlab<-ytics
 littletics<-which(ytics<gap[1])
 bigtics<-which(ytics>=gap[2])
 axis(2,at=c(ytics[littletics],ytics[bigtics]-gapsize),
  labels=c(ytics[littletics],ytics[bigtics]))
 halfwidth<-min(diff(x))/2
 plot.lim<-par("usr")
 y[bigones]<-y[bigones]-gapsize
 rect(x - halfwidth, plot.lim[3], x + halfwidth, y, col = col)
 axis.break(2,gap[1],style="gap")
 invisible(x)
}
