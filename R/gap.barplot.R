gap.barplot<-function (y,gap,xaxlab,xtics,yaxlab,ytics,ylim=NA,xlab=NULL,ylab=NULL,
 horiz=FALSE,col=NULL,...) {
 if (missing(y)) stop("y values required")
 x <- 1:length(y)
 if (missing(gap)) stop("gap must be specified")
 if (is.null(ylab)) ylab <- deparse(substitute(y))
 if (is.null(col)) col <- color.gradient(c(0,1),c(0,1,0),c(1,0),length(y))
 else if(length(col) < length(y)) rep(col,length.out=length(y))
 littleones <- which(y <= gap[1])
 bigones <- which(y >= gap[2])
 if (any(y > gap[1] & y < gap[2])) warning("gap includes some values of y")
 gapsize <- gap[2] - gap[1]
 if (missing(xaxlab)) xaxlab <- as.character(x)
 xlim <- c(min(x)-0.4,max(x)+0.4)
 if (is.na(ylim[1])) ylim <- c(min(y),max(y) - gapsize)
 if (missing(ytics)) ytics <- pretty(y)
 if (missing(yaxlab)) yaxlab <- ytics
 littletics <- which(ytics < gap[1])
 bigtics <- which(ytics >= gap[2])
 halfwidth <- min(diff(x))/2
 if(horiz) {
  if(!is.null(xlab)) {
   tmplab<-xlab
   xlab<-ylab
   ylab<-tmplab
  }
  plot(0,xlim=ylim,ylim=xlim,xlab=xlab,ylab=ylab,axes=FALSE,type="n",...)
  plot.lim <- par("usr")
  botgap<-ifelse(gap[1]<0,gap[1],plot.lim[1])
  box()
  axis(2,at=x,labels=xaxlab,...)
  axis(1,at=c(ytics[littletics],ytics[bigtics]-gapsize),
   labels=c(ytics[littletics],ytics[bigtics]),...)
  rect(botgap,x[y<gap[1]] - halfwidth,y[y<gap[1]],
   x[y<gap[1]] + halfwidth,col=col[y<gap[1]])
  rect(botgap,x[bigones] - halfwidth,y[bigones]-gapsize,
   x[bigones] + halfwidth,col=col[bigones])
  axis.break(1,gap[1],style="gap")
 }
 else {
  plot(0,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,axes=FALSE,type="n",...)
  plot.lim <- par("usr")
  botgap<-ifelse(gap[1]<0,gap[1],plot.lim[3])
  box()
  axis(1,at=x,labels=xaxlab,...)
  axis(2,at=c(ytics[littletics],ytics[bigtics] - gapsize),
   labels=c(ytics[littletics],ytics[bigtics]),...)
  rect(x[y<gap[1]] - halfwidth,botgap,x[y<gap[1]] + halfwidth,
   y[y<gap[1]],col=col[y<gap[1]])
  rect(x[bigones] - halfwidth,botgap,x[bigones] + halfwidth,
   y[bigones]-gapsize,col=col[bigones])
  axis.break(2,gap[1],style="gap")
 }
 invisible(x)
}
