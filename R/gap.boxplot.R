gap.boxplot<-function (x,...,gap=list(top=c(NA,NA),bottom=c(NA,NA)),
 range=1.5,width=NULL,varwidth=FALSE,notch=FALSE,outline=TRUE,names,
 ylim=NA,plot=TRUE,border=par("fg"),col=NULL,log="",axis.labels=NULL,
 pars=list(boxwex=0.8,staplewex=0.5,outwex=0.5), 
 horizontal=FALSE,add=FALSE,at=NULL,main=NULL) {

 if(!is.na(gap$top[1]))
  if(gap$top[1] > gap$top[2]) gap$top<-rev(gap$top)
 if(!is.na(gap$bottom[1]))
  if(gap$bottom[1] > gap$bottom[2]) gap$bottom<-rev(gap$bottom)
 if(is.na(ylim[1])) {
  bxpt<-boxplot(x,...,range=range,plot=FALSE)
  ylim<-range(c(bxpt$stats,bxpt$out))
 }
 else bxpt<-boxplot(x,...,ylim=ylim,range=range,plot=FALSE)
 bxgap<-bxpt
 if(!is.na(gap$top[1])) {
  bxgap$stats[bxgap$stats > gap$top[1] & bxgap$stats < gap$top[2]]<-NA
  if(any(is.na(bxgap$stats))) 
   stop("gap cannot include the median, interquartiles or the staples")
  topdiff<-diff(gap$top)
  bxgap$stats[bxgap$stats > gap$top[2]]<-
   bxgap$stats[bxgap$stats > gap$top[2]]-topdiff
  intopgap<-bxgap$out > gap$top[1] & bxgap$out < gap$top[2]
  bxgap$out[intopgap]<-NA
  abovetop<-which(bxgap$out > gap$top[2])
  bxgap$out[abovetop]<-bxgap$out[abovetop]-topdiff
  rangetop<-gap$top[1]
  ylim[2]<-ylim[2]-topdiff
 }
 else rangetop<-ylim[2]
 if(!is.na(gap$bottom[1])) {
  bxgap$stats[bxgap$stats > gap$bottom[1] & bxgap$stats < gap$bottom[2]] <- NA
  if(any(is.na(bxgap$stats))) 
   stop("gap cannot include the median, interquartiles or the staples")
  bottomdiff<-diff(gap$bottom)
  bxgap$stats[bxgap$stats < gap$bottom[1]]<-
   bxgap$stats[bxgap$stats < gap$bottom[1]]+bottomdiff
  bxgap$out[bxgap$out > gap$bottom[1] & bxgap$out < gap$bottom[2]] <- NA
  belowbottom<-which(bxgap$out < gap$bottom[1])
  bxgap$out[belowbottom]<-bxgap$out[belowbottom]+bottomdiff
  rangebottom<-gap$bottom[2]
  ylim[1]<-ylim[1]+bottomdiff
 }
 else rangebottom<-ylim[1]
 if(any(is.na(bxgap$out))) 
  warning("At least one outlier falls into a gap")
 nboxes<-dim(bxgap$stats)[2]
 plot(0,xlim=c(0.5,nboxes+0.5),ylim=ylim,type="n",
  axes=FALSE,xlab="",ylab="",main=main)
 plotlim<-par("usr")
 box()
 axis(1,labels=bxpt$names,at=1:nboxes)
 midticks<-pretty(c(rangebottom,rangetop))
 axis(2,at=midticks[midticks > rangebottom & midticks < rangetop])
 if(is.null(width)) width<-pars$boxwex
 rect(1:nboxes-width/2,bxgap$stats[2,],1:nboxes+width/2, 
  bxgap$stats[4,],border=border,col=col)
 if(notch) {
  ymult<-diff(plotlim[3:4])/diff(plotlim[1:2])
  if(is.null(col)) boxcol<-"white"
  else boxcol<-col
  rect(1:nboxes-width/1.95,bxgap$conf[1,],1:nboxes+width/1.95,
   bxgap$conf[2,],border=NA,col=boxcol)
  median.left<-1:nboxes+width*bxgap$conf[1]/(ymult*nboxes)-width/2
  median.right<-1:nboxes-width*bxgap$conf[1]/(ymult*nboxes)+width/2
  segments(1:nboxes-width/2,bxgap$conf[1,],median.left, 
   bxgap$stats[3,],col=border)
  segments(1:nboxes-width/2,bxgap$conf[2,],median.left, 
   bxgap$stats[3,],col=border)
  segments(median.right,bxgap$stats[3,],1:nboxes+width/2, 
   bxgap$conf[1,],col=border)
  segments(median.right,bxgap$stats[3,],1:nboxes+width/2, 
   bxgap$conf[2,],col=border)
 }
 else {
  median.left<-1:nboxes-width/2
  median.right<-1:nboxes+width/2
 }
 segments(median.left,bxgap$stats[3,],median.right,bxgap$stats[3,],
  lwd=2,col=border)
 segments(1:nboxes,bxgap$stats[1,],1:nboxes,bxgap$stats[2,],
  lty=2,col=border)
 segments(1:nboxes, bxgap$stats[4,],1:nboxes,bxgap$stats[5,],
  lty=2,col=border)
 segments(1:nboxes-pars$staplewex*width/2,bxgap$stats[1,],
  1:nboxes+pars$staplewex*width/2,bxgap$stats[1,],col = border)
 segments(1:nboxes-pars$staplewex*width/2,bxgap$stats[5,],
  1:nboxes+pars$staplewex*width/2,bxgap$stats[5,],col=border)
 if(!is.na(gap$top[1])) topadjust<-diff(gap$top)
 else topadjust<-0
 if(!is.na(gap$bottom[1])) bottomadjust<-diff(gap$bottom)
 else bottomadjust<-0
 if(!is.null(axis.labels)) axis(2,label=axis.labels,
  at=c(axis.labels[1]+bottomadjust,axis.labels[2]-topadjust))
 if(!is.na(gap$top[1])) axis.break(2,gap$top[1],style="gap")
 if(!is.na(gap$bottom[1]))
  axis.break(2,gap$bottom[2]-diff(plotlim[3:4])*0.02,style="gap")
 points(bxgap$group,bxgap$out)
 invisible(bxgap)
}
