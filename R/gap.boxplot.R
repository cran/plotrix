gap.boxplot<-function(x,...,gap=list(top=c(NA,NA),bottom=c(NA,NA)),
 range=1.5,width=NULL,varwidth=FALSE,notch=FALSE,outline=TRUE,
 names,plot=TRUE,border=par("fg"),col=NULL,log="",
 pars=list(boxwex=0.8,staplewex=0.5,outwex=0.5),
 horizontal=FALSE,add=FALSE,at=NULL,main=NULL) {

 ### make sure that the gaps are in the right order
 if(gap$top[1]>gap$top[2]) gap$top<-rev(gap$top)
 if(gap$bottom[1]>gap$bottom[2]) gap$bottom<-rev(gap$bottom)
 # get the boxplot stats
 bxpt<-boxplot(x,...,range=range,plot=FALSE)
 # calculate the plot limits assuming no gaps
 ylim<-range(c(bxpt$stats,bxpt$out))
 bxgap<-bxpt
 # adjust for the top gap
 if(!is.na(gap$top[1])) {
  bxgap$stats[bxgap$stats>gap$top[1] & bxgap$stats<gap$top[2]]<-NA
  ### check for any NAs in the stats
  if(any(is.na(bxgap$stats)))
   stop("gap cannot include the median, interquartiles or the staples")
  bxgap$stats[bxgap$stats>gap$top[2]]<-
   bxgap$stats[bxgap$stats>gap$top[2]]-diff(gap$top)
  bxgap$out[bxgap$out>gap$top[1] & bxgap$out<gap$top[2]]<-NA
  bxgap$out[bxgap$out>gap$top[2]]<-
   bxgap$out[bxgap$out>gap$top[2]]-diff(gap$top)
  rangetop<-gap$top[1]
  ylim[2]<-max(c(bxgap$stats,bxgap$out),na.rm=TRUE)
 }
 else rangetop<-ylim[2]
 # adjust for the bottom gap
 if(!is.na(gap$bottom[1])) {
  bxgap$stats[bxgap$stats>gap$bottom[1] & bxgap$stats<gap$bottom[2]]<-NA
  ### check for any NAs in the stats
  if(any(is.na(bxgap$stats)))
   stop("gap cannot include the median, interquartiles or the staples")
  bxgap$stats[bxgap$stats<gap$bottom[1]]<-
   bxgap$stats[bxgap$stats<gap$bottom[1]]+diff(gap$bottom)
  bxgap$out[bxgap$out>gap$bottom[1] & bxgap$out<gap$bottom[2]]<-NA
  bxgap$out[bxgap$out<gap$bottom[1]]<-
   bxgap$out[bxgap$out<gap$bottom[2]]+diff(gap$bottom)
  rangebottom<-gap$bottom[2]
  ylim[1]<-min(c(bxgap$stats,bxgap$out),na.rm=TRUE)
 }
 else rangebottom<-ylim[1]
 if(any(is.na(bxgap$out)))
  warning("At least one outlier falls into a gap")
 nboxes<-dim(bxgap$stats)[2]
 plot(0,xlim=c(0.5,nboxes+0.5),ylim=ylim,type="n",axes=FALSE,
  xlab="",ylab="",main=main)
 plotlim<-par("usr")
 box()
 axis(1,labels=bxpt$names,at=1:nboxes)
 midticks<-pretty(c(rangebottom,rangetop))
 axis(2,at=midticks[midticks>rangebottom & midticks<rangetop])
 if(is.null(width)) width<-pars$boxwex
 rect(1:nboxes-width/2,bxgap$stats[2,],1:nboxes+width/2,bxgap$stats[4,],
  border=border,col=col)
 # notches
 if(notch) {
  ymult<-diff(plotlim[3:4])/diff(plotlim[1:2])
  if(is.null(col)) boxcol<-"white"
  else boxcol<-col
  rect(1:nboxes-width/1.95,bxgap$conf[1,],1:nboxes+width/1.95,bxgap$conf[2,],
   border=NA,col=boxcol)
  median.left<-1:nboxes+width*bxgap$conf[1]/(ymult*nboxes)-width/2
  median.right<-1:nboxes-width*bxgap$conf[1]/(ymult*nboxes)+width/2
  segments(1:nboxes-width/2,bxgap$conf[1,],median.left,bxgap$stats[3,],col=border)
  segments(1:nboxes-width/2,bxgap$conf[2,],median.left,bxgap$stats[3,],col=border)
  segments(median.right,bxgap$stats[3,],1:nboxes+width/2,bxgap$conf[1,],col=border)
  segments(median.right,bxgap$stats[3,],1:nboxes+width/2,bxgap$conf[2,],col=border)
 }
 else {
  median.left<-1:nboxes-width/2
  median.right<-1:nboxes+width/2
 }
 # median lines
 segments(median.left,bxgap$stats[3,],median.right,bxgap$stats[3,],
  lwd=2,col=border)
 # whiskers
 segments(1:nboxes,bxgap$stats[1,],1:nboxes,bxgap$stats[2,],lty=2,col=border)
 segments(1:nboxes,bxgap$stats[4,],1:nboxes,bxgap$stats[5,],lty=2,col=border)
 # staples?
 segments(1:nboxes-pars$staplewex*width/2,bxgap$stats[1,],
  1:nboxes+pars$staplewex*width/2,bxgap$stats[1,],col=border)
 segments(1:nboxes-pars$staplewex*width/2,bxgap$stats[5,],
  1:nboxes+pars$staplewex*width/2,bxgap$stats[5,],col=border)
 if(!is.na(gap$top[1])) {
  topadjust<-diff(gap$top)
  axis(2,label=floor(ylim[2]+topadjust),
   at=floor(ylim[2])+topadjust-floor(topadjust))
  axis.break(2,gap$top[1],style="gap")
 }
 if(!is.na(gap$bottom[1])) {
  bottomadjust<-diff(gap$bottom)
  axis(2,label=ceiling(ylim[1]-bottomadjust),
   at=ceiling(ylim[1])-(bottomadjust-floor(bottomadjust)))
  # put the bottom break below the upper gap limit
  axis.break(2,gap$bottom[2]-diff(plotlim[3:4])*0.02,style="gap")
 }
 # outliers
 points(bxgap$group,bxgap$out)
 invisible(bxgap)
}
