barp<-function(height,width=0.4,names.arg=NULL,legend.lab=NULL,legend.pos="e",
 col=NULL,border=par("fg"),main=NULL,xlab="",ylab="",xlim=NULL,ylim=NULL,
 staxx=FALSE,staxy=FALSE,height.at=NULL,height.lab=NULL,cex.axis=par("cex.axis"),
 cylindrical=FALSE,shadow=FALSE) {

 if(is.data.frame(height)) its_ok<-is.numeric(unlist(height))
 else its_ok<-is.numeric(height)
 if(!its_ok) stop("barp can only display bars with numeric heights")
 hdim<-dim(height)
 if(is.null(hdim)) {
  ngroups<-length(height)
  barcol=col
  barpinfo<-list(x=1:ngroups,y=height)
 }
 else {
  ngroups<-hdim[2]
  if(!is.matrix(col) && length(col)==hdim[1])
   barcol<-matrix(rep(col,each=ngroups),nrow=hdim[1],byrow=TRUE)
  else barcol<-col
  barpinfo<-list(x=matrix(rep(1:ngroups,each=hdim[1]),ncol=hdim[2]),
   y=as.matrix(height))
 }
 if(is.null(xlim)) xlim<-c(0.4,ngroups+0.6)
 negy<-any(height<0)
 if(is.null(ylim)) {
  if(negy) miny<-min(height)*1.05
  else miny<-0
  ylim<-c(miny,max(height)*1.05)
 }
 else miny<-ylim[1]
 plot(0,type="n",main=main,xlab=xlab,ylab=ylab,axes=FALSE,xlim=xlim,ylim=ylim,
  xaxs="i",yaxs="i")
 if(negy) abline(h=0)
 if(is.null(names.arg)) names.arg<-1:ngroups
 if(staxx) {
  axis(1,at=1:ngroups,labels=rep("",ngroups),cex.axis=cex.axis)
  staxlab(1,at=1:ngroups,labels=names.arg,cex=cex.axis)
 }
 else axis(1,at=1:ngroups,labels=names.arg,cex.axis=cex.axis)
 if(is.null(height.at)) height.at<-pretty(ylim)
 if(is.null(height.lab)) height.lab<-pretty(ylim)
 if(staxy) {
  axis(2,at=height.at,labels=rep("",length(height.lab)),cex.axis=cex.axis)
  staxlab(2,at=height.at,labels=height.lab,cex=cex.axis)
 }
 else axis(2,at=height.at,labels=height.lab,cex.axis=cex.axis)
 bottoms<-ifelse(negy,0,miny)
 if(is.null(hdim)) {
  if(shadow) {
   for(bar in 1:ngroups)
    polygon.shadow(c(bar-width,bar-width,bar+width,bar+width),
     c(bottoms,height[bar],height[bar],bottoms),
     offset=c(0.2*width,0.05*(height[bar]-ylim[2])))
  }
  if(cylindrical)
   cylindrect(1:ngroups-width,bottoms,1:ngroups+width,height,col=barcol)
  else rect(1:ngroups-width,bottoms,1:ngroups+width,height,col=barcol)
 }
 else {
  bottoms<-matrix(bottoms,nrow=hdim[1],ncol=hdim[2])
  for(subgroup in 1:hdim[1]) {
   if(shadow) {
    for(bar in 1:ngroups) {
     barleft<-bar-width+(subgroup-1)*2*width/hdim[1]
     barright<-barleft+2*width/hdim[1]
     polygon.shadow(c(barleft,barleft,barright,barright),
      c(bottoms[bar],height[subgroup,bar],height[subgroup,bar],bottoms[bar]),
      offset=c(0.2*width,0.05*(height[subgroup,bar]-ylim[2])))
    }
   }
   if(cylindrical)
    cylindrect(1:ngroups-width+(subgroup-1)*2*width/hdim[1],bottoms[subgroup,],
     1:ngroups-width+(subgroup)*2*width/hdim[1],height[subgroup,],
     col=barcol[subgroup,])
   else rect(1:ngroups-width+(subgroup-1)*2*width/hdim[1],bottoms[subgroup,],
    1:ngroups-width+(subgroup)*2*width/hdim[1],height[subgroup,],
    col=barcol[subgroup,])
  }
 }
 if(!is.null(legend.lab)) {
  if(is.na(legend.pos[1])) {
   cat("Click at the lower left corner of the legend\n")
   legend.pos<-locator(1)
   xjust<-yjust<-0
  }
  if(legend.pos[1] == "e") {
   legend.pos<-emptyspace(barpinfo,bars=TRUE)
   xjust<-yjust<-0.5
  }
  legend(legend.pos,legend=legend.lab,fill=col,xjust=xjust,yjust=yjust)
 }
 box()
 return(barpinfo)
}
