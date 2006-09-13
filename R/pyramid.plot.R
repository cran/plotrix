pyramid.plot<-function(xy,xx,labels,top.labels=c("Male","Age","Female"),
 main="",xycol,xxcol,gap=1) {

 ncats<-length(labels)
 if(length(xy) != ncats || length(xx) != ncats)
  stop("xy, xx and labels must all be the same length")
 par(mar=c(4,2,4,2))
 halfwidth<-ceiling(max(c(xy,xx)))+gap
 plot(0,xlim=c(-halfwidth,halfwidth),ylim=c(0,ncats+1),type="n",axes=FALSE,
  xlab="",ylab="",xaxs="i",yaxs="i",main=main)
 axis(1,at=-halfwidth:-gap,labels=seq(halfwidth-gap,0,by=-1))
 axis(1,at=gap:halfwidth,labels=0:(halfwidth-gap))
 axis(2,at=1:ncats,labels=rep("",ncats),pos=gap)
 axis(4,at=1:ncats,labels=rep("",ncats),pos=-gap)
 text(0,1:ncats,labels)
 rect(-(xy+gap),1:ncats-0.4,rep(-gap,ncats),1:ncats+0.4,col=xycol)
 rect(rep(gap,ncats),1:ncats-0.4,(xx+gap),1:ncats+0.4,col=xxcol)
 mtext(top.labels,3,0,at=c(-gap,0,gap),adj=c(1,0.5,0))
 mtext(c("%","%"),1,2,at=c(-halfwidth/2,halfwidth/2))
 par(mar=c(5,4,4,2))
}
