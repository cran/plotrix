box.heresy<-function(x,y,uinner,linner=uinner,ulim,llim=ulim,intervals=FALSE,
 arrow.cap=NA,pch=22,main="",xlab="",ylab="",xaxlab=NA,col="white",
 do.first=NULL,...) {

 if(missing(y)) {
  y<-x
  x<-1:length(y)
 }
 if(is.na(xaxlab)) xaxlab<-x
 if(intervals) {
  ulim<-y+ulim
  llim<-y-llim
 }
 if(length(x) > 1) xrange<-range(x)
 else xrange<-c(0.5,1.5)
 xspace<-diff(xrange)/10
 boxwidth<-diff(xrange)/(4*length(x))
 plot(x,y,xlim=c(xrange[1]-xspace,xrange[2]+xspace),ylim=range(c(llim,ulim)),
  main=main,xlab=xlab,ylab=ylab,type="n",xaxt="n")
 if(!is.null(do.first)) eval(parse(text=do.first))
 axis(1,at=x,labels=xaxlab)
 if(is.na(arrow.cap)) arrow.cap<-boxwidth/diff(par("usr")[1:2])
 dispersion(x,y,ulim,llim,intervals=FALSE,arrow.cap=arrow.cap,...)
 if(intervals) {
  uinner<-y+uinner
  linner<-y-linner
 }
 rect(x-boxwidth,linner,x+boxwidth,uinner,col=col)
 points(x,y,pch=pch)
}
