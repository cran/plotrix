size_n_color<-function(x,y,size,col,main="",
 xlab="",xat=1:length(unique(x)),xaxlab=NULL,xcex=1,xlas=0,xgrid=FALSE,
 ylab="",yat=1:length(unique(y)),yaxlab=NULL,ycex=1,ylas=1,ygrid=TRUE,
 mar=c(5,4,4,2),boxit=TRUE,pch=19,add=FALSE,...) {

 if(!add) {
  oldmar<-par(mar=mar)
  plot(x,y,main=main,xlab=xlab,ylab=ylab,axes=FALSE,type="n",...)
  xylim<-par("usr")
  if(xgrid)
   segments(xat,xylim[3],xat,xylim[4],col="lightgray",lty=2)
  if(ygrid)
   segments(xylim[1],yat,xylim[2],yat,col="lightgray",lty=2)
 }
 points(x,y,cex=size,col=col,pch=pch)
 axis(1,at=xat,labels=xaxlab,las=xlas,cex.axis=xcex)
 axis(2,at=yat,labels=yaxlab,las=ylas,cex.axis=ycex)
 if(boxit) box()
 if(!add) par(mar=oldmar)
}
