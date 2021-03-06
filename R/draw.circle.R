draw.circle<-function(x,y,radius,nv=100,border=NULL,col=NA,
 lty=1,density=NULL,angle=45,lwd = 1) {

 xylim<-par("usr")
 plotdim<-par("pin")
 ymult<-getYmult()
 angle.inc<-2*pi/nv
 angles<-seq(0,2*pi-angle.inc,by=angle.inc)
 if(length(col)<length(radius)) 
  col<-rep(col,length.out=length(radius))
 for(circle in 1:length(radius)) {
  xv<-cos(angles)*radius[circle]+x
  yv<-sin(angles)*radius[circle]*ymult+y
  polygon(xv,yv,border=border,col=col[circle],lty=lty,
   density=density,angle=angle,lwd=lwd)
 }
 invisible(list(x=xv,y=yv))
}
