bin.wind.records<-function(winddir,windspeed,ndir=8,radians=FALSE,
 speed.breaks=c(0,10,20,30)) {
 # the windagg matrix has wind speeds as rows and wind directions as columns
 windagg<-matrix(0,ncol=ndir,nrow=5)
 dir.breaks<-rep(0,ndir)
 if(radians) {
  angleinc<-2*pi/ndir
  dir.breaks[1]<-pi/ndir
 }
 else {
  angleinc<-360/ndir
  dir.breaks[1]<-180/ndir
 }
 for(i in 2:ndir) dir.breaks[i]<-dir.breaks[i-1]+angleinc
 nspeeds<-length(speed.breaks)+1
 for(i in 1:length(winddir)) {
  dir<-1
  while(winddir[i] > dir.breaks[dir] && dir < ndir) dir<-dir+1
  if(winddir[i] > dir.breaks[ndir]) dir<-1
  speed<-1
  while(windspeed[i] > speed.breaks[speed] && speed < nspeeds) speed<-speed+1
  windagg[speed,dir]<-windagg[speed,dir]+1
 }
 windagg<-100*windagg/sum(windagg)
 return(windagg)
}

draw.circle<-function(x,y,radius,nv=100,border=NULL,col=NA,lty=1,lwd=1) {
 xylim<-par("usr")
 plotdim<-par("pin")
 ymult<-(xylim[4]-xylim[3])/(xylim[2]-xylim[1])*plotdim[1]/plotdim[2]
 angle.inc<-2*pi/nv
 angles<-seq(0,2*pi-angle.inc,by=angle.inc)
 xv<-cos(angles)*radius+x
 yv<-sin(angles)*radius*ymult+y
 polygon(xv,yv,border=border,col=col,lty=lty,lwd=lwd)
 invisible(list(x=xv,y=yv))
}

oz.windrose.legend<-function(
 speed.col=c("#dab286","#fe9a66","#ce6733","#986434"),
 speed.width=c(0.2,0.4,0.6,0.8),legend.pos=27) {
  
 wdnames<-c("E","NE","N","NW","W","SW","S","SE")
 draw.circle(-14,legend.pos,2)
 angles<-seq(0,7*pi/4,by=pi/4)
 for(i in 1:8) {
  x<-cos(angles[i])*c(2,2.5,3.5)-14
  y<-sin(angles[i])*c(2,2.5,3.5)+legend.pos
  segments(x[1],y[1],x[2],y[2])
  text(x[3],y[3],wdnames[i],cex=0.7)
 }
 wsnames<-c("0-10","10-20","20-30","30+")
 draw.circle(-5,legend.pos,1)
 for(i in 1:length(speed.col)) {
  x<-c(i*7.5-11.5,i*7.5-4)
  y<-c(legend.pos-speed.width[i],legend.pos+speed.width[i])
  polygon(c(x[1],x[1],x[2],x[2]),c(y[1],y[2],y[2],y[1]),col=speed.col[i])
  text((x[1]+x[2])/2,legend.pos-1.5,wsnames[i],cex=0.7)
 }
 text(-5,legend.pos+1.6,"Calm")
 text(0,legend.pos+1.5,"km/h")
 text(12,legend.pos-3,"Scale factor = 30%")
}

oz.windrose<-function(windagg,
 speed.col=c("#dab286","#fe9a66","#ce6733","#986434"),
 speed.width=c(0.2,0.4,0.6,0.8),
 show.legend=TRUE,legend.pos=27,...) {
 
 oldmar<-par("mar")
 if(legend.pos>0) wrmar<-c(4,5,6,5)
 else wrmar<-c(6,5,4,5)
 par(mar=wrmar,xpd=TRUE)
 plot(0,xlim=c(-20,20),ylim=c(-20,20),type="n",axes=FALSE,xlab="",ylab="",...)
 winddim<-dim(windagg)
 calm.radius<-sum(windagg[1,])/winddim[2]
 draw.circle(0,0,10+calm.radius,border="gray")
 if(calm.radius<5) draw.circle(0,0,20+calm.radius,border="gray")
 boxed.labels(c(9.7+calm.radius,19.5+calm.radius),c(1.3,2),
  c("10%",ifelse(calm.radius<5,"20%","")),ypad=0.7,border=FALSE)
 draw.circle(0,0,calm.radius,border="gray")
 angle.inc<--2*pi/winddim[2]
 angles<-seq(5*pi/2,pi/2+angle.inc,by=angle.inc)
 for(i in 1:winddim[2]) {
  next.radius<-calm.radius
  for(j in 2:winddim[1]) {
   xinner<-cos(angles[i])*next.radius
   xouter<-cos(angles[i])*(windagg[j,i]+next.radius)
   yinner<-sin(angles[i])*next.radius
   youter<-sin(angles[i])*(windagg[j,i]+next.radius)
   next.radius<-sqrt(xouter*xouter+youter*youter)
   # find the four points for each rectangle
   xoffset<-cos(angles[i]-pi/2)*speed.width[j-1]
   yoffset<-sin(angles[i]-pi/2)*speed.width[j-1]
   polygon(c(xinner-xoffset,xinner+xoffset,xouter+xoffset,xouter-xoffset),
    c(yinner-yoffset,yinner+yoffset,youter+yoffset,youter-yoffset),
    col=speed.col[j-1])
  }
 }
 text(-25,5,paste("Calm ",sum(windagg[1,]),"%",sep="",collapse=""),col="blue")
 if(show.legend)
  oz.windrose.legend(speed.col=speed.col,speed.width=speed.width,
   legend.pos=legend.pos)
 par(oldmar)
}
