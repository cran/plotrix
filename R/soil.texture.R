get.soil.texture<-function(use.percentages=FALSE,cnames=c("sand","silt","clay")) {
 return(get.triprop(use.percentages=use.percentages,cnames=cnames))
}

soil.texture<-function(soiltexture=NULL,main="",
 at=list(l=seq(0.1,0.9,by=0.1),r=seq(0.1,0.9,by=0.1),b=seq(0.1,0.9,by=0.1)),
 axis.labels=c("percent clay","percent silt","percent sand"),
 tick.labels=list(l=seq(10,90,by=10),r=seq(10,90,by=10),b=seq(10,90,by=10)),
 show.names=TRUE,show.lines=TRUE,col.names="gray",bg.names=par("bg"),
 show.grid=FALSE,col.axis="black",col.lines="gray",col.grid="gray",
 lty.grid=3,show.legend=FALSE,label.points=FALSE,point.labels=NULL,
 col.symbols="black",pch=par("pch"),...) {

 par(xpd=TRUE)
 currentfg<-par("fg")
 plot(0.5,type="n",axes=FALSE,xlim=c(0,1.1),ylim=c(0,1),main="",xlab="",ylab="")
 triax.frame(main=main,at=at,axis.labels=axis.labels,tick.labels=tick.labels,
  col.axis=col.axis,show.grid=show.grid,col.grid=col.grid,lty.grid=lty.grid)
 arrows(0.12,0.41,0.22,0.57,length=0.15)
 arrows(0.78,0.57,0.88,0.41,length=0.15)
 arrows(0.6,-0.1,0.38,-0.1,length=0.15)
 sin60<-sin(pi/3)
 if(show.lines) {
  # boundary of clay with extensions
  x1<-c(0.275,0.355,0.6)
  x2<-c(0.42,0.8,0.7)
  y1<-c(0.55*sin60,0.4*sin60,0.4*sin60)
  y2<-c(0.28*sin60,0.4*sin60,0.6*sin60)
  segments(x1,y1,x2,y2,col=col.lines)
  # lower bound of clay loam & silty divider
  x1<-c(0.42,0.66)
  x2<-c(0.86,0.6)
  y1<-c(0.28*sin60,0.28*sin60)
  y2<-c(0.28*sin60,0.4*sin60)
  segments(x1,y1,x2,y2,col=col.lines)
  x1<-c(0.175,0.1,0.38)
  x2<-c(0.383,0.38,0.42)
  y1<-c(0.35*sin60,0.2*sin60,0.2*sin60)
  y2<-c(0.35*sin60,0.2*sin60,0.28*sin60)
  segments(x1,y1,x2,y2,col=col.lines)
  # sand corner
  x1<-c(0.05,0.075)
  x2<-c(0.12,0.3)
  y1<-c(0.1*sin60,0.15*sin60)
  y2<-c(0,0)
  segments(x1,y1,x2,y2,col=col.lines)
  x1<-c(0.38,0.44,0.5,0.8,0.86)
  x2<-c(0.44,0.54,0.64,0.86,0.94)
  y1<-c(0.2*sin60,0.078*sin60,0,0,0.12*sin60)
  y2<-c(0.078*sin60,0.078*sin60,0.28*sin60,0.12*sin60,0.12*sin60)
  segments(x1,y1,x2,y2,col=col.lines)
 }
 if(show.names) {
  xpos<-c(0.5,0.7,0.7,0.73,0.73,0.5,0.275,0.275,0.27,0.27,0.25,0.127,
   0.155,0.055,0.49,0.72,0.9)
  ypos<-c(0.57,0.49*sin60,0.44*sin60,0.36*sin60,0.32*sin60,
   0.35*sin60,0.43*sin60,0.39*sin60,0.3*sin60,0.26*sin60,
   0.13*sin60,0.072*sin60,0.032*sin60,0.021,0.18*sin60,
   0.15*sin60,0.06*sin60)
  snames<-c("clay","silty","clay","silty clay","loam","clay loam",
   "sandy","clay","sandy clay","loam","sandy loam","loamy","sand",
   "sand","loam","silt loam","silt")
  boxed.labels(xpos,ypos,snames,border=FALSE,col=col.names,xpad=0.5)
 }
 par(xpd=FALSE)
 # now call triax.points
 if(is.null(soiltexture)) return(NULL)
 soilpoints<-triax.points(soiltexture,show.legend=show.legend,
  label.points=label.points,point.labels=point.labels,
  col.symbols=col.symbols,pch=pch,...)
 invisible(soilpoints)
}
