pie3D.labels<-function(radialpos,radius=1,height=0.3,theta=pi/6, 
 labels,labelcol=par("fg"),labelcex=1.5,minsep=0.3){

 oldcex<-par("cex")
 nlab<-length(labels)
 par(cex=labelcex,xpd=TRUE)
 for (i in 1:nlab) {
  if(i < nlab) {
   labelsep<-radialpos[i+1] - radialpos[i]
   if(labelsep < minsep) {
    radialpos[i]<-radialpos[i]+(labelsep-minsep)/2
    radialpos[i+1]<-radialpos[i+1]-(labelsep-minsep)/2
   }
  }
  xpos <- 1.15 * radius * cos(radialpos[i])
  offset <- (radialpos[i] > pi && radialpos[i] < 2 * pi) * height
  ypos <- 1.2 * sin(radialpos[i]) * sin(theta) * radius - offset
  text(xpos,ypos,labels[i],col=labelcol,adj=c(0.5,as.numeric(ypos<0)))
 }
 par(cex=oldcex,xpd=FALSE)
}

draw.tilted.sector<-function(x=0,y=0,edges=100,radius=1,height=0.3,
 theta=pi/6,start=0,end=pi*2,border=par("fg"),col=par("bg"),explode=0,
 shade=0.8) {

 angleinc<-2*pi/edges
 angles<-c(seq(start,end,by=angleinc),end)
 viscurve<-(angles>=pi)&(angles<=2*pi)
 nv<-length(angles)
 bisector<-(start+end)/2
 if(explode){
  x<-x+cos(bisector)*explode
  y<-y+sin(bisector)*(1-sin(theta))*explode
 }
 if(shade>0 && shade<1){
  rgbcol<-col2rgb(col)
  shadecol<-rgb(shade*rgbcol[1]/255,shade*rgbcol[2]/255,
  shade*rgbcol[3]/255)
 }
 else shadecol<-col
 xp<-cos(angles)*(radius)+x
 yp<-sin(angles)*(1-sin(theta))*radius+y
 if(start > 3*pi/2){
  polygon(c(xp[nv],x,x,xp[nv],xp[nv]),c(yp[nv],y,
   y-height,yp[nv]-height,yp[nv]-height),border=border,
   col=shadecol)
  polygon(c(xp[viscurve],rev(xp[viscurve])),c(yp[viscurve],
   rev(yp[viscurve])-height),border=border,col=shadecol)
  polygon(c(xp[1],x,x,xp[1],xp[1]),c(yp[1],y,
   y-height,yp[1]-height,yp[1]),border=border,
   col=shadecol)
 }
 else {
  if(start > pi/2) {
   polygon(c(xp[1],x,x,xp[1],xp[1]),c(yp[1],y,
    y-height,yp[1]-height,yp[1]),border=border,
    col=shadecol)
   polygon(c(xp[nv],x,x,xp[nv],xp[nv]),c(yp[nv],
    y,y-height,yp[nv]-height,yp[nv]-height),
    border=border,col=shadecol)
   if(end > pi)
    polygon(c(xp[viscurve],rev(xp[viscurve])),c(yp[viscurve],
     rev(yp[viscurve])-height),border=border,
     col=shadecol)
  }
  else {
   if(end > pi || start<2*pi)
    polygon(c(xp[viscurve],rev(xp[viscurve])),c(yp[viscurve],
     rev(yp[viscurve])-height),border=border,
     col=shadecol)
   if(end > pi/2 && end < 3*pi/2){
    polygon(c(xp[nv],x,x,xp[nv],xp[nv]),c(yp[nv],
     y,y-height,yp[nv]-height,yp[nv]-height),
     border=border,col=shadecol)
   }
   polygon(c(xp[1],x,x,xp[1],xp[1]),c(yp[1],y,
    y-height,yp[1]-height,yp[1]),border=border,
    col=shadecol)
  }
 }
 polygon(c(xp,x),c(yp,y),border=border,col=col)
 return(bisector)
}

pie3D<-function(x,edges=100,radius=1,height=0.3,theta=pi/6, 
 start=0,border=par("fg"),col=NULL,labels=NULL,labelpos=NULL,
 labelcol=par("fg"),labelcex=1.5,sector.order=NULL,explode=0,
 shade=0.8,...) {

 if(!is.numeric(x) || any(x < 0)) 
  stop("pie3D: x values must be positive numbers")
 # drop zeros
 if(any(x == 0)) x<-x[x>0]
 # drop NAs
 if(any(is.na(x))) x<-x[!is.na(x)]
 oldmar<-par("mar")
 par(mar=c(4,4,4,4),xpd=TRUE)
 x<-c(0, cumsum(x)/sum(x))*2*pi+start
 nsectors<-length(x)-1
 if(is.null(col)) col <- rainbow(nsectors)
 else if(length(col) < nsectors) col<-rep(col,length.out=nsectors)
 if(is.null(sector.order))
  # get the order of drawing sectors
  sector.order<-
   order(sin((x[2:(nsectors+1)]+x[1:nsectors])/2),decreasing=TRUE)
 bc<-rep(0,nsectors)
 # set up an empty plot
 plot(0,xlab="",ylab="",xlim=c(-1,1),ylim=c(-1,1),type="n",axes=FALSE,...)
 for(i in sector.order) {
  bc[i]<-draw.tilted.sector(radius=radius,height=height, 
   theta=theta,start=x[i],end=x[i+1],edges=edges, 
   border=border,col=col[i],explode=explode,shade=shade)
 }
 if(!is.null(labels)) {
  if(!is.null(labelpos)) 
   bc<-labelpos
   pie3D.labels(bc,height=height,theta=theta, 
    labels=labels,labelcol=labelcol,labelcex=labelcex)
 }
 par(mar=oldmar,xpd=FALSE)
 invisible(bc)
}
