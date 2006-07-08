draw.tilted.sector<-function(x=0,y=0,edges=100,radius=1,height=0.3,theta=pi/6,
 start=0,end=pi*2,border=par("fg"),col=par("bg"),explode=0) {

 angleinc<-2*pi/edges
 angles<-c(seq(start,end,by=angleinc),end)
 nv<-length(angles)
 bisector<-(start+end)/2
 if(explode) {
  x<-x+cos(bisector)*explode
  y<-y+sin(bisector)*(1-sin(theta))*explode
 }
 xp<-cos(angles)*(radius)+x
 yp<-sin(angles)*(1-sin(theta))*radius+y
 if(start > 3 * pi/2) {
  viscurve<-angles <= pi*2
  # end facet
  polygon(c(xp[nv],x,x,xp[nv],xp[nv]),
  c(yp[nv],y,y-height,yp[nv]-height,yp[nv]-height),
  border=border,col=col)
  # outer curve
  polygon(c(xp[viscurve],rev(xp[viscurve])),
   c(yp[viscurve],rev(yp[viscurve])-height),border=border,col=col)
  # start facet
  polygon(c(xp[1],x,x,xp[1],xp[1]),
  c(yp[1],y,y-height,yp[1]-height,yp[1]),
  border=border,col=col)
 }
 else {
  if((end > pi) && (start <= pi))  {
   viscurve<-(angles >= pi) & (angles <= 2*pi)
  }
  else {
   viscurve <- rep(TRUE,length(angles))
  }
  if(start > pi/2) {
   # start facet
   polygon(c(xp[1],x,x,xp[1],xp[1]),
   c(yp[1],y,y-height,yp[1]-height,yp[1]),
   border=border,col=col)
   # end facet
   polygon(c(xp[nv],x,x,xp[nv],xp[nv]),
    c(yp[nv],y,y-height,yp[nv]-height,yp[nv]-height),
    border=border,col=col)
   # outer curve
   if(end > pi)
    polygon(c(xp[viscurve],rev(xp[viscurve])),
     c(yp[viscurve],rev(yp[viscurve])-height),border=border,col=col)
  }
  else {
   viscurve<-(angles >= pi) & (angles <= 2*pi)
   # outer curve
   if(end > pi || start < 2 * pi)
    polygon(c(xp[viscurve],rev(xp[viscurve])),
     c(yp[viscurve],rev(yp[viscurve])-height),border=border,col=col)
   if(end > pi/2 && end < 3 * pi/2) {
    # end facet
    polygon(c(xp[nv],x,x,xp[nv],xp[nv]),
    c(yp[nv],y,y-height,yp[nv]-height,yp[nv]-height),
    border=border,col=col)
   }
   # start facet
   polygon(c(xp[1],x,x,xp[1],xp[1]),
   c(yp[1],y,y-height,yp[1]-height,yp[1]),
   border=border,col=col)
  }
 }
 # top sector
 polygon(c(xp,x),c(yp,y),border=border,col=col)
 return(bisector)
}

pie3D.labels<-function(radialpos,radius=1,height=0.3,theta=pi/6,
 labels,labelcol=par("fg"),labelcex=1.5) {

 oldcex<-par("cex")
 par(cex=labelcex,xpd=TRUE)
 for(i in 1:length(labels)) {
  xpos<-1.2 * cos(radialpos[i])
  fr<-radialpos[i] > pi
  offset<-(1 - 2 * fr) * 0.4 - fr * height
  ypos<-sin(radialpos[i])*(1-sin(theta)) * 0.75 * radius + offset
  text(xpos,ypos,labels[i],adj=0.5,col=labelcol)
 }
 par(cex=oldcex,xpd=FALSE)
}

pie3D<-function(x,edges=100,radius=1,height=0.3,theta=pi/6,start=0,
 border=par("fg"),col=NULL,labels=NULL,labelpos=NULL,
 labelcol=par("fg"),labelcex=1.5,explode=0,...) {

 if(!is.numeric(x) || any(x<0))
  stop("pie3D: x values must be positive numbers")
 # ignore zeros
 if(any(x == 0)) x<-x[x>0]
 # drop NAs
 if(any(is.na(x))) x<-x[!is.na(x)]
 oldmar=par("mar")
 par(mar=c(4,4,4,4),xpd=TRUE)
 x<-c(0,cumsum(x)/sum(x))*2*pi+start
 nsectors <- length(x)-1
 if (is.null(col)) col<-rainbow(nsectors)
 else if(length(col) < nsectors) col<-rep(col,nsectors)
 # get the order of drawing sectors
 sector.order<-
  order(sin((x[2:(nsectors+1)]+x[1:nsectors])/2),decreasing=TRUE)
 bc<-rep(0,nsectors)
 # set up an empty plot, passing things like title in ...
 plot(0,xlab="",ylab="",xlim=c(-radius,radius),ylim=c(-radius,radius),
  type="n",axes=FALSE,...)
 for(i in sector.order) {
  bc[i]<-
   draw.tilted.sector(radius=radius,height=height,theta=theta,
    start=x[i],end=x[i+1],edges=edges,border=border,col=col[i],
    explode=explode)
 }
 if(!is.null(labels)) {
  if(!is.null(labelpos)) bc<-labelpos
  pie3D.labels(bc,radius=radius,height=height,theta=theta,
   labels=labels,labelcol=labelcol,labelcex=labelcex)
 }
 par(mar=oldmar,xpd=FALSE)
 invisible(bc)
}
