# plots data as radial lines or a polygon on a 24 hour "clockface" going 
# clockwise. clock.pos should be in decimal hours between 0 and 24.
# use hm2dec=TRUE to convert hour/minute values to hour/decimal values.
# example: clock24.plot(rnorm(16)+3,seq(5.5,20.5,length.out=16))

clock24.plot<-function(lengths,clock.pos,labels=0:23,minutes=FALSE,
 hm2dec=FALSE,label.pos=NULL,rp.type="r",loglen=FALSE,explab=FALSE,...) {
 
 npos<-length(lengths)
 # if no positions are given, spread the lines out over the circle 
 if(missing(clock.pos)) clock.pos<-seq(0,24-24/(npos+1),length=npos)
 else {
  if(hm2dec) {
   splittime<-strsplit(clock.pos,":")
   hm2dh<-function(x) return(as.numeric(x[1])+as.numeric(x[2])/60)
   clock.pos<-sapply(splittime,hm2dh)
  }
 }
 # start at "midnight" and go clockwise
 radial.pos<-pi*(clock.pos*15)/180
 if(minutes) labels<-paste(labels,"00",sep=":")
 if(is.null(label.pos)) label.pos<-seq(0,pi*1.917,length.out=24)
 invisible(radial.plot(lengths,radial.pos,labels=labels,label.pos=label.pos,
  rp.type=rp.type,start=pi/2,clockwise=TRUE,loglen=loglen,explab=explab,...))
}

# plots data as radial lines or a polygon starting at the right and going
# counterclockwise.
# angles should be given in 0-360 values, use radial.plot for radians
# example: polar.plot(rnorm(20)+3,seq(90,280,by=10))

polar.plot<-function(lengths,polar.pos=NULL,labels,label.pos=NULL,
 start=0,clockwise=FALSE,rp.type="r",loglen=FALSE,explab=FALSE,...) {
 
 npoints<-length(lengths)
 if(is.null(polar.pos))  {
  halfinc<-pi/npoints # evenly spaced, offset from zero
  radial.pos<-seq(halfinc,2*pi-halfinc,length.out=npoints)
 }
 else radial.pos<-pi*polar.pos/180
 if(start) start<-pi*start/180
 if(missing(labels)) {
  labels<-label.pos<-seq(0,340,by=20)
  label.pos<-pi*label.pos/180
 }
 invisible(radial.plot(lengths,radial.pos,labels,label.pos,start=start,
  clockwise=clockwise,rp.type=rp.type,loglen=loglen,explab=explab,...))
}

# plots radial lines of length 'lengths', symbols at 'lengths' from the
# center or a polygon with corresponding vertices at 'radial.pos' in radians.
# starts at the 'east' position and goes counterclockwise unless
# the "start" and "clockwise" arguments are changed
# label.prop is the proportion of max(lengths) that gives the
# radial position of the labels

radial.plot<-function(lengths,radial.pos=NULL,labels=NA,label.pos=NULL,
 radlab=FALSE,start=0,clockwise=FALSE,rp.type="r",label.prop=1.1,main="",
 xlab="",ylab="",line.col=par("fg"),lty=par("lty"),lwd=par("lwd"),
 mar=c(2,2,3,2),show.grid=TRUE,show.grid.labels=4,show.radial.grid=TRUE,
 rad.col="gray",grid.col="gray",grid.bg="transparent",grid.left=FALSE,
 grid.unit=NULL,point.symbols=1,point.col=par("fg"),show.centroid=FALSE,
 radial.lim=NULL,radial.labels=NULL,boxed.radial=TRUE,poly.col=NA,
 add=FALSE,loglen=FALSE,explab=FALSE,...) {
 
 # first convert lengths to log values if desired
 if(loglen) lengths<-do.call(log10,list(lengths))
 if(is.null(radial.lim)) radial.lim<-c(floor(min(lengths,na.rm=TRUE)),
  ceiling(max(lengths,na.rm=TRUE)))
 length.dim<-dim(lengths)
 if(is.null(length.dim)) {
  npoints<-length(lengths)
  nsets<-1
  lengths<-matrix(lengths,nrow=1)
 }
 else {
  npoints<-length.dim[2]
  nsets<-length.dim[1]
  lengths<-as.matrix(lengths)
 }
 lengths<-lengths-radial.lim[1]
 lengths[lengths<0]<-NA
 if(is.null(radial.pos)) {
  halfinc<-pi/npoints # evenly spaced, offset from zero
  radial.pos<-seq(halfinc,2*pi-halfinc,length.out=npoints)
 }
 if(is.null(label.pos)) label.pos=radial.pos
 radial.pos.dim<-dim(radial.pos)
 if(is.null(radial.pos.dim))
  radial.pos<-matrix(rep(radial.pos,nsets),nrow=nsets,byrow=TRUE)
 else radial.pos<-as.matrix(radial.pos)
 if(rp.type == "l" && clockwise == TRUE) start<-pi/2
 if(clockwise) radial.pos<--radial.pos
 if(start) radial.pos<-radial.pos+start
 if(show.grid) {
  if(loglen == 0) {
   if(length(radial.lim) < 3) grid.pos<-pretty(radial.lim)
   else grid.pos<-radial.lim
  } else {
   # log scaling is ON
   rangerl<-ceiling(radial.lim)
   grid.pos<-rangerl[1]:rangerl[2]
   oddnum<-length(grid.pos) %% 2
   if(oddnum) dropit<-2
   while(length(grid.pos) > 6) {
    if(oddnum) {
     grid.pos<-grid.pos[-dropit]
     dropit<-dropit+1
    } else {
     grid.pos<-grid.pos[seq(1,length(grid.pos),by=2)]
    }
   }
   if(is.null(radial.labels)) {
    if(explab) radial.labels<-paste0(10,"^",grid.pos)
    else radial.labels<-10^grid.pos
   }
   if(grid.pos[1] < radial.lim[1]) grid.pos[1]<-radial.lim[1]
   angles<-seq(0,1.96*pi,by=0.04*pi)
  }
  maxlength<-max(grid.pos-radial.lim[1])
 } else {
  grid.pos<-NA
  maxlength<-diff(radial.lim)
 }
 oldpar<-par("xpd","mar","pty")
 if(!add) {
  par(mar=mar,pty="s")
  plot(c(-maxlength,maxlength),c(-maxlength,maxlength),type="n",axes=FALSE,
   main=main,xlab=xlab,ylab=ylab)
  if(show.grid) {
   radial.grid(labels=labels,label.pos=label.pos,radlab=radlab,
    radial.lim=radial.lim,start=start,clockwise=clockwise,
    label.prop=label.prop,grid.pos=grid.pos,rad.col=rad.col,
    grid.col=grid.col,grid.bg=grid.bg,
    show.radial.grid=show.radial.grid)
  }
 }
 par(xpd=TRUE)
 # stretch everything out to the correct length
 if(length(line.col) < nsets) line.col<-1:nsets
 if(length(rp.type) < nsets) rp.type<-rep(rp.type,length.out=nsets)
 if(length(point.symbols) < nsets)
  point.symbols<-rep(point.symbols,length.out=nsets)
 if(length(point.col) < nsets) point.col<-rep(point.col,length.out=nsets)
 if(length(poly.col) < nsets) poly.col<-rep(poly.col,length.out=nsets)
 if(length(lty) < nsets) lty<-rep(lty,length.out=nsets)
 if(length(lwd) < nsets) lwd<-rep(lwd,length.out=nsets)
 for(i in 1:nsets) {
  if(nsets > 1) {
   linecol<-line.col[i]
   polycol<-poly.col[i]
   pointcol<-point.col[i]
   pointsymbols<-point.symbols[i]
   ltype<-lty[i]
   lwidth<-lwd[i]
  }
  else {
   linecol<-line.col
   polycol<-poly.col
   pointcol<-point.col
   pointsymbols<-point.symbols
   ltype<-lty
   lwidth<-lwd
  }
  # split up rp.type if there is a combination of displays
  rptype<-unlist(strsplit(rp.type[i],""))
  if(match("s",rptype,0)) {
   if(is.null(pointsymbols)) pointsymbols<-i
   if(is.null(pointcol)) pointcol<-i
  }
  # get the vector of x positions
  xpos<-cos(radial.pos[i,])*lengths[i,]
  # get the vector of y positions
  ypos<-sin(radial.pos[i,])*lengths[i,]
  # plot radial lines if rp.type == "r"    
  if(match("r",rptype,0))
   segments(0,0,xpos,ypos,col=linecol,lty=ltype,lwd=lwidth,...)
  if(match("p",rptype,0))
   polygon(xpos,ypos,border=linecol,col=polycol,lty=ltype,
    lwd=lwidth,...)
  if(match("s",rptype,0))
   points(xpos,ypos,pch=point.symbols,col=pointcol,...)
  if(match("t",rptype,0)) {
   if(boxed.radial)
    boxed.labels(xpos,ypos,point.symbols,col=pointcol)
   else text(xpos,ypos,point.symbols,col=pointcol)
  }
  if(match("l",rptype,0))
   lines(xpos,ypos,lty=ltype,lwd=lwidth,col=linecol,...)
  if(show.centroid) {
   if(match("p",rptype,0)) {
    nvertices<-length(xpos)
    # first get the "last to first" area component
    polygonarea<-xpos[nvertices]*ypos[1] - xpos[1]*ypos[nvertices]
    for(vertex in 1:(nvertices-1))
     polygonarea<-
      polygonarea+xpos[vertex]*ypos[vertex+1]-xpos[vertex+1]*ypos[vertex]
    polygonarea<-polygonarea/2
    centroidx<-
     (xpos[nvertices]+xpos[1])*(xpos[nvertices]*ypos[1]-xpos[1]*ypos[nvertices])
    centroidy<-
     (ypos[nvertices]+ypos[1])*(xpos[nvertices]*ypos[1]-xpos[1]*ypos[nvertices])
    for(vertex in 1:(nvertices-1)) {
     centroidx<-centroidx + (xpos[vertex]+xpos[vertex+1])*
      (xpos[vertex]*ypos[vertex+1]-xpos[vertex+1]*ypos[vertex])
     centroidy<-centroidy + (ypos[vertex]+ypos[vertex+1])*
      (xpos[vertex]*ypos[vertex+1]-xpos[vertex+1]*ypos[vertex])
    }
    points(centroidx/(6*polygonarea),centroidy/(6*polygonarea),
     col=point.col[i],pch=point.symbols[i],cex=2,...)
   
   }
   else
    points(mean(xpos),mean(ypos),col=pointcol,pch=pointsymbols,
     cex=2,...)
  }
 }
 if(show.grid.labels && !add) {
  if(show.grid.labels%%2) {
   ypos<-grid.pos-radial.lim[1]
   xpos<-rep(0,length(grid.pos))
   if(show.grid.labels==1) ypos<--ypos
  }
  else {
   xpos<-grid.pos-radial.lim[1]
   ypos<-rep(0,length(grid.pos))
   if(show.grid.labels==2) xpos<--xpos
  }
  if(is.null(radial.labels)) radial.labels<-grid.pos
  if(!is.null(grid.unit))
   radial.labels[length(grid.pos)]<-
    paste0(radial.labels[length(grid.pos)],grid.unit)
  if(boxed.radial)
   boxed.labels(xpos,ypos,radial.labels,border=FALSE,
    cex=par("cex.lab"))
  else text(xpos,ypos,radial.labels,cex=par("cex.lab"))
 }
 invisible(oldpar)
}
