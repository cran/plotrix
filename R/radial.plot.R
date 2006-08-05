# plots data as radial lines or a polygon on a 24 hour "clockface" going 
# clockwise. clock.pos should be in decimal hours between 0 and 24.
# Remember to convert hour/minute values to hour/decimal values.
# example: clock24.plot(rnorm(16)+3,seq(5.5,20.5))

clock24.plot<-function(lengths,clock.pos,rp.type="r",...) {
 npos<-length(lengths)
 # if no positions are given, spread the lines out over the circle 
 if(missing(clock.pos)) clock.pos<-seq(0,24-24/(npos+1),length=npos)
 # start at "midnight" and go clockwise
 radial.pos<-pi*(450-clock.pos*15)/180
 clock.labels<-paste(0:23,"00",sep="")
 clock.label.pos<-seq(5*pi/2,7*pi/12,by=-pi/12)
 radial.plot(lengths,radial.pos,labels=clock.labels,
  label.pos=clock.label.pos,rp.type=rp.type,...)
}

# plots data as radial lines or a polygon starting at the right and going
# counterclockwise.
# angles should be given in 0-360 values, use radial.plot for radians
# example: polar.plot(rnorm(20)+3,seq(90,280,by=10))

polar.plot<-function(lengths,polar.pos,labels,label.pos,rp.type="r",...) {
 npos<-length(lengths)
 # if no positions are given, add the average distance between positions so that
 # the first and last line don't overlap
 if(missing(polar.pos)) radial.pos<-seq(0,(2-2/(npos+1))*pi,length=npos)
 else radial.pos<-pi*polar.pos/180
 if(!missing(label.pos)) label.pos<-pi*label.pos/180
 if(missing(labels)) {
  labels<-as.character(seq(0,340,by=20))
  label.pos<-seq(0,1.89*pi,length=18)
 }
 radial.plot(lengths,radial.pos,labels,label.pos,rp.type=rp.type,...)
}

# plots radial lines of length 'lengths', symbols at 'lengths' from the
# center or a polygon with corresponding vertices at 'radial.pos' in radians.
# starts at the 'east' position and goes counterclockwise
# label.prop is the proportion of max(lengths) that gives the
# radial position of the labels

radial.plot<-function(lengths,radial.pos,labels,label.pos,
 rp.type="r",label.prop=1.1,main="",xlab="",ylab="",line.col=par("fg"),
 mar=c(2,2,3,2),show.grid=TRUE,grid.col="gray",grid.bg=par("bg"),
 point.symbols=NULL,point.col=NULL,show.centroid=FALSE,
 radial.lim=NULL,...) {
 
 length.dim<-dim(lengths)
 if(is.null(radial.lim)) radial.lim<-range(lengths)
 if(is.null(length.dim)) {
  npoints<-length(lengths)
  nsets<-1
  lengths<-matrix(lengths,nrow=1)
 }
 else {
  npoints<-length.dim[2]
  nsets<-length.dim[1]
 }
 if(missing(radial.pos))
  radial.pos<-seq(0,pi*(2-2/npoints),length=npoints)
 radial.pos.dim<-dim(radial.pos)
 if(is.null(radial.pos.dim))
  radial.pos<-matrix(rep(radial.pos,nsets),nrow=nsets,byrow=TRUE)
 if(show.grid) {
  grid.pos<-pretty(radial.lim)
  if(grid.pos[1] <= 0) grid.pos<-grid.pos[-1]
  maxlength<-max(grid.pos)
  angles<-seq(0,1.96*pi,by=0.04*pi)
 }
 else {
  grid.pos<-NA
  maxlength<-max(radial.lim)
 }
 oldpar<-par(no.readonly=TRUE)
 par(mar=mar,pty="s")
 plot(c(-maxlength,maxlength),c(-maxlength,maxlength),type="n",axes=FALSE,
  main=main,xlab=xlab,ylab=ylab,...)
 par(xpd=TRUE)
 if(length(line.col) < nsets) line.col<-1:nsets
 if(rp.type == "s") {
  if(is.null(point.symbols)) point.symbols<-1:nsets
  if(length(point.symbols)<nsets)
   point.symbols<-rep(point.symbols,length.out=nsets)
  if(is.null(point.col)) point.col<-1:nsets
  if(length(point.col)<nsets)
   point.col<-rep(point.col,length.out=nsets)
 }
 for(i in 1:nsets) {
  # get the vector of x positions
  xpos<-cos(radial.pos[i,])*lengths[i,]
  # get the vector of y positions
  ypos<-sin(radial.pos[i,])*lengths[i,]
  # plot radial lines if rp.type == "r"    
  if(rp.type == "r") segments(0,0,xpos,ypos,col=line.col[i],...)
  if(rp.type == "p") polygon(xpos,ypos,border=line.col[i],col=NA,...)
  if(rp.type == "s") points(xpos,ypos,pch=point.symbols[i],col=point.col[i],...)
  if(show.centroid)
   points(mean(xpos),mean(ypos),col=point.col[i],pch=point.symbols[i],cex=2,...)
 }
 if(missing(labels)) {
  if(length(radial.pos) <= 20) {
   labels<-as.character(round(radial.pos,2))
   label.pos<-radial.pos
  }
  else {
   label.pos<-seq(0,1.8*pi,length=9)
   labels<-as.character(round(label.pos,2))
  }
 }
 if(missing(label.pos))
  label.pos<-seq(0,pi*(2-2/npoints),length=npoints)
 xpos<-cos(label.pos)*maxlength
 ypos<-sin(label.pos)*maxlength
 segments(0,0,xpos,ypos,col=grid.col)
 xpos<-cos(label.pos)*maxlength*label.prop
 ypos<-sin(label.pos)*maxlength*label.prop
 boxed.labels(xpos,ypos,labels,ypad=0.7,border=FALSE)
 if(show.grid) {
  for(i in seq(length(grid.pos),1,by=-1)) {
   xpos<-cos(angles)*grid.pos[i]
   ypos<-sin(angles)*grid.pos[i]
   polygon(xpos,ypos,border=grid.col,col=grid.bg)
  }
  ypos<-rep(-maxlength/15,length(grid.pos))
  boxed.labels(grid.pos,ypos,as.character(grid.pos),border=FALSE)
 }
 par(oldpar)
}
