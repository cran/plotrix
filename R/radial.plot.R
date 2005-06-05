# linearly transforms a vector of numbers to a new range

rescale<-function(x,newrange) {
 if(missing(x) | missing(newrange)) {
  usage.string<-paste("Usage: rescale(x,newrange)\n",
   "\twhere x is a numeric object and newrange is the new min and max\n",
   sep="",collapse="")
  stop(usage.string)
 }
 if(is.numeric(x) && is.numeric(newrange)) {
  xrange<-range(x)
  if(xrange[1] == xrange[2]) stop("rescale: can't rescale a constant vector!")
  mfac<-(newrange[2]-newrange[1])/(xrange[2]-xrange[1])
  return(newrange[1]+(x-xrange[1])*mfac)
 }
 else {
  warning("Only numeric objects can be rescaled")
  return(x)
 }
}

# plots data as radial lines or a polygon on a 24 hour "clockface" going 
# clockwise. radial.pos and radial.range should be in hours.
# Remember to convert hour/minute values to hour/decimal values.
# example: clock24.plot(rnorm(16)+3,seq(5.5,20.5))

clock24.plot<-function(lengths,clock.pos,rp.type="r",...) {
 npos<-length(lengths)
 # if no positions are given, spread the lines out over the circle 
 if(missing(clock.pos)) clock.pos<-seq(0,24-24/(npos+1),length=npos)
 radial.range<-range(clock.pos)
 radial.range[1]<-radial.range[1]-(radial.range[2]-radial.range[1])/(npos-1)
 newrange<-c(pi*(2.5-radial.range[1]/12),pi*(0.5+(24-radial.range[2])/12))
 # rescale to a range of pi/2 to 2.5*pi
 # starting at "midnight" and going clockwise
 clock.radial.pos<-rescale(c(clock.pos,radial.range),newrange)[1:npos]
 clock.labels<-as.character(seq(0,2300,by=100))
 clock.label.pos<-rev(seq(pi/2,29*pi/12,by=pi/12))
 radial.plot(lengths,clock.radial.pos,newrange,clock.labels,clock.label.pos,
  rp.type=rp.type,...)
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
 if(missing(labels)) {
  labels<-as.character(seq(0,340,by=20))
  label.pos<-seq(0,1.9*pi,length=18)
 }
 if(missing(label.pos)) label.pos<-pi*label.pos/180
 radial.plot(lengths,radial.pos,range(radial.pos),labels,label.pos,
  rp.type=rp.type,...)
}

# plots radial lines of length 'lengths' or a polygon with corresponding
# vertices specified by 'radial.pos' in radians.
# starts at the 'east' position and goes counterclockwise
# label.prop is the proportion of max(lengths) that gives the
# radial position of the labels

radial.plot<-function(lengths,radial.pos,radial.range,labels,label.pos,
 rp.type="r",label.prop=1.1,main="",xlab="",ylab="",line.col=par("fg"),
 mar=c(2,2,3,2),show.grid=TRUE,grid.col="gray",grid.bg=par("bg"),...) {
 
 length.dim<-dim(lengths)
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
  radial.pos<-seq(0,pi*(2-2/(npoints+1)),length=npoints)
 radial.pos.dim<-dim(radial.pos)
 if(is.null(radial.pos.dim))
  radial.pos<-matrix(rep(radial.pos,nsets),nrow=nsets,byrow=TRUE)
 # calculate missing range as above
 if(missing(radial.range)) {
  radial.range<-range(radial.pos)
  radial.range[1]<-radial.range[1]-(radial.range[2]-radial.range[1])/
  (length(radial.pos)-1)
 }
 if(show.grid) {
  grid.pos<-pretty(lengths)
  if(grid.pos[1] <= 0) grid.pos<-grid.pos[-1]
  maxlength<-max(grid.pos)
  angles<-seq(0,2*pi,by=0.04*pi)
 }
 else {
  grid.pos<-NA
  maxlength<-label.prop*max(lengths)
 }
 oldmar<-par("mar")
 par(mar=mar)
 plot(c(-maxlength,maxlength),c(-maxlength,maxlength),type="n",axes=FALSE,
  main=main,xlab=xlab,ylab=ylab,...)
 par(xpd=TRUE)
 if(show.grid) {
  for(i in 1:length(grid.pos)) {
   xpos<-cos(angles)*grid.pos[i]
   ypos<-sin(angles)*grid.pos[i]
   polygon(xpos,ypos,border=grid.col,col=grid.bg)
  }
  ypos<-rep(-maxlength/15,length(grid.pos))
  boxed.labels(grid.pos,ypos,as.character(grid.pos))
 }
 if(length(line.col) < nsets) line.col<-1:nsets
 for(i in 1:nsets) {
  # get the vector of x positions
  xpos<-cos(radial.pos[i,])*lengths[i,]
  # get the vector of y positions
  ypos<-sin(radial.pos[i,])*lengths[i,]
  # plot radial lines if rp.type == "r"    
  if(rp.type == "r") segments(0,0,xpos,ypos,col=line.col[i],...)
  else polygon(xpos,ypos,border=line.col[i],col=NA,...)
 }
 if(missing(labels)) {
  if(length(radial.pos) <= 20) {
   labels<-as.character(round(radial.pos,2))
   label.pos<-radial.pos
  }
  else {
   label.pos<-seq(0,1.9*pi,length=9)
   labels<-as.character(round(label.pos,2))
  }
 }
 if(missing(label.pos)) label.pos<-seq(0,pi*(2-2/npoints),length=npoints)
 xpos<-cos(label.pos)*maxlength
 ypos<-sin(label.pos)*maxlength
 segments(0,0,xpos,ypos,col=grid.col)
 xpos<-cos(label.pos)*maxlength*label.prop
 ypos<-sin(label.pos)*maxlength*label.prop
 boxed.labels(xpos,ypos,labels,ypad=0.7,border=FALSE)
 par(mar=oldmar,xpd=FALSE)
}
