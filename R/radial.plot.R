# scales a vector of numbers to a new range

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

# plots data as radial lines on a 24 hour "clockface" going clockwise.
# radial.pos and radial.range should be in hours.
# Remember to convert hour/minute values to hour/decimal values.
# example: clock24.plot(rnorm(16)+3,seq(5.5,20.5))

clock24.plot<-function(lengths,clock.pos,...) {
 npos<-length(lengths)
 # if no positions are given, spread the lines out over the circle 
 if(missing(clock.pos)) clock.pos<-seq(0,24-24/(npos+1),length=npos)
 radial.range<-range(clock.pos)
 radial.range[1]<-radial.range[1]-(radial.range[2]-radial.range[1])/(npos-1)
 newrange<-c(pi*(2.5-radial.range[1]/12),pi*(0.5+(24-radial.range[2])/12))
 # rescale to a range of pi/2 to 2.5*pi
 # starting at "midnight" and going clockwise
 clock.radial.pos<-rescale(c(clock.pos,radial.range),newrange)[1:npos]
 clock.labels<-as.character(seq(100,2400,by=100))
 clock.label.pos<-seq(29*pi/12,pi/2,by=-pi/12)
 radial.plot(lengths,clock.radial.pos,newrange,clock.labels,clock.label.pos,...)
}

# plots data as radial lines starting at the right and going counterclockwise
# angles should be given in 0-360 values, use radial.plot for radians
# example: polar.plot(rnorm(20)+3,seq(90,280,by=10))

polar.plot<-function(lengths,polar.pos,labels,label.pos,...) {
 npos<-length(lengths)
 # if no positions are given, add the average distance between positions so that
 # the first and last line don't overlap
 if(missing(polar.pos)) polar.pos<-seq(0,360-360/(npos+1),length=npos)
 if(missing(labels)) {
  label.pos<-seq(0,340,by=20)
  labels<-as.character(label.pos)
  label.range<-c(0,pi*340/180)
 }
 if(missing(label.pos)) label.pos<-polar.pos
 polar.range<-range(polar.pos)
 newrange<-c(pi*polar.range[1]/180,pi*(2-(360-polar.range[2])/180))
 # rescale to radians
 radial.pos<-rescale(c(polar.pos,polar.range),newrange)[1:npos]
 nlabels<-length(labels)
 label.pos<-rescale(c(label.pos,0,360),c(0,2*pi))[1:nlabels]
 radial.plot(lengths,radial.pos,newrange,labels,label.pos,...)
}

# plots radial lines of length 'lengths' from a central origin
# at the angles specified by 'radial.pos' in radians
# starts at the 'east' position and goes counterclockwise
# label.prop is the proportion of max(lengths) that gives the
# radial position of the labels

radial.plot<-function(lengths,radial.pos,radial.range,labels,label.pos,
 label.prop=1.1,main="",xlab="",ylab="",...) {
 maxlength<-label.prop*max(lengths)
 # calculate missing range as above
 if(missing(radial.range)) {
  radial.range<-range(radial.pos)
  radial.range[1]<-radial.range[1]-(radial.range[2]-radial.range[1])/
  (length(radial.pos)-1)
 }
 plot(c(-maxlength,maxlength),c(-maxlength,maxlength),type="n",axes=FALSE,
  main=main,xlab=xlab,ylab=ylab,...)
 # get the vector of x positions
 xpos<-cos(radial.pos)*lengths
 # get the vector of y positions
 ypos<-sin(radial.pos)*lengths
 segments(0,0,xpos,ypos,...)
 if(missing(labels)) labels<-as.character(round(radial.pos,2))
 if(missing(label.pos)) {
  xpos<-cos(radial.pos)*maxlength
  ypos<-sin(radial.pos)*maxlength
 }
 else {
  xpos<-cos(label.pos)*maxlength
  ypos<-sin(label.pos)*maxlength
 }
 text(xpos,ypos,labels)
}

