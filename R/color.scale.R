# linearly scales values of x into the color ranges specified

color.scale<-function(x,redrange,greenrange,bluerange){
 ncolors<-length(x)
 validx<-!is.na(x)
 xrange<-range(x[validx])
 nreds<-length(redrange)
 if(nreds > 1) {
  reds<-rep(NA,ncolors)
  xstart<-xrange[1]
  # an ugly kludge to fix the highest value not being included
  xinc<-1.0001*diff(xrange)/(nreds-1)
  for(seg in 1:(nreds-1)) {
   segindex<- x >= xstart & x <= xstart+xinc & validx
   reds[segindex]<-rescale(x[segindex],redrange[c(seg,seg+1)])
   xstart<-xstart+xinc
  }
  if(min(reds[validx]) < 0 || max(reds[validx]) > 1)
   reds<-rescale(reds,c(0,1))
 }
 else reds<-rep(redrange,ncolors)
 ngreens<-length(greenrange)
 if(ngreens > 1) {
  greens<-rep(greenrange[ngreens],ncolors)
  xstart<-xrange[1]
  xinc<-1.0001*diff(xrange)/(ngreens-1)
  for(seg in 1:(ngreens-1)) {
   segindex<- x >= xstart & x <= xstart+xinc & validx
   greens[segindex]<-rescale(x[segindex],greenrange[c(seg,seg+1)])
   xstart<-xstart+xinc
  }
  if(min(greens[validx]) < 0 || max(greens[validx]) > 1)
   greens<-rescale(greens,c(0,1))
 }
 else greens<-rep(greenrange,ncolors)
 nblues<-length(bluerange)
 if(length(bluerange) > 1) {
  blues<-rep(bluerange[nblues],ncolors)
  xstart<-xrange[1]
  xinc<-1.0001*diff(xrange)/(nblues-1)
  for(seg in 1:(nblues-1)) {
   segindex<- x >= xstart & x <= xstart+xinc & validx
   blues[segindex]<-rescale(x[segindex],bluerange[c(seg,seg+1)])
   xstart<-xstart+xinc
  }
  if(min(blues[validx]) < 0 || max(blues[validx]) > 1)
   blues<-rescale(blues,c(0,1))
 }
 else blues<-rep(bluerange,ncolors)
 colors<-x
 colors[validx]<-rgb(reds[validx],greens[validx],blues[validx])
 return(colors)
}
