color.scale<-function(x,redrange=NA,greenrange=NA,bluerange=NA,extremes=NA) {
 if(!is.na(extremes[1])){
  # calculate the color ranges from the extremes
  colmat<-col2rgb(extremes)
  redrange<-colmat[1,]/255
  greenrange<-colmat[2,]/255
  bluerange<-colmat[3,]/255
 }
 ncolors<-length(x)
 xrange<-range(x)
 nreds<-length(redrange)
 if(nreds>1) {
  reds<-rep(redrange[nreds],ncolors)
  xstart<-xrange[1]
  xinc<-diff(xrange)/(nreds-1)
  for(seg in 1:(nreds-1)){
   segindex<-(x >= xstart) & (x <= (xstart+xinc))
   reds[segindex]<-rescale(x[segindex],redrange[c(seg,seg+1)])
   xstart<-xstart+xinc
  }
  if(min(reds) < 0 || max(reds) > 1) reds<-rescale(reds,c(0,1))
 }
 else reds<-rep(redrange,ncolors)
 ngreens<-length(greenrange)
 if(ngreens>1) {
  greens<-rep(greenrange[ngreens],ncolors)
  xstart<-xrange[1]
  xinc<-diff(xrange)/(ngreens-1)
  for(seg in 1:(ngreens-1)){
   segindex<-(x >= xstart) & (x <= (xstart+xinc))
   greens[segindex]<-rescale(x[segindex],greenrange[c(seg,seg+1)])
   xstart<-xstart+xinc
  }
  if(min(greens) < 0 || max(greens) > 1)
   greens<-rescale(greens,c(0,1))
 }
 else greens<-rep(greenrange,ncolors)
 nblues<-length(bluerange)
 if(length(bluerange)>1) {
  blues<-rep(bluerange[nblues],ncolors)
  xstart<-xrange[1]
  xinc<-diff(xrange)/(nblues-1)
  for(seg in 1:(nblues-1)){
   segindex<-(x >= xstart) & (x <= (xstart+xinc))
   blues[segindex]<-rescale(x[segindex],bluerange[c(seg,seg+1)])
   xstart<-xstart+xinc
  }
  if(min(blues) < 0 || max(blues) > 1)
  blues<-rescale(blues,c(0,1))
 }
 else blues<-rep(bluerange,ncolors)
 xdim<-dim(x)
 if(is.null(xdim)) colors<-rgb(reds,greens,blues)
 else colors<-matrix(rgb(reds,greens,blues),nrow=xdim[1])
 return(colors)
}
