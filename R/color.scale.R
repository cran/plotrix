color.scale<-function(x,redrange,greenrange,bluerange,scale.up=FALSE){
 ncolors<-length(x)
 if(length(redrange) > 1) {
  reds<-rescale(x,redrange)
  if(min(reds) < 0 || max(reds) > 255) reds<-rescale(reds,c(0,255))
 }
 else reds<-rep(redrange,ncolors)
 if(length(greenrange) > 1) {
  greens<-rescale(x,greenrange)
  if(min(greens) < 0 || max(greens) > 255) greens<-rescale(greens,c(0,255))
 }
 else greens<-rep(greenrange,ncolors)
 if(length(bluerange) > 1) {
  blues<-rescale(x,bluerange)
  if(min(blues) < 0 || max(blues) > 255) blues<-rescale(blues,c(0,255))
 }
 else blues<-rep(bluerange,ncolors)
 colormatrix<-cbind(reds,greens,blues)
 colvec<-apply(colormatrix,1,rgb.to.hex,scale.up)
 return(colvec)
}
