color.scale<-function(x,redrange,greenrange,bluerange){
 ncolors<-length(x)
 if(length(redrange) > 1) {
  reds<-rescale(x,redrange)
  if(min(reds) < 0 || max(reds) > 1) reds<-rescale(reds,c(0,1))
 }
 else reds<-rep(redrange,ncolors)
 if(length(greenrange) > 1) {
  greens<-rescale(x,greenrange)
  if(min(greens) < 0 || max(greens) > 1) greens<-rescale(greens,c(0,1))
 }
 else greens<-rep(greenrange,ncolors)
 if(length(bluerange) > 1) {
  blues<-rescale(x,bluerange)
  if(min(blues) < 0 || max(blues) > 1) blues<-rescale(blues,c(0,1))
 }
 else blues<-rep(bluerange,ncolors)
 colvec<-rgb(reds,greens,blues)
 return(colvec)
}
