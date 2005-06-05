color.gradient<-function(reds,greens,blues,nslices=50) {
 maxncol<-max(c(length(reds),length(greens),length(blues)))
 if(maxncol < 2) {
  cat("color.gradient: Must specify at least two values for one color\n")
  return(NULL)
 }
 if(length(reds) < nslices) {
  reds<-approx(reds,n=nslices)$y
  # take care of any values < 0 or > 1
  if(min(reds) < 0 || max(reds) > 1) reds<-rescale(reds,c(0,1))
 }
 else {
  # chop off extra values so they don't mess up cbind()
  if(length(reds) > nslices) reds<-reds[1:nslices]
 }
 if(length(greens) < nslices) {
  greens<-approx(greens,n=nslices)$y
  if(min(greens) < 0 || max(greens) > 1) greens<-rescale(greens,c(0,1))
 }
 else if(length(greens) > nslices) greens<-greens[1:nslices]
 if(length(blues) < nslices) {
  blues<-approx(blues,n=nslices)$y
  if(min(blues) < 0 || max(blues) > 1) blues<-rescale(blues,c(0,1))
 }
 else if(length(blues) > nslices) blues<-blues[1:nslices]
 colvec<-rgb(reds,greens,blues)
 return(colvec)
}

gradient.rect<-function(xleft,ybottom,xright,ytop,reds,greens,blues,
 nslices=50,gradient="x") {
 # assume that the user will never want black gradients, so scale up
 colvec<-color.gradient(reds,greens,blues,nslices)
 if(!is.null(colvec)) {
  if(gradient == "x") {
   if(length(xleft) == 1) {
    xinc<-(xright-xleft)/(nslices-1)
    xlefts<-seq(xleft,xright-xinc,length=nslices)
    xrights<-xlefts+xinc
   }
   else {
    xlefts<-xleft
    xrights<-xright
   }
   rect(xlefts,ybottom,xrights,ytop,col=colvec,lty=0)
  }
  else {
   if(length(ybottom) == 1) {
    yinc<-(ytop-ybottom)/(nslices-1)
    ybottoms<-seq(ybottom,ytop-yinc,length=nslices)
    ytops<-ybottoms+yinc
   }
   else {
    ybottoms<-ybottom
    ytops<-ytop
   }
   rect(xleft,ybottoms,xright,ytops,col=colvec,lty=0)
  }
 }
 return(colvec)
}
