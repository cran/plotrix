rgb.to.hex<-function(rgb,scale.up=TRUE) {
 if(length(rgb) != 3) stop("rgb must be an rgb triplet")
 if(any(rgb < 0) || any(rgb > 255)) stop("all rgb must be between 0 and 255")
 # if it looks like a 0-1 value AND scale.up is TRUE, get the 0-255 equivalent
 if(all(rgb <= 1) && scale.up) rgb<-rgb*255 
 hexdigit<-c(0:9,letters[1:6])
 return(paste("#",hexdigit[rgb[1]%/%16+1],hexdigit[rgb[1]%%16+1],
  hexdigit[rgb[2]%/%16+1],hexdigit[rgb[2]%%16+1],
  hexdigit[rgb[3]%/%16+1],hexdigit[rgb[3]%%16+1],
  sep="",collapse=""))
}

color.gradient<-function(reds,greens,blues,nslices=50,scale.up=FALSE) {
 maxncol<-max(c(length(reds),length(greens),length(blues)))
 if(maxncol < 2) {
  cat("color.gradient: Must specify at least two values for one color\n")
  return(NULL)
 }
 if(length(reds) < nslices) {
  reds<-approx(reds,n=nslices)$y
  # take care of any values < 0 or > 255
  if(min(reds) < 0 || max(reds) > 255) reds<-rescale(reds,c(0,255))
 }
 else {
  # chop off extra values so they don't mess up cbind()
  if(length(reds) > nslices) reds<-reds[1:nslices]
 }
 if(length(greens) < nslices) {
  greens<-approx(greens,n=nslices)$y
  if(min(greens) < 0 || max(greens) > 255) greens<-rescale(greens,c(0,255))
 }
 else if(length(greens) > nslices) greens<-greens[1:nslices]
 if(length(blues) < nslices) {
  blues<-approx(blues,n=nslices)$y
  if(min(blues) < 0 || max(blues) > 255) blues<-rescale(blues,c(0,255))
 }
 else if(length(blues) > nslices) blues<-blues[1:nslices]
 colormatrix<-cbind(reds,greens,blues)
 colvec<-apply(colormatrix,1,rgb.to.hex,scale.up)
 return(colvec)
}

gradient.rect<-function(xleft,ybottom,xright,ytop,reds,greens,blues,
 nslices=50,gradient="x") {
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
