rgb.to.hex<-function(rgb,scale.up=TRUE) {
 if(length(rgb) != 3) stop("rgb must be an rgb triplet")
 if(any(rgb < 0) || any(rgb > 255)) stop("all rgb must be between 0 and 255")
 # if it looks like a 0-1 value, get the 0-255 equivalent
 if(all(rgb <= 1)) rgb<-rgb*255 
 hexdigit<-c(0:9,letters[1:6])
 return(paste("#",hexdigit[rgb[1]%/%16+1],hexdigit[rgb[1]%%16+1],
  hexdigit[rgb[2]%/%16+1],hexdigit[rgb[2]%%16+1],
  hexdigit[rgb[3]%/%16+1],hexdigit[rgb[3]%%16+1],
  sep="",collapse=""))
}

gradient.rect<-function(xleft,ybottom,xright,ytop,reds,greens,blues,
 nslices=20,gradient="x") {
 maxncol<-max(c(length(reds),length(greens),length(blues)))
 if(maxncol < 2) stop("Must specify at least two values for one color")
 if(maxncol > 2 || maxncol > nslices) nslices<-maxncol
 if(length(reds) == 2) {
  # assume they are endpoints and calculate linear gradient
  if(reds[1] < 0 || reds[2] > 1) {
   reds[1]<-ifelse(reds[1] < 0,0,reds[1])
   reds[2]<-ifelse(reds[2] > 1,1,reds[2])
  }
  reds<-seq(reds[1],reds[2],length=nslices)
 }
 if(length(greens) == 2) {
  # assume they are endpoints and calculate linear gradient
  if(greens[1] < 0 || greens[2] > 1) {
   greens[1]<-ifelse(greens[1] < 0,0,greens[1])
   greens[2]<-ifelse(greens[2] > 1,1,greens[2])
  }
  greens<-seq(greens[1],greens[2],length=nslices)
 }
 if(length(blues) == 2) {
  # assume they are endpoints and calculate linear gradient
  if(blues[1] < 0 || blues[2] > 1) {
   blues[1]<-ifelse(blues[1] < 0,0,blues[1])
   blues[2]<-ifelse(blues[2] > 1,1,blues[2])
  }
  blues<-seq(blues[1],blues[2],length=nslices)
 }
 colormatrix<-cbind(reds,greens,blues)
 colvec<-apply(colormatrix,1,rgb.to.hex)
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
 return(colvec)
}
