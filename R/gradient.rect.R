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
