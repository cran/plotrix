gradient.rect<-function(xleft,ybottom,xright,ytop,reds,greens,blues,col=NULL,
 nslices=50,gradient="x") {
 # assume that the user will never want black gradients, so scale up
 if(is.null(col)) col<-color.gradient(reds,greens,blues,nslices)
 else nslices<-length(col)
 if(!is.null(col)) {
  if(gradient == "x") {
   if(length(xleft) == 1) {
    xinc<-(xright-xleft)/nslices
    xlefts<-seq(xleft,xright-xinc,length=nslices)
    xrights<-xlefts+xinc
   }
   else {
    xlefts<-xleft
    xrights<-xright
   }
   rect(xlefts,ybottom,xrights,ytop,col=col,lty=0)
   rect(xlefts[1],ybottom,xrights[nslices],ytop,border="black")
  }
  else {
   if(length(ybottom) == 1) {
    yinc<-(ytop-ybottom)/nslices
    ybottoms<-seq(ybottom,ytop-yinc,length=nslices)
    ytops<-ybottoms+yinc
   }
   else {
    ybottoms<-ybottom
    ytops<-ytop
   }
   rect(xleft,ybottoms,xright,ytops,col=col,lty=0)
   rect(xleft,ybottoms[1],xright,ytops[nslices],border="black")
  }
 }
 return(col)
}
