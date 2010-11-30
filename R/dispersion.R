dispersion<-function (x,y,ulim,llim=ulim,intervals=TRUE,
 arrow.cap=0.01,arrow.gap=NA,type="a",fill=NA,lty=NA,pch=NA,border=NA,...) {

 if(is.list(x) && length(x[[1]]) == length(x[[2]])) {
  y<-x$y
  x<-x$x
 }
 if(missing(y) && !missing(x)) {
  y<-x
  x<-1:length(x)
 }
 # if absolute values are passed, convert them to intervals
 if(!intervals) {
  llim<-y-llim
  ulim<-ulim-y
 }
 plotlim<-par("usr")
 npoints<-length(x)
 if(is.na(arrow.gap)) arrow.gap<-strheight("O")/1.5
 for(i in 1:npoints) {
  if(toupper(type) == "A") {
   if(!is.na(llim[i])) {
    if(arrow.gap >= llim[i] * 0.9) {
     caplen<-arrow.cap * diff(par("usr")[1:2])
     x0<-x[i]-caplen
     x1<-x[i]+caplen
     y0<-rep(y[i]-llim[i],2)
     y1<-rep(y[i]-llim[i],2)
     segments(x0,y0,x1,y1,...)
    }
    else {
     caplen<-arrow.cap*par("pin")[1]
     x0<-x1<-rep(x[i],2)
     y0<-y[i]-arrow.gap
     y1<-y[i]-llim[i]
     arrows(x0,y0,x1,y1,length=caplen,angle=90,...)
    }
   }
   else {
    x0<-x1<-rep(x[i],2)
    y0<-y[i]-arrow.gap
    y1<-plotlim[3]
    segments(x0,y0,x1,y1,...)
   }
   if(!is.na(ulim[i])) {
    if(arrow.gap >= ulim[i] * 0.9) {
     caplen<-arrow.cap * diff(par("usr")[1:2])
     x0<-x[i]-caplen
     x1<-x[i]+caplen
     y0<-rep(y[i]+ulim[i],2)
     y1<-rep(y[i]+ulim[i],2)
     segments(x0,y0,x1,y1,...)
    }
    else {
     caplen<-arrow.cap*par("pin")[1]
     x0<-x1<-rep(x[i],2)
     y0<-y[i]+arrow.gap
     y1<-y[i]+ulim[i]
     arrows(x0,y0,x1,y1,length=caplen,angle=90,...)
    }
   }
   else {
    x0<-x1<-rep(x[i],2)
    y0<-y[i]-arrow.gap
    y1<-plotlim[4]
    segments(x0,y0,x1,y1,...)
   }
  }
 }
 if(toupper(type) == "L") {
  if(!is.na(fill)) {
   polygon(c(x,rev(x)),c(y+ulim,rev(y-llim)),col=fill,border=NA)
   if(is.na(pch)) {
    if(is.na(lty)) points(x,y,pch=pch,...)
    else lines(x,y,lty=lty,pch=pch,type="b",...)
   }
   else {
    if(!is.na(lty)) lines(x,y,lty=lty,...)
   }
  }
  if(!is.na(border)) {
   lines(x,y+ulim,lty=border,...)
   lines(x,y-llim,lty=border,...)
  }
 }
}
