dispersion<-function (x,y,ulim,llim=ulim,arrow.cap=0.01,arrow.gap=NA,
 type="a",fill=NA,...) {

 if(is.list(x) && length(x[[1]]) == length(x[[2]])) {
  y<-x$y
  x<-x$x
 }
 if(missing(y) && !missing(x)) {
  y<-x
  x<-1:length(x)
 }
 npoints<-length(x)
 if(is.na(arrow.gap)) arrow.gap<-strheight("O")/1.5
 for(i in 1:npoints) {
  if(toupper(type) == "A") {
   if(!is.na(ulim[i])) {
    if(arrow.gap >= ulim[i] * 0.9 || arrow.gap >= llim[i] * 0.9) {
     caplen<-arrow.cap * diff(par("usr")[1:2])
     x0<-rep(x[i]-caplen, 2)
     x1<-rep(x[i]+caplen, 2)
     y0<-rep(c(y[i]-llim[i],y[i]+ulim[i]),2)
     y1<-rep(c(y[i]-llim[i],y[i]+ulim[i]),2)
     segments(x0,y0,x1,y1,...)
    }
    else {
     caplen<-arrow.cap*par("pin")[1]
     x0<-x1<-rep(x[i],2)
     y0<-c(y[i]+arrow.gap,y[i]-arrow.gap)
     y1<-c(y[i]+ulim[i],y[i]-llim[i])
     arrows(x0,y0,x1,y1,length=caplen,angle=90,...)
    }
   }
  }
  if(toupper(type) == "L") {
  if(!is.na(fill)) polygon(c(x,rev(x)),c(y+ulim,rev(y-llim)),col=fill,border=NA)
   lines(x,y+ulim,...)
   lines(x,y-llim,...)
  }
 }
}
