count.overplot<-function(x,y,tol=NULL,...) {
 if(missing(x))
  stop("Usage: count.overplot(x,y,tol=NULL,...)")
 dimx<-dim(x)
 # if x is a data frame or matrix with at least two columns, split it
 if(missing(y) && !is.null(dimx)) {
  y<-x[,2]
  x<-x[,1]
 }
 # get rid of any pairs containing NA
 if(any(is.na(x)|is.na(y))) {
  indices<-!is.na(x)&!is.na(y)
  x<-x[indices]
  y<-y[indices]
 }
 xlim<-range(x)
 ylim<-range(y)
 xlen<-length(x)
 if(xlen != length(y)) stop("x and y must be the same length.")
 if(is.null(tol)) tol<-c(strwidth("o")/2,strheight("o")/2)
 flags<-1:xlen
 xsep<-ysep<-xdup<-ydup<-xydup<-rep(0,xlen)
 nsep<-ndup<-0
 for(i in 1:xlen) {
  if(!is.na(flags[i])) {
   dups<-abs(x - x[i]) <= tol[1] & abs(y - y[i]) <= tol[2]
   ndups<-sum(dups)
   if(ndups > 1) {
    ndup<-ndup+1
    xydup[ndup]<-ndups
    xdup[ndup]<-x[i]
    ydup[ndup]<-y[i]
   }
   else {
    nsep<-nsep+1
    xsep[nsep]<-x[i]
    ysep[nsep]<-y[i]
   }
  }
  flags[dups]<-NA
 }
 plot(xsep[1:nsep],ysep[1:nsep],xlim=xlim,ylim=ylim,...)
 text(xdup[1:ndup],ydup[1:ndup],xydup[1:ndup])
}
