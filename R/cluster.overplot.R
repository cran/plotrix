cluster.overplot<-function(x,y,away=NULL,tol=NULL) {
 if(missing(x))
  stop("Usage: cluster.overplot(x,y,away=NULL)")
 dimx<-dim(x)
 # if x is a data frame or matrix with at least two columns, split it
 if(missing(y) && !is.null(dimx)) {
  y<-x[,2]
  x<-x[,1]
 }
 xlen<-length(x)
 if(xlen != length(y)) stop("x and y must be the same length.")
 if(is.null(away)) away<-c((max(x)-min(x))/100,(max(y)-min(y))/100)
 if(is.null(tol)) tol<-c((max(x)-min(x))/1000,(max(y)-min(y))/1000)
 away.x<-c(0,-away[1],away[1],0,0,-away[1],away[1],-away[1],away[1])
 away.y<-c(0,0,0,-away[2],away[2],-away[2],away[2],away[2],-away[2])
 flags<-1:xlen
 for(i in 1:xlen) {
  if(!is.na(flags[i])) {
   duplicates<-abs(x - x[i]) <= tol[1] & abs(y - y[i]) <= tol[2]
   if(sum(duplicates) > 1) {
    away.index<-1
    for(j in 1:xlen) {
     if(duplicates[j]) {
      x[j]<-x[j]+away.x[away.index]
      y[j]<-y[j]+away.y[away.index]
      away.index<-away.index+1
      if(away.index > 9) away.index<-1
     }
    }
   }
  }
  flags[duplicates]<-NA
 }
 return(list(x=x,y=y))
}
