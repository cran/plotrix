spread.labels<-function(x,y,labels=NULL,offset,bg="white",border=FALSE,...) {
 if(missing(x))
  stop("Usage: spread.labels(x,y,labels=NULL,offset,bg=\"white\",...)")
 if(diff(range(x)) < diff(range(y))) {
  sort.index<-sort.list(y)
  x<-x[sort.index]
  y<-y[sort.index]
  ny<-length(y)
  if(missing(offset)) offset<-diff(par("usr")[1:2])/4
  offsets<-rep(c(offset,-offset),ny/2+1)[1:ny]
  newy<-seq(y[1],y[ny],length=ny)
  segments(x+offsets,newy,x,y)
  boxed.labels(x+offsets,newy,labels,bg=bg,border=border,...)
 }
 else {
  sort.index<-sort.list(x)
  x<-x[sort.index]
  y<-y[sort.index]
  nx<-length(x)
  offsets<-rep(c(offset,-offset),nx/2+1)[1:nx]
  newx<-seq(x[1],x[nx],length=nx)
  segments(newx,y+offsets,x,y)
  boxed.labels(newx,y+offsets,labels,bg=bg,border=border,...)
 }
}
