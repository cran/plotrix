spread.labels<-function (x,y,labels=NULL,offset,bg="white",border=FALSE,
 between=FALSE,linecol=par("fg"),...) {

 if(missing(x)) 
  stop("Usage: spread.labels(x,y,labels,...)")
 ny<-length(y)
 if(between) {
  if(length(linecol)==1) linecol<-rep(linecol,2)
  nlabels<-length(labels)
  newy<-seq(y[1],y[ny],length=nlabels)
  # put the labels in the middle
  labelx<-rep(mean(x),nlabels)
  # do the left lines
  segments(x[1:nlabels],y[1:nlabels],labelx-strwidth(labels)/2,newy,
   col=linecol[1])
  # now the right lines
  segments(x[(nlabels+1):ny],y[(nlabels+1):ny],labelx+strwidth(labels)/2,newy,
   col=linecol[2])
  boxed.labels(labelx,newy,labels,bg=bg,border=border,...)
 }
 else {
  if(diff(range(x))<diff(range(y))) {
   sort.index<-sort.list(y)
   x<-x[sort.index]
   y<-y[sort.index]
   newy<-seq(y[1],y[ny],length=length(labels))
   if(missing(offset)) offset<-diff(par("usr")[1:2])/4
   offsets<-rep(c(offset, -offset), ny/2 + 1)[1:ny]
   segments(x+offsets,newy,x,y)
   boxed.labels(x+offsets,newy,labels[sort.index],bg=bg,border=border,...)
  }
  else {
   sort.index<-sort.list(x)
   x<-x[sort.index]
   y<-y[sort.index]
   nx<-length(x)
   newx <- seq(x[1],x[nx],length=length(labels))
   if(missing(offset)) offset<-diff(par("usr")[3:4])/4
   offsets<-rep(c(offset,-offset),nx/2+1)[1:nx]
   segments(newx,y+offsets,x,y)
   boxed.labels(newx,y+offsets,labels[sort.index],bg=bg,border=border,...)
  }
 }
}