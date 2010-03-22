hexagon<-function(x,y,unitcell=1,col=NA,border="black") {
 polygon(c(x,x,x+unitcell/2,x+unitcell,x+unitcell,x+unitcell/2),
  c(y+unitcell*0.125,y+unitcell*0.875,y+unitcell*1.125,y+unitcell*0.875,
    y+unitcell*0.125,y-unitcell*0.125),col=col,border=border)
}

color2D.matplot<-function(x,redrange=c(0,1),greenrange=c(0,1),bluerange=c(0,1),
 extremes=NA,cellcolors=NA,show.legend=FALSE,nslices=10,xlab="Column",
 ylab="Row",do.hex=FALSE,axes=TRUE,show.values=FALSE,vcol="white",vcex=1,
 border="black",na.color=NA,...) {
 
 if(is.matrix(x) || is.data.frame(x)) {
  xdim<-dim(x)
  if(is.data.frame(x)) x<-unlist(x)
  else x<-as.vector(x)
  oldpar<-par("xaxs","yaxs","xpd","mar")
  par(xaxs="i",yaxs="i")
  if(do.hex) par(mar=c(5,4,4,4))
  plot(c(0,xdim[2]),c(0,xdim[1]),xlab=xlab,ylab=ylab,type="n",axes=FALSE,...)
  oldpar$usr<-par("usr")
  if(!do.hex) {
   box()
   pos<-0
  }
  else pos<- -0.3
  if(axes) {
   xticks<-pretty(0:xdim[2])[-1]
   axis(1,at=xticks-0.5,labels=xticks,pos=pos)
   yticks<-pretty(0:xdim[1])[-1]
   axis(2,at=xdim[1]-yticks+0.5,yticks)
  }
  if(all(is.na(cellcolors)))
   cellcolors<-color.scale(x,redrange,greenrange,bluerange,extremes,na.color)
  # start from the top left - isomorphic with the matrix layout
  if(do.hex) {
   par(xpd=TRUE)
   offset<-0
   if(length(border) < xdim[1]*xdim[2])
    border<-rep(border,length.out=xdim[1]*xdim[2])
   for(row in 1:xdim[1]) {
    for(column in 0:(xdim[2]-1)) {
     hexagon(column+offset,xdim[1]-row,col=cellcolors[row+xdim[1]*column],
      border=border[row+xdim[1]*column])
     if(show.values)
      text(column+offset+0.5,xdim[1]-row+0.5,x[row+column*xdim[1]],
       col=vcol,cex=vcex)
    }
    offset<-ifelse(offset,0,0.5)
   }
   par(xpd=FALSE)
  }
  else {
   rect(sort(rep((1:xdim[2])-1,xdim[1])),rep(seq(xdim[1]-1,0,by=-1),xdim[2]),
    sort(rep(1:xdim[2],xdim[1])),rep(seq(xdim[1],1,by=-1),xdim[2]),
    col=cellcolors,border=FALSE)
   if(show.values)
    text(sort(rep((1:xdim[2])-0.5,xdim[1])),
     rep(seq(xdim[1]-0.5,0,by=-1),xdim[2]),
     round(x,show.values),col=vcol,cex=vcex)
  }
  naxs<-which(is.na(x))
  xy<-par("usr")
  plot.din<-par("din")
  plot.pin<-par("pin")
  bottom.gap<-(xy[3]-xy[4])*(plot.din[2]-plot.pin[2])/(2*plot.pin[2])
  grx1<-xy[1]
  gry1<-bottom.gap*0.95
  grx2<-xy[1]+(xy[2]-xy[1])/4
  gry2<-bottom.gap*0.8
  if(length(cellcolors) > 1) {
   colmat<-col2rgb(c(cellcolors[which.min(x)],cellcolors[which.max(x)]))
   redrange<-colmat[1,]/255
   greenrange<-colmat[2,]/255
   bluerange<-colmat[3,]/255
  }
  rect.col<-color.scale(1:nslices,redrange,greenrange,bluerange)
  if(show.legend)
   color.legend(grx1,gry1,grx2,gry2,round(range(x,na.rm=TRUE),show.legend),
    rect.col=rect.col)
  par(oldpar)
 }
 else cat("x must be a data frame or matrix\n")
}
