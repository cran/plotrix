hexagon<-function(x,y,unitcell=1,col=NA,border="black") {
 polygon(c(x,x,x+unitcell/2,x+unitcell,x+unitcell,x+unitcell/2),
  c(y+unitcell*0.125,y+unitcell*0.875,y+unitcell*1.125,,y+unitcell*0.875,
    y+unitcell*0.125,y-unitcell*0.125),col=col,border=border)
}

color2D.matplot<-function(x,redrange=c(0,1),greenrange=c(0,1),bluerange=c(0,1),
 show.legend=FALSE,xlab="Column",ylab="Row",do.hex=FALSE,...) {
 
 if(is.matrix(x) || is.data.frame(x)) {
  xdim<-dim(x)
  if(is.data.frame(x)) x<-unlist(x)
  else x<-as.vector(x)
  oldpar<-par(no.readonly=TRUE)
  par(xaxs="i",yaxs="i")
  plot(c(0,xdim[2]),c(0,xdim[1]),xlab=xlab,ylab=ylab,type="n",axes=FALSE,...)
  if(!do.hex) {
   box()
   pos<-0
  }
  else pos<--0.3
  axis(1,at=pretty(0:xdim[2])[-1]-0.5,labels=pretty(0:xdim[2])[-1],pos=pos)
  yticks<-pretty(0:xdim[1])[-1]
  axis(2,at=xdim[1]-yticks+0.5,yticks)
  cellcolors<-color.scale(as.vector(x),redrange,greenrange,bluerange)
  # start from the top left - isomorphic with the matrix layout
  if(do.hex) {
   par(xpd=TRUE)
   offset<-0
   for(row in seq(xdim[1],1,by=-1)) {
    for(column in 0:(xdim[2]-1))
     hexagon(column+offset,row-1,col=cellcolors[row+column*(xdim[1]-1)])
    offset<-ifelse(offset,0,0.5)
   }
   par(xpd=FALSE)
  }
  else
   rect(sort(rep((1:xdim[2])-1,xdim[1])),rep(seq(xdim[1]-1,0,by=-1),xdim[2]),
    sort(rep(1:xdim[2],xdim[1])),rep(seq(xdim[1],1,by=-1),xdim[2]),
    col=cellcolors,border=FALSE)
  grx1<--xdim[1]/15
  gry1<--xdim[2]/5.5
  grx2<-grx1+xdim[1]/3
  gry2<-gry1+xdim[2]/20
  if(show.legend) {
   par(xpd=TRUE)
   gradient.rect(grx1,gry1,grx2,gry2,redrange,greenrange,bluerange,nslices=10)
   par(xpd=FALSE)
   mtext(round(range(x),2),1,2,at=c(grx1,grx2))
  }
  par(oldpar)
 }
 else cat("x must be a data frame or matrix\n")
}
