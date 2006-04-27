# display a pie chart at an arbitrary location on an existing plot

floating.pie<-function (xpos,ypos,x,edges=200,radius=1,col=NULL,
 startpos=0,...) {
 if (!is.numeric(x) || any(is.na(x) | x<=0))
  stop("floating.pie: x values must be positive.")
 x<-c(0,cumsum(x)/sum(x))
 dx <- diff(x)
 nx <- length(dx)
 if (is.null(col)) col<-rainbow(nx)
 else if(length(col) < nx) col<-rep(col,nx)
 # get the center values in radians
 bc<-2*pi*(x[1:nx]+dx/2)+startpos
 for(i in 1:nx) {
  n<-max(2,floor(edges*dx[i]))
  t2p<-2*pi*seq(x[i],x[i + 1],length=n)+startpos
  xc<-c(cos(t2p)*radius+xpos,xpos)
  yc<-c(sin(t2p)*radius+ypos,ypos)
  polygon(xc,yc,col=col[i],...)
  t2p<-2*pi*mean(x[i+0:1])+startpos
  xc<-cos(t2p)*radius
  yc<-sin(t2p)*radius
  lines(c(1,1.05)*xc,c(1,1.05)*yc)
 }
 return(bc)
}

# place text labels at the specified distance from x,y on the radial lines
# specified by angles.

pie.labels<-function(x,y,angles,labels,radius=1,bg="white",border=TRUE,...) {
 if(nargs()<4)
  stop("Usage: pie.labels(x,y,angles,labels,radius=1,bg=\"white\",border=TRUE,...)")
 xc<-cos(angles)*radius+x
 yc<-sin(angles)*radius+y
 boxed.labels(xc,yc,labels,bg=bg,border=border,...)
}
