# display a pie chart at an arbitrary location on an existing plot

floating.pie<-function (xpos,ypos,x,edges=200,radius=1,col=NULL,...) {
 if (!is.numeric(x) || any(is.na(x) | x<=0))
  stop("floating.pie: `sectors' values must be positive.")
 x <- c(0, cumsum(x)/sum(x))
 dx <- diff(x)
 nx <- length(dx)
 if (is.null(col)) col<-rainbow(nx)
 else if(length(col) < nx) col<-rep(col,nx)
 # get the center values in radians
 bc<-2*pi*(x[1:nx]+dx/2)
 for (i in 1:nx) {
  n <- max(2, floor(edges * dx[i]))
  t2p <- 2 * pi * seq(x[i], x[i + 1], length = n)
  xc <- c(cos(t2p)+xpos,xpos) * radius
  yc <- c(sin(t2p)+ypos,ypos) * radius
  polygon(xc, yc, col = col[i],...)
  t2p <- 2 * pi * mean(x[i + 0:1])
  xc <- cos(t2p) * radius
  yc <- sin(t2p) * radius
  lines(c(1, 1.05) * xc, c(1, 1.05) * yc)
 }
 return(bc)
}

# place text labels on a pie chart centered at x,y

pie.labels<-function(x,y,angles,labels,radius=1,col="white",border=TRUE,...) {
 if(nargs()<4)
  stop("Usage: pie.labels(x,y,angles,labels,radius=1,col=\"white\",border=TRUE,...)")
 xc<-cos(angles)*radius+x
 yc<-sin(angles)*radius+y
 boxed.labels(xc,yc,labels,col=col,border=border,...)
}
