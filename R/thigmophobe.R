# thigmophobe returns the direction (as 1|2|3|4 - see pos= in the text function) 
# _away_ from the nearest point where x and y are vectors of 2D coordinates

thigmophobe<-function(x,y) {
 # get the current upper and lower limits of the plot
 plot.span<-par("usr")
 x.span<-plot.span[2] - plot.span[1]
 y.span<-plot.span[4] - plot.span[3]
 # if either axis is logarithmic, transform the values into logarithms
 if(par("xlog")) x<-log(x)
 if(par("ylog")) y<-log(y)
 # scale the values to the plot span
 # this avoids the numerically larger
 # axis dominating the distance measure
 x<-x/x.span
 y<-y/y.span
 # get the distance matrix as a full matrix
 xy.dist<-as.matrix(dist(cbind(x,y)))
 lenx<-length(x)
 nearest.index<-rep(0,lenx)
 for(index in 1:lenx)
  nearest.index[index]<-as.numeric(names(which.min(xy.dist[-index,index])))
 # get the x and y differences for each point to the nearest point
 xdiff<-x - x[nearest.index]
 ydiff<-y - y[nearest.index]
 # first set the east/west direction
 dir.ew<-ifelse(xdiff > 0,4,2)
 # now do the north/south
 dir.ns<-ifelse(ydiff > 0,3,1)
 dir.away<-ifelse(abs(xdiff)>abs(ydiff),dir.ew,dir.ns)
 return(dir.away)
}

# thigmophobe.labels positions labels at points so that they
# are most distant from the nearest other point, where the
# points are described as x and y coordinates.

thigmophobe.labels<-function(x,y,labels=1:length(x),...) {
 
 if(!missing(x) && !missing(y)) {
  text.pos<-thigmophobe(x,y)
  text(x,y,labels,pos=text.pos,...)
 }
 else
  cat("Usage: thigmophobe.labels(x,y,labels=1:length(x),...)\n")
}
