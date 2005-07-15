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
 # set any congruent points to N/S labels or they'll overprint
 for(i in 1:lenx) {
  if(!xdiff[i] & !ydiff[i])
   dir.away[c(i,nearest.index[i])]<-c(1,3)
 }
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

# thigmophobe.points moves points closer than 'tol' away from
# each other by 'away'

thigmophobe.points<-function(x,y,away=NULL,tol=NULL) {
 if(missing(x))
  stop("Usage: thigmophobe.points(x,y,away=NULL,tol=NULL)")
 dimx<-dim(x)
 # if x is a data frame or matrix with at least two columns, split it
 if(missing(y) && !is.null(dimx)) {
  y<-x[,2]
  x<-x[,1]
 }
 xlen<-length(x)
 if(xlen != length(y)) stop("x and y must be the same length.")
 if(is.null(away)) away<-c(strwidth("o")/4,strheight("o")/4)
 if(is.null(tol)) tol<-c(strwidth("o")/2,strheight("o")/2)
 # use 'pos' to index offset
 away.x<-c(0,-away[1],0,away[1])
 away.y<-c(-away[2],0,away[2],0)
 away.index<-thigmophobe(x,y)
 flags<-1:xlen
 for(i in 1:xlen) {
  if(!is.na(flags[i])) {
   overplots<-abs(x - x[i]) <= tol[1] & abs(y - y[i]) <= tol[2]
   if(sum(overplots) > 1) {
    for(j in 1:xlen) {
     if(overplots[j]) {
      x[j]<-x[j]+away.x[away.index[j]]
      y[j]<-y[j]+away.y[away.index[j]]
     }
    }
   }
  }
  flags[overplots]<-NA
 }
 return(list(x=x,y=y))
}
