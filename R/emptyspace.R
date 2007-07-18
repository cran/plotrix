# emptyspace finds the center of the a horizontal rectangle on a plot that
# has no points in x,y within it. It keeps dividing the plot into smaller and
# smaller rectangles until it finds at least one empty rectangle, so it may return
# the center of a very small space on a very crowded plot.
# It doesn't yet guarantee that the rectangle chosen will be the largest empty
# space on the plot.
# It can be used on barplots as well if the user passes the horizontal positions of
# the centers of the bars and sets bars to TRUE.

emptyspace<-function(x,y=NA,bars=FALSE) {
 xlim<-par("usr")[1:2]
 ylim<-par("usr")[3:4]
 if(is.na(y[1])) {
  if(is.list(x)) {
   # is x a list with x and y positions?
   if(names(x)[1] == "x") {
    y<-as.vector(x[[2]])
    x<-as.vector(x[[1]])
   }
   else {
    # check if x is a list from brkdn.plot with two elements (or maybe more later)
    if(length(x) > 1) {
     # string out the values of points and dispersion limits
     y<-c(as.vector(x[[1]]),as.vector(x[[1]]+x[[2]]),as.vector(x[[1]]-x[[2]]))
     xdim<-dim(x[[1]])
     # assume that the groups of points were centered on integer values
     x<-rep(rep(1:xdim[1],each=xdim[2]),3)
    }
    else stop("Can't compute points unless x is a list with at least two matrices")
   }
  }
 }
 # add points to fill up the space where the bars are
 if(bars) {
  botpos<-ifelse(any(y < 0),0,ylim[1])
  yinc<-diff(ylim)/10
  for(bar in 1:length(y)) {
   if(y[bar] < 0) newy<-seq(y[bar],0,by=yinc)
   else newy<-seq(botpos,y[bar],by=yinc)
   y<-c(y,newy)
   x<-c(x,rep(x[bar],length(newy)))
  }
 }
 # counts the number of points that lie within each subsection of the plot
 # returns a matrix with the number of rows and columns equal to one less than
 # the length of the columns in xysplits
 count.xy<-function(x,y,xysplits) {
  nbits<-dim(xysplits)[1]-1
  xycount<-matrix(NA,nrow=nbits,ncol=nbits)
  for(ybit in 1:nbits) {
   for(xbit in 1:nbits) {
    xycount[ybit,xbit]<-
     sum(x > xysplits[xbit,1] & x < xysplits[xbit+1,1] &
      y > xysplits[ybit,2] & y < xysplits[ybit+1,2])
   }
  }
  return(xycount)
 }
 nsplits<-1
 # this splits the plot into nsplits*nsplits equal rectangles
 # returns a matrix in which the first column is the x values of the edges of
 # the rectangles and the second column is the y values
 splitxy<-function(xlim,ylim,nsplits) {
  props<-c(0,1:nsplits/(nsplits+1),1)
  xybits<-cbind(xlim[1]+diff(xlim)*props,ylim[1]+diff(ylim)*props)
  return(xybits)
 }
 xysplits<-splitxy(xlim,ylim,nsplits)
 xycount<-count.xy(x,y,xysplits)
 while(min(xycount) > 0) {
  nsplits<-nsplits+1
  xysplits<-splitxy(xlim,ylim,nsplits)
  xycount<-count.xy(x,y,xysplits)
 }
 empty.cells<-xycount == 0
 emptiest.row<-which.max(apply(empty.cells,1,sum))
 # returns the indices of the start and end of the longest run of TRUE values
 # or non-zero values in x
 longest.run<-function(x) {
  thisrun<-bigrun<-c(0,0)
  for(i in 1:length(x)) {
   if(x[i]) {
    if(thisrun[1] == 0) thisrun[1]<-i
    thisrun[2]<-i
   }
   else {
    if(diff(thisrun) > diff(bigrun)) bigrun<-thisrun
    thisrun<-c(0,0)
   }
  }
  if(diff(thisrun) >= diff(bigrun)) bigrun<-thisrun
  return(bigrun)
 }
 if(sum(empty.cells[emptiest.row,]) > 1)
  emptiest.cols<-longest.run(empty.cells[emptiest.row,])
 else emptiest.cols<-rep(which(empty.cells[emptiest.row,]),2)
 # calculate the center of the emptiest row and longest run of empty columns
 return(list(x=(xysplits[emptiest.cols[1],1]+xysplits[emptiest.cols[2]+1,1])/2,
  y=(xysplits[emptiest.row,2]+xysplits[emptiest.row+1,2])/2))
}