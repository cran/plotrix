intersectDiagram<-function(x,pct=FALSE,show.nulls=FALSE,xnames=NULL, 
 namesep="+",mar=c(0,0,3,0),main="Intersection Diagram",cex=1,
 col=NULL,minspacing=NA,space_transform=NA) {

 if(is.matrix(x) || is.data.frame(x))
  x<-makeIntersectList(x,xnames=xnames,sep=namesep)
 if(!match(class(x),"intersectList",0))
  stop("x must be a matrix, data frame or intersectList")
 oldmar<-par("mar")
 par(mar=mar)
 # attribute labels
 attributes<-x[[length(x)]]
 # total number of attributes
 nattributes<-length(attributes)
 # get rid of the attribute labels
 x[[length(x)]]<-NULL
 # total number of objects
 nobjects<-x[[length(x)]]
 # get rid of the total number of objects
 x[[length(x)]]<-NULL
 # number of intersection levels with at least one object
 nlevels<-length(x)
 # if no colors specified, use rainbow
 if(is.null(col)) col<-c(rainbow(nattributes),NA)
 else if(length(col) < nattributes) col<-rep(col,length.out=nattributes)
 # total number of objects for each intersection level
 objectsums<-sapply(x,sum)
 # level with the most objects
 maxlevel<-which.max(objectsums)
 nNonZero<-function(x) return(sum(x>0))
 maxintersections<-max(sapply(x,nNonZero))
 maxn<-max(unlist(x))
 if(is.na(minspacing)) minspacing<-0.1*maxn
 maxx<-objectsums[maxlevel]+minspacing*maxintersections+1
 attsep<-paste("[",namesep,"]",sep="")
 plot(0,xlim=c(0,maxx),ylim=c(0,nlevels+show.nulls),
  main=main,xlab="",ylab="",type="n",axes=FALSE)
 # step through each level of intersections
 for(level in 1:nlevels) {
  # determine the intersect level by the number of elements in the first name
  intersectLevel<-length(unlist(strsplit(names(x[[level]][1]),attsep)))
  # indices of intersections with at least one object in this level
  intersections<-which(x[[level]] > 0)
  # get all the names in this level with at least one object
  blocknames<-names(x[[level]][intersections])
  # proportions of the row for each intersection rectangle
  if(is.na(space_transform))
   centers<-c(0,x[[level]][intersections])/objectsums[level]
  else {
   centers<-c(0,do.call(space_transform,list(x[[level]][intersections])))/
    sum(do.call(space_transform,list(x[[level]][intersections])))
  }
  # centers of the rectangles in x positions
  centers<-
   maxx*cumsum((centers[1:length(intersections)]+centers[2:length(centers)])/2)
  for(intersect in 1:length(intersections)) {
   # make the label for the intersection
   cellqnt<-ifelse(pct,
    paste(round(100*x[[level]][intersections[intersect]]/nobjects,1),
     "%",sep=""),
    x[[level]][intersections[intersect]]) 
   # start halfway back from the center
   leftx<-centers[intersect]-x[[level]][intersections[intersect]]/2
   # indices of the colors to use
   colindex<-
    which(attributes %in% unlist(strsplit(blocknames[intersect],attsep)))
   # number of colors
   ncol<-length(colindex)
   # width of each color slice
   xinc<-x[[level]][intersections[intersect]]/ncol
   # colors for the slices
   slicecol<-col[colindex]
   # step through the slices
   for(slice in 1:ncol) {
    # draw a rectangle with no border
    rect(leftx,nlevels-level+show.nulls+0.1,
     leftx+xinc,nlevels-level+show.nulls+0.9,
     col=slicecol[slice],border=NA)
    # move to the next slice
    leftx<-leftx+xinc
   }
   # restore the initial left edge
   leftx<-centers[intersect]-x[[level]][intersections[intersect]]/2
   # draw a box around the intersection rectangle
   rect(leftx,nlevels-level+show.nulls+0.1,
    leftx+x[[level]][intersections[intersect]],nlevels-level+show.nulls+0.9)
   # display the label
   boxed.labels(leftx+x[[level]][intersections[intersect]]/2, 
    nlevels-level+show.nulls+0.5,paste(blocknames[intersect], 
    cellqnt,sep="\n"),cex=cex)
  }
 }
 if(show.nulls) {
  # number of objects with no set membership
  nonset<-as.numeric(nobjects-sum(objectsums))
  # left edge of the rectangle
  leftx<-sum(par("usr")[1:2])/2-nonset/2
  # draw the rectangle
  if(nonset) rect(leftx,0.1,leftx+nonset,0.9)
  # center of the rectangle
  xpos<-leftx+nonset/2
  # display the label
  if(pct) nonset<-paste(round(100*nonset/nobjects,1),"%",sep="")
  boxed.labels(xpos,0.5,paste("Non-members",nonset,sep="\n"),cex=cex)
 }
 par(mar = oldmar)
 invisible(x)
}
