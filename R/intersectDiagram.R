intersectDiagram<-function(x,pct=FALSE,show.nulls=FALSE,mar=c(0,0,3,0),
 main="Intersection Diagram",col=NULL,minspacing=0.1) {

 if(is.matrix(x) || is.data.frame(x)) x<-makeIntersectList(x)
 oldmar<-par("mar")
 par(mar=mar)
 # don't count the total in the last element of the list
 lenx<-length(x)-1
 xtotal<-x[[length(x)]]
 xnames<-names(x[[1]])
 if(is.null(xnames)) xnames<-LETTERS[1:lenx]
 if(is.null(col)) col<-c(rainbow(length(x)-1),NA)
 listsums<-sapply(x,sum)
 horizmax<-max(listsums)
 xsum<-0
 plot(0,xlim=c(0,horizmax*(1+minspacing)),ylim=c(0,lenx+show.nulls),
  main=main,xlab="",ylab="",type="n",axes=FALSE)
 for(comb in 1:lenx) {
  xsum<-xsum+sum(x[[comb]])
  blocknames<-names(x[[comb]])
  if(is.null(blocknames)) {
   rowmat<-combn(xnames,comb)
   blocknames<-pasteCols(rowmat)
  }
  lenxcomb<-length(x[[comb]])
  gap<-(horizmax*(1+minspacing)-sum(x[[comb]]))/lenxcomb
  startx<-gap/2
  for(intersect in 1:lenxcomb) {
   cellqnt<-ifelse(pct,
    paste(round(100*x[[comb]][intersect]/xtotal,1),"%",sep=""),
    x[[comb]][intersect])
   if(x[[comb]][intersect] > 0) {
    if(!is.na(col[1])) {
     lencol<-length(col)
     xinc<-x[[comb]][intersect]/comb
     slice<-1
     leftx<-startx
     for(bn in 1:lencol) {
      if(length(grep(xnames[bn],blocknames[intersect],fixed=TRUE))) {
       polygon(c(leftx,leftx,leftx+xinc,leftx+xinc),
        c(lenx+show.nulls-comb+0.1,lenx+show.nulls-comb+0.9,
	lenx+show.nulls-comb+0.9,lenx+show.nulls-comb+0.1),
	border=NA,col=col[bn])
        slice<-slice+1
        leftx<-leftx+xinc
      }
     }
    }
    rect(startx,lenx+show.nulls-comb+0.1,
     startx+x[[comb]][intersect],lenx+show.nulls-comb+0.9)
   }
   boxed.labels(startx+x[[comb]][intersect]/2,lenx+show.nulls-comb+0.5,
    paste(blocknames[intersect],cellqnt,sep="\n"))
   startx<-startx+x[[comb]][intersect]+gap
  }
 }
 if(show.nulls) {
  leftx<-sum(par("usr")[1:2])/2-x[[lenx]]/2
   polygon(c(leftx,leftx,leftx+x[[lenx]],leftx+x[[lenx]]), 
    c(0.1,0.9,0.9,0.1),col=NA)
   cellqnt<-xtotal-xsum
   if(pct) cellqnt<-paste(round(100*cellqnt/xtotal,1),"%",sep="")
   boxed.labels(leftx+x[[lenx]]/2,0.5,
    paste("Non-members",cellqnt,sep="\n"),col=col[length(col)])
 }
 par(mar=oldmar)
}
