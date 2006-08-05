std.error<-function(x,na.rm) {
 stderr<-sd(x,na.rm=TRUE)
 return(stderr/sqrt(length(x)))
}

# in general, get.segs expects a list with varying lengths of values
# it returns a 4xn matrix of midpoints, upper and lower limits and Ns
# where n is the number of elements in the list or columns in a data frame.

get.segs<-function(x,midpoint="mean",lower.limit="std.error",
 upper.limit="std.error") {

 xlen<-length(x)
 segs<-matrix(0,nrow=4,ncol=xlen)
 for(i in 1:xlen) {
  segs[1,i]<-do.call(midpoint,list(x[[i]],na.rm=TRUE))
  segs[2,i]<-segs[1,i]-do.call(lower.limit,list(x[[i]],na.rm=TRUE))
  segs[3,i]<-segs[1,i]+do.call(upper.limit,list(x[[i]],na.rm=TRUE))
  segs[4,i]<-sum(!is.na(x[[i]]))
 }
 colnames(segs)<-names(x)
 return(segs)
}

centipede.plot<-function(segs,midpoint="mean",lower.limit="std.error",
 upper.limit="std.err",left.labels=NULL,right.labels=NULL,sort.segs=TRUE,
 main="",xlab="",vgrid=NA,mar=NA,col=par("fg"),bg="green",...) {

 if(missing(segs))
  stop("Usage: centipede.plot(segs,...)\n\twhere segs is a matrix of midpoints and limits")
 segdim<-dim(segs)
 if(sort.segs) segs<-segs[,order(segs[1,])]
 oldpar<-par(no.readonly=TRUE)
 if(is.na(mar)) mar=c(4,6,1+2*(nchar(main)>0),5)
 par(mar=mar)
 plot(x=c(min(segs[2,]),max(segs[3,])),y=c(1,segdim[2]),
  main=main,xlab="",ylab="",type="n",axes=FALSE,...)
 box()
 if(!is.na(vgrid)) abline(v=vgrid,lty=2)
 axis(1)
 arrows(segs[2,],1:segdim[2],segs[3,],1:segdim[2],
  length=0.05,angle=90,code=3,col=col)
 points(segs[1,],1:segdim[2],pch=21,col=col,bg=bg)
 if(is.null(left.labels)) {
  left.labels<-colnames(segs)
  if(is.null(left.labels))
   left.labels<-paste("V",1:segdim[2],sep="")
 }
 plot.limits<-par("usr")
 mtext(left.labels,2,line=0.2,at=1:segdim[2],adj=1,las=1)
 if(is.null(right.labels))
  right.labels<-paste(round(segs[1,],2),segs[4,],sep=";")
 mtext(right.labels,4,line=0.2,at=1:segdim[2],adj=0,las=1)
 if(nchar(xlab)) mtext(xlab,1,line=2)
 par(oldpar)
}
