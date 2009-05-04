bumpchart<-function (y,top.labels=colnames(y),labels=rownames(y),rank=TRUE,
 mar=c(2,8,5,8),pch=19,...) {

 if(missing(y)) 
  stop("Usage: spread.labels(y,labels,...)")
 ydim<-dim(y)
 if(is.null(ydim)) stop("y must be a matrix or data frame")
 oldmar<-par("mar")
 par(mar=mar)
 if(rank) y<-apply(y,2,rank)
 labels<-rev(labels)
 y<-apply(y,2,rev)
 ny<-length(y)
 matplot(t(y),ylab="",type="b",pch=pch,axes=FALSE,...)
 par(xpd=TRUE)
 xylim<-par("usr")
 minspacing<-strheight("M")*1.5
 text(1:ydim[2],xylim[4],top.labels)
 labelpos<-spreadout(y[,1],minspacing)
 text(xylim[1],labelpos,labels,adj=1)
 labelpos<-spreadout(y[,ydim[2]],minspacing)
 text(xylim[2],labelpos,labels,adj=0)
 par(mar=oldmar,xpd=FALSE)
}
