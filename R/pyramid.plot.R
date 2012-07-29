pyramid.plot<-function(lx,rx,labels=NA,top.labels=c("Male","Age","Female"),
 main="",laxlab=NULL,raxlab=NULL,unit="%",lxcol,rxcol,gap=1,ppmar=c(4,2,4,2),
 labelcex=1,add=FALSE,xlim,show.values=FALSE,ndig=1,do.first=NULL) {

 xdim<-length(dim(lx))
 if(missing(rx) && xdim > 0) {
  rx<-lx[,2]
  lx<-lx[,1]
 }
 if(any(c(lx,rx)<0)) stop("Negative quantities not allowed")
 ncats<-ifelse(xdim,dim(lx)[1],length(lx))
 if(length(labels)==1) labels<-1:ncats
 ldim<-length(dim(labels))
 nlabels<-ifelse(ldim,length(labels[,1]),length(labels))
 if(nlabels != ncats) stop("lx and labels must all be the same length")
 if(missing(xlim))
  xlim<-rep(ifelse(xdim,ceiling(max(c(rowSums(lx),rowSums(rx)))),
   ceiling(max(c(lx,rx)))),2)
 if(!is.null(laxlab) && xlim[1] < max(laxlab)) xlim[1]<-max(laxlab)
 if(!is.null(raxlab) && xlim[2] < max(raxlab)) xlim[2]<-max(raxlab)
 oldmar<-par("mar")
 if(!add) {
  par(mar=ppmar,cex.axis=labelcex)
  # create an empty plot
  plot(0,xlim=c(-(xlim[1]+gap),xlim[2]+gap),ylim=c(0,ncats+1),
   type="n",axes=FALSE,xlab="",ylab="",xaxs="i",yaxs="i",main=main)
  if(!is.null(do.first)) eval(do.first)
  # add the bottom axes
  if(is.null(laxlab)) {
   laxlab<-seq(xlim[1]-gap,0,by=-1)
   axis(1,at=-xlim[1]:-gap,labels=laxlab)
  }
  else axis(1,at=-(laxlab+gap),labels=laxlab)
  if(is.null(raxlab)) {
   raxlab<-0:(xlim[2]-gap)
   axis(1,at=gap:xlim[2],labels=raxlab)
  }
  else axis(1,at=raxlab+gap,labels=raxlab)
  if(gap > 0) {
   axis(2,at=1:ncats,labels=rep("",ncats),pos=gap,tcl=-0.25)
   axis(4,at=1:ncats,labels=rep("",ncats),pos=-gap,tcl=-0.25)
  }
  # display the category labels
  if(is.null(dim(labels))) {
   if(gap) text(0,1:ncats,labels,cex=labelcex)
   else {
    text(xlim[1],1:ncats,labels,cex=labelcex,adj=0)
    text(xlim[2],1:ncats,labels,cex=labelcex,adj=1)
   }
  }
  else {
   if(gap) {
    lpos<--gap
    rpos<-gap
   }
   else {
    lpos<--xlim[1]
    rpos<-xlim[2]
   }
   text(lpos,1:ncats,labels[,1],pos=4,cex=labelcex,adj=0)
   text(rpos,1:ncats,labels[,2],pos=2,cex=labelcex,adj=1)
  }
  # display the top or bottom labels
  mtext(top.labels,3,0,at=c(-xlim[1]/2,0,xlim[2]/2),adj=0.5,cex=labelcex)
  mtext(c(unit,unit),1,2,at=c(-xlim[1]/2,xlim[2]/2))
 }
 if(xdim == 0) {
  if(missing(lxcol)) lxcol<-rainbow(ncats)
  if(missing(rxcol)) rxcol<-rainbow(ncats)
  rect(-(lx+gap),1:ncats-0.4,rep(-gap,ncats),1:ncats+0.4,col=lxcol)
  rect(rep(gap,ncats),1:ncats-0.4,(rx+gap),1:ncats+0.4,col=rxcol)
  if(show.values) {
   par(xpd=TRUE)
   text(-(gap+lx),1:ncats,round(lx,ndig),pos=2,cex=labelcex)
   text(gap+rx,1:ncats,round(rx,ndig),pos=4,cex=labelcex)
   par(xpd=FALSE)
  }
 }
 else {
  nstack<-dim(lx)[2]
  if(missing(lxcol)) lxcol <- rainbow(nstack)
  if(missing(rxcol)) rxcol <- rainbow(nstack)
  lxstart<-rxstart<-rep(gap,ncats)
  for(i in 1:nstack) {
   lxcolor<-rep(lxcol[i],ncats)
   rxcolor<-rep(rxcol[i],ncats)
   rect(-(lx[,i]+lxstart),1:ncats-0.4,-lxstart,1:ncats+0.4,col=lxcolor)
   rect(rxstart,1:ncats-0.4,rx[,i]+rxstart,1:ncats+0.4,col=rxcolor)
   lxstart<-lx[,i]+lxstart
   rxstart<-rx[,i]+rxstart
  }
 }
 return(oldmar)
}
