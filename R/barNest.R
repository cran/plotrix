barNest<-function(formula=NULL,data=NULL,FUN=c("mean","sd"),
 x=NULL,ylim=NULL,main="",xlab="",ylab="",shrink=0.1,errbars=FALSE,
 col=NA,labelcex=1,lineht=NA,showall=TRUE,barlabels=NULL,
 showlabels=TRUE,mar=NULL,arrow.cap=0.01,trueval=NA) {

 if(is.null(x)) x<-brkdnNest(formula=formula,data=data,FUN=FUN,trueval=trueval)
 else if(!is.list(x)) warning("x is not a list, and this is very unlikely to work")
 if(is.null(ylim)) {
  lenx<-length(x)
  if(errbars) {
   if(lenx == 2) x[[3]]<-x[[2]]
   ylim<-c(min(unlist(x[[1]])-unlist(x[[2]]),na.rm=TRUE),
    max(unlist(x[[1]])+unlist(x[[3]]),na.rm=TRUE))
  }
  else {
   funname<-names(x[[1]][[1]])[2]
   ylim<-range(unlist(lapply(x[[1]],"[",funname)),na.rm=TRUE)
  }
  ylim<-ylim+c(ifelse(ylim[1]<0,-0.04,0),0.04)*diff(ylim)
  if(ylim[1] > 0) ylim[1]<-0
 }
 if(!is.null(mar)) oldmar<-par(mar=mar)
 plot(0,xlim=c(0,1),ylim=ylim,main=main,xlab=xlab,ylab=ylab,xaxt="n",yaxs="i",type="n")
 parusr<-par("usr")
 if(is.na(lineht))
  lineht<-labelcex*diff(parusr[3:4])*(par("mai")[1]/par("pin")[2])/par("mar")[1]
 nlevels=length(x[[1]])
 drawNestedBars(x,start=0,end=1,shrink=shrink,errbars=errbars,
  col=col,labelcex=labelcex,lineht=lineht,showall=showall,
  showlabels=showlabels,arrow.cap=arrow.cap)
 abline(h=0)
 if(!is.null(mar)) par(mar=oldmar)
 invisible(x)
}
