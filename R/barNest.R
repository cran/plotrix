barNest<-function(formula=NULL,data=NULL,FUN=c("mean","sd"),
 ylim=NULL,main="",xlab="",ylab="",shrink=0.1,errbars=FALSE,
 col=NA,labelcex=1,lineht=NA,showall=TRUE,barlabels=NULL,
 showlabels=TRUE,mar=NULL,arrow.cap=0.01,trueval=NA) {

 x<-brkdnNest(formula=formula,data=data,FUN=FUN,trueval=trueval)
 nbn<-length(as.character(attr(terms(formula),"variables")[-1]))
 if(is.null(ylim)) {
  lenx<-length(x)
  if(errbars) {
   errbars<-lenx
   if(lenx == 3)
    ylim<-c(min(unlist(x[[3]][[nbn]][,nbn]),na.rm=TRUE),
     max(unlist(x[[2]][[nbn]][,nbn]),na.rm=TRUE))
   else {
    x[[3]]<-x[[2]]
    ylim<-c(min(unlist(x[[1]][[nbn]][,nbn])-unlist(x[[3]][[nbn]][,nbn]),na.rm=TRUE),
     max(unlist(x[[1]][[nbn]][,nbn])+unlist(x[[2]][[nbn]][,nbn]),na.rm=TRUE))
    if(ylim[1] < 0) ylim[1]<-0
   }
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
  barlabels=barlabels,showlabels=showlabels,arrow.cap=arrow.cap)
 abline(h=0)
 if(FUN[1]=="valid.n") box()
 if(!is.null(mar)) par(mar=oldmar)
 invisible(x)
}
