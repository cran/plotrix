barNest<-function(formula=NULL,data=NULL,maxlevels=10,
 mct=mean,lmd=std.error,umd=lmd,x=NULL,ylim=NULL,
 main="",xlab="",ylab="",shrink=0.1,errbars=FALSE,
 col=NA,labelcex=1,lineht=NA,showall=FALSE,barlabels=NULL,
 showlabels=TRUE,mar=NULL,arrow.cap=0.01,trueval=NA) {

 if(is.null(x))
  x<-brkdnNest(formula=formula,data=data,maxlevels=maxlevels,
   mct=mct,lmd=lmd,umd=umd,trueval=trueval)
 else
  if(!is.list(x)) warning("x is not a list, and this is very unlikely to work")
 if(!is.na(trueval)) errbars=FALSE
 if(!is.null(barlabels)) x[[4]]<-barlabels
 if(is.null(ylim)) {
  ymin<-min(c(0,unlist(x[[1]])-errbars*unlist(x[[2]])),na.rm=TRUE)
  if(ymin < 0) ymin<-ymin*1.02
  if(all(is.na(unlist(x[[3]]))))
   ymax<-max(unlist(x[[1]]),na.rm=TRUE)
  else
   ymax<-max(c(max(unlist(x[[1]]),na.rm=TRUE),
    max(unlist(x[[1]])+errbars*unlist(x[[3]]),na.rm=TRUE)))
  ylim<-c(ymin,ymax*1.02)
  if(!is.null(mar)) par(mar=mar)
 }
 nlevels<-length(x[[1]])
 plot(0,xlim=c(0,1),ylim=ylim,main=main,xlab=xlab,ylab=ylab,xaxt="n",
   yaxs="i",type="n")
 parusr<-par("usr")
 if(is.na(lineht))
  lineht<-diff(parusr[3:4])*(par("mai")[1]/par("pin")[2])/par("mar")[1]
 if(is.list(col)) nextcol<-col[[1]]
 else nextcol<-col
 nextx<-list(x[[1]][[1]],x[[2]][[1]],x[[3]][[1]],"Overall")
 drawNestedBars(nextx,start=0,end=1,shrink=0,labely=-lineht*nlevels,
  col=nextcol,labelcex=labelcex,showbars=showall,showlabels=showlabels,
  arrow.cap=arrow.cap)
 for(brklevel in 2:nlevels) {
  if(showall || showlabels || brklevel == nlevels) {
   showbars<-showall || brklevel == nlevels
   if(is.list(col)) nextcol<-col[[brklevel]]
   else nextcol<-col
   nextx<-list(x[[1]][[brklevel]],x[[2]][[brklevel]],x[[3]][[brklevel]],
    x[[4]][[brklevel]])
   drawNestedBars(nextx,start=0,end=1,shrink=shrink,
    labely=-lineht*(nlevels+1-brklevel),errbars=errbars && brklevel == nlevels,
    col=nextcol,labelcex=labelcex,showbars=showbars,showlabels=showlabels,
    arrow.cap=arrow.cap)
  }
 }  
 invisible(x)
}
