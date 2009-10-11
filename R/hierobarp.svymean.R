hierobarp.svymean<-function (x,meanvar,dispvar=NULL,xlim=NULL,ylim=NULL,
 main="",xlab="",ylab="",yticks=NULL,start=0,end=1,shrink=0.02,errbars=FALSE,
 col=NA,labelcex=1,lineht=NA,showall=TRUE,barlabels=NULL,showbrklab=TRUE,mar=NULL) {

 dimx<-dim(x)
 squeeze<-(end - start) * shrink
 if(start == 0) {
  if(is.null(xlim)) xlim<-c(start,end)
  if(is.null(ylim)) {
   ymin<-min(c(0,x[[meanvar]]-x[[dispvar]]),na.rm=TRUE)
   if(ymin < 0) ymin <- ymin * 1.02
   ymax<-max(x[[meanvar]]+x[[dispvar]],na.rm=TRUE)
   ylim<-c(ymin,ymax*1.02)
   if(!is.null(mar)) par(mar=mar)
  }
  plot(0,xlim=xlim,ylim=ylim,main=main,xlab=xlab,ylab=ylab,
   xaxt="n",yaxt="n",xaxs="i",yaxs="i",type="n")
  if(is.null(yticks)) axis(2)
  else staxlab(2,at=0:(length(yticks)-1),yticks)
  parusr<-par("usr")
  if(is.na(lineht)) 
   lineht<-diff(parusr[3:4])*(par("mai")[1]/par("pin")[2])/par("mar")[1]
  start<-start+squeeze
  end<-end-squeeze
  if(showall) {
   barcol<-ifelse(is.list(col),col[[1]],col)
   rect(start,0,end,mean(x[[meanvar]],na.rm=TRUE),col=barcol)
   par(xpd=TRUE)
   segments(c(start,end,start),
    c(rep(ylim[1],2),ylim[1]-lineht*(dim(x)[2]-1.5)),
    c(start,end,end),rep(ylim[1],3)-lineht*(dim(x)[2]-1.5))
   barname<-ifelse(is.null(barlabels),"Overall",barlabels[[1]])
   boxed.labels((start+end)/2,ylim[1]-lineht*(dim(x)[2]-1.5),barname,bg=barcol,
    cex=labelcex)
   par(xpd=FALSE)
  }
  if(is.list(col)) col[[1]]<-NULL
  if(is.list(barlabels)) barlabels[[1]]<-NULL
 }
 # start the recursive plotting
 barnames<-unique(x[,1])
 barnames<-barnames[!is.na(barnames)]
 nbars<-length(barnames)
 newwidth<-(end-start)/nbars
 newcol<-col
 newcol[[1]]<-NULL
 for(block in 1:nbars) {
  end<-start+newwidth
  xblock<-x[x[,1] == barnames[block],2:dimx[2]]
  if(showall) {
   rect(start+squeeze,0,end-squeeze,mean(xblock[[meanvar]],na.rm=TRUE),
    col=col[[1]][block])
   par(xpd=TRUE)
   if(dimx[2] > 3)
    segments(c(start+squeeze,end-squeeze,start+squeeze),
     c(rep(ylim[1],2),ylim[1]-lineht*(dimx[2]-2.5)),
     c(start+squeeze,end-squeeze,end-squeeze),
     rep(ylim[1],3)-lineht*(dimx[2]-2.5))
   else {
    if(errbars && !is.null(dispvar))
     dispersion((start+end)/2,xblock[[meanvar]],xblock[[dispvar]],
      arrow.cap=0.01)
   }
  }
  par(xpd=TRUE)
  boxed.labels((start+end)/2,ylim[1]-lineht*(dim(x)[2]-2.5),barnames[block],
   bg=col[[1]][block],cex=labelcex)
  par(xpd=FALSE)
  if(dimx[2] > 3)
   hierobarp.svymean(x=xblock,meanvar=meanvar,dispvar=dispvar,xlim=xlim,ylim=ylim,
    main = main,xlab=xlab,ylab=ylab,start=start+squeeze,end=end-squeeze,
    shrink = shrink*1.5,errbars=errbars,col=newcol,labelcex=labelcex,
    lineht=lineht,showall=showall,barlabels=barlabels,showbrklab=showbrklab)
  start<-end
 }
 invisible(x)
}
