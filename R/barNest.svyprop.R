barNest.svyprop<-function (x,truevar,falsevar,xlim=NULL,ylim=NULL,
 main="",xlab="",ylab="",yticks=NULL,start=0,end=1,shrink=0.02,errbars=FALSE,
 col=NA,labelcex=1,lineht=NA,showall=TRUE,barlabels=NULL,showbrklab=TRUE,mar=NULL) {

 dimx<-dim(x)
 squeeze<-(end - start) * shrink
 if(start == 0) {
  if(is.null(xlim)) xlim<-c(start,end)
  if(is.null(ylim)) ylim<-c(0,1.02)
  if(!is.null(mar)) par(mar=mar)
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
   rect(start,0,end,
    sum(x[[truevar]],na.rm=TRUE)/sum(x[[truevar]]+x[[falsevar]],na.rm=TRUE),
    col=barcol)
   par(xpd=TRUE)
   segments(c(start,end,start),
    c(rep(ylim[1],2),ylim[1]-lineht*(dim(x)[2]-3.5)),
    c(start,end,end),rep(ylim[1],3)-lineht*(dim(x)[2]-3.5))
   barname<-ifelse(is.null(barlabels),"Overall",barlabels[[1]])
   boxed.labels((start+end)/2,ylim[1]-lineht*(dim(x)[2]-3.5),barname,bg=barcol,
    cex=labelcex)
  # mtext(barname,side=1,line=dim(x)[2]-4,at=(start+end)/2,cex=labelcex)
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
   barheight<-
    sum(xblock[[truevar]],na.rm=TRUE)/sum(xblock[[truevar]]+xblock[[falsevar]])
   rect(start+squeeze,0,end-squeeze,barheight,col=col[[1]][block])
   par(xpd=TRUE)
   if(dimx[2] > 5)
    segments(c(start+squeeze,end-squeeze,start+squeeze),
     c(rep(ylim[1],2),ylim[1]-lineht*(dimx[2]-4.5)),
     c(start+squeeze,end-squeeze,end-squeeze),
     rep(ylim[1],3)-lineht*(dimx[2]-4.5))
  }
  par(xpd=TRUE)
  boxed.labels((start+end)/2,ylim[1]-lineht*(dim(x)[2]-4.5),barnames[block],
   bg=col[[1]][block],cex=labelcex)
  par(xpd=FALSE)
  if(dimx[2] > 5)
   barNest.svyprop(x=xblock,truevar=truevar,falsevar=falsevar,xlim=xlim,ylim=ylim,
    main = main,xlab=xlab,ylab=ylab,start=start+squeeze,end=end-squeeze,
    shrink = shrink*1.5,errbars=errbars,col=newcol,labelcex=labelcex,
    lineht=lineht,showall=showall,barlabels=barlabels,showbrklab=showbrklab)
  start<-end
 }
 invisible(x)
}
