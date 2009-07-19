hierobrk<-function(formula,data,maxlevels=10,mct=mean,lmd=NULL,umd=lmd) {
 std.error<-function (x,na.rm) {
  vn<-function(x) return(sum(!is.na(x)))
  dimx<-dim(x)
  if(is.null(dimx)) {
   stderr<-sd(x,na.rm=TRUE)
   vnx<-vn(x)
  }
  else {
   if(is.data.frame(x)) {
    vnx<-unlist(sapply(x,vn))
    stderr<-unlist(sapply(x, sd, na.rm = TRUE))
   }
   else {
    vnx<-unlist(apply(x,2,vn))
    stderr<-unlist(apply(x,2,sd,na.rm=TRUE))
   }
  }
  return(stderr/sqrt(vnx))
 }
 bn<-as.character(attr(terms(formula),"variables")[-1])
 nbn<-length(bn)
# if(!is.numeric(data[[bn[1]]]))
#  stop("\nhierobrk: variable on left of formula must be numeric")
 findex<-which(names(data)%in%bn[2:nbn])
 mctlist<-by(data[[bn[1]]],data[,findex],mct,na.rm=TRUE)
 if(is.null(lmd)) {
  lcllist<-by(data[[bn[1]]],data[,findex],std.error,na.rm=TRUE)
  ucllist<-lcllist
 }
 else {
  lcllist<-by(data[[bn[1]]],data[,findex],lmd,na.rm=TRUE)
  ucllist<-by(data[[bn[1]]],data[,findex],umd,na.rm=TRUE)
 }
 return(list(mctlist,lcllist,ucllist))
}

hierobarp<-function(formula=NULL,data=NULL,maxlevels=10,
 mct=mean,lmd=std.error,umd=lmd, x=NULL,xlim=NULL,ylim=NULL,
 main="",xlab="",ylab="",start=0,end=1,shrink=0.02,errbars=FALSE,col=NA,
 labelcex=1,lineht=NA,showall=FALSE,showbrklab=FALSE,mar=NULL) {

 squeeze<-(end-start)*shrink
 if(is.null(x)) {
  x<-hierobrk(formula=formula,data=data,maxlevels=maxlevels,mct=mct,lmd=lmd,umd=umd)
  if(is.null(xlim)) xlim<-c(start,end)
  if(is.null(ylim)) {
   ymin<-min(c(0,unlist(x[[1]])-unlist(x[[2]])),na.rm=TRUE)
   if(ymin < 0) ymin<-ymin*1.02
   ymax<-max(unlist(x[[1]])+unlist(x[[3]]),na.rm=TRUE)
   ylim<-c(ymin,ymax*1.02)
   cat(ylim,"\n")
   if(!is.null(mar)) par(mar=mar)
  }
  plot(0,xlim=xlim,ylim=ylim,main=main,xlab=xlab,ylab=ylab,xaxt="n",
   xaxs="i",yaxs="i",type="n")
  parusr<-par("usr")
  if(is.na(lineht))
   lineht<-diff(parusr[3:4])*(par("mai")[1]/par("pin")[2])/par("mar")[1]
  firstcol<-if(is.list(col)) firstcol<-col[[1]]
  else firstcol<-col
  start<-start+squeeze
  end<-end-squeeze
  if(showall) rect(start,0,end,mean(x[[1]],na.rm=TRUE),col=firstcol)
  par(xpd=TRUE)
  segments(c(start,end),rep(ylim[1],2),c(start,end),
   rep(ylim[1],2)-lineht*(length(dim(x[[1]]))+1))
  mtext("Overall",side=1,line=length(dim(x[[1]])),at=(start+end)/2,cex=labelcex)
  par(xpd=FALSE)
  if(is.list(col)) {
   for(colindex in 1:(length(col)-1)) col[[colindex]]<-col[[colindex+1]]
   col[[length(col)]]<-NULL
  }
 }
 if(is.list(col)) barcol<-col[[1]]
 else barcol<-col
 mctdim<-dim(x[[1]])
 ndim<-length(mctdim)
 lastdim<-mctdim[ndim]
 if(is.null(mctdim)) {
  arrow.gap<-par("usr")[4]/100
  newwidth<-(end-start)/length(x[[1]])
  barnames<-names(x[[1]])
  for(lastbar in 1:length(x[[1]])) {
   end<-start+newwidth
   rect(start+squeeze,0,end-squeeze,x[[1]][lastbar],col=barcol[lastbar])
   if(errbars)
    dispersion((start+end)/2,x[[1]][lastbar],x[[2]][lastbar],x[[3]][lastbar],
     arrow.gap=arrow.gap)
   par(xpd=TRUE)
   segments(c(start+squeeze,end-squeeze),rep(ylim[1],2),
    c(start+squeeze,end-squeeze),rep(ylim[1],2)-lineht)
   mtext(barnames[lastbar],side=1,line=0.1,at=(start+end)/2,cex=labelcex)
   par(xpd=FALSE)
   start<-end
  }
 }
 else {
  newwidth<-(end-start)/lastdim
  if(length(barcol) < lastdim) barcol<-rep(barcol,length.out=lastdim)
  if(is.list(col)) {
   if(length(col) > 1) {
    newcol<-vector("list",length(col)-1)
    for(colindex in 2:length(col)) newcol[[colindex-1]]<-col[[colindex]]
   }
   else newcol<-col
  }
  else newcol<-col
  sliceargs<-vector("list",ndim+1)
  xslice<-vector("list",3)
  xdn<-dimnames(x[[1]])
  xdn<-xdn[[length(xdn)]]
  for(slice in 1:lastdim) {
   end<-start+newwidth
   for(stat in 1:3) {
    sliceargs[[1]]<-x[[stat]]
    for(arg in 2:ndim) sliceargs[[arg]]<-TRUE
    sliceargs[[ndim+1]]<-slice
#   print(sliceargs)
    xslice[[stat]]<-do.call('[',sliceargs)
   }
   if(showall)
    rect(start+squeeze,0,end-squeeze,mean(unlist(xslice[[1]]),na.rm=TRUE),
     col=barcol[slice])
   par(xpd=TRUE)
   segments(c(start+squeeze,end-squeeze),rep(ylim[1],2),
    c(start+squeeze,end-squeeze),rep(ylim[1],2)-lineht*ndim)
   mtext(xdn[slice],side=1,line=ndim-1,at=(start+end)/2,cex=labelcex)
   par(xpd=FALSE)
   hierobarp(x=xslice,xlim=xlim,ylim=ylim,main=main,xlab=xlab,ylab=ylab,
    start=start+squeeze,end=end-squeeze,shrink=shrink+0.015,errbars=errbars,
    col=newcol,lineht=lineht,showall=showall,showbrklab=showbrklab,labelcex=labelcex)
   start<-end
  }
 }
 invisible(x)
}
