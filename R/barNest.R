hierobrk<-function(formula,data,maxlevels=10,mct=mean,lmd=NULL,umd=lmd,trueval=NA) {
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
    stderr<-unlist(apply(x,2,sd,na.rm = TRUE))
   }
  }
  return(stderr/sqrt(vnx))
 }
 propbrk<-function(x,trueval=TRUE) {
  proptab<-table(x)
  prop<-proptab[which(names(proptab)==trueval)]/sum(proptab)
  return(prop)
 }
 bn<-as.character(attr(terms(formula),"variables")[-1])
 nbn<-length(bn)
 # this gets the order of factors for breakdown right
 findex<-NULL
 barlabels<-vector("list",nbn-1)
 for(varname in 2:nbn) {
  thisindex<-which(names(data) %in% bn[varname])
  findex <- c(findex,thisindex)
  # and this hopefully gets the order of labels right
  if(match(class(data[[thisindex]]),"factor",0))
   barlabels[[varname-1]]<-levels(data[[thisindex]])
  else barlabels[[varname-1]]<-sort(unique(data[[thisindex]]))
 }
 if(is.na(trueval))
  mctlist<-by(data[[bn[1]]],data[,findex],mct,na.rm=TRUE)
 else
  mctlist<-by(data[[bn[1]]],data[,findex],propbrk,trueval=trueval)
 if(is.null(lmd)) {
  lcllist<-by(data[[bn[1]]],data[,findex],std.error,na.rm=TRUE)
  ucllist<-lcllist
 }
 else {
  lcllist<-by(data[[bn[1]]],data[,findex],lmd,na.rm=TRUE)
  ucllist<-by(data[[bn[1]]],data[,findex],umd,na.rm=TRUE)
 }
 return(list(mctlist,lcllist,ucllist,barlabels))
}

barNest<-function(formula=NULL,data=NULL,maxlevels=10,
 mct=mean,lmd=std.error,umd=lmd, x=NULL,xlim=NULL,ylim=NULL,
 main="",xlab="",ylab="",start=0,end=1,shrink=0.02,errbars=FALSE,
 col=NA,labelcex=1,lineht=NA,showall=FALSE,barlabels=NULL,
 showbrklab=TRUE,mar=NULL,arrow.cap=0.01,trueval=NA) {

 squeeze<-(end-start)*shrink
 if(is.null(x)) {
  x<-hierobrk(formula=formula,data=data,maxlevels=maxlevels,mct=mct,lmd=lmd,umd=umd,
   trueval=trueval)
  if(!is.na(trueval)) errbars=FALSE
  if(is.null(xlim)) xlim<-c(start,end)
  if(is.null(ylim)) {
   ymin<-min(c(0,unlist(x[[1]])-errbars*unlist(x[[2]])),na.rm=TRUE)
   if(ymin < 0) ymin<-ymin*1.02
   ymax<-max(c(max(unlist(x[[1]]),na.rm=TRUE),
    max(unlist(x[[1]])+errbars*unlist(x[[3]]),na.rm=TRUE)))
   ylim<-c(ymin,ymax*1.02)
   if(!is.null(mar)) par(mar=mar)
  }
  if(is.null(barlabels) && length(x) > 3) barlabels<-x[[4]]
 }
 else if(!is.list(x)) x<-list(x)
 mctdim<-dim(x[[1]])
 ndim<-length(mctdim)
 if(start == 0) {
  plot(0,xlim=xlim,ylim=ylim,main=main,xlab=xlab,ylab=ylab,xaxt="n",
   xaxs="i",yaxs="i",type="n")
  parusr<-par("usr")
  if(is.na(lineht))
   lineht<-diff(parusr[3:4])*(par("mai")[1]/par("pin")[2])/par("mar")[1]
  firstcol<-if(is.list(col)) barcol<-col[[1]]
  else barcol<-col
  start<-start+squeeze
  end<-end-squeeze
  if(showall) {
   rect(start,0,end,mean(x[[1]],na.rm=TRUE),col=firstcol)
   par(xpd=TRUE)
   segments(c(start,end,start),
    c(rep(ylim[1],2),ylim[1]-lineht*(ndim+0.5)),
    c(start,end,end),rep(ylim[1],3)-lineht*(ndim+0.5))
   boxed.labels((start+end)/2,ylim[1]-lineht*(ndim+0.5),"Overall",
    bg=ifelse(is.na(barcol),"white",barcol),cex=labelcex)
   par(xpd=FALSE)
  }
  if(is.list(col)) {
   for(colindex in 1:(length(col)-1)) col[[colindex]]<-col[[colindex+1]]
   col[[length(col)]]<-NULL
  }
 }
 if(is.list(col)) barcol<-col[[1]]
 else barcol<-col
 lastdim<-mctdim[ndim]
 if(is.null(mctdim)) {
  if(is.na(arrow.cap)) arrow.cap<-par("usr")[4]/100
  newwidth<-(end-start)/length(x[[1]])
  barnames<-barlabels[[1]]
  for(lastbar in 1:length(x[[1]])) {
   end<-start+newwidth
   rect(start+squeeze,0,end-squeeze,x[[1]][lastbar],col=barcol[lastbar])
   if(errbars)
    dispersion((start+end)/2,x[[1]][lastbar],x[[3]][lastbar],x[[2]][lastbar],
     arrow.cap=arrow.cap)
   if(showbrklab) {
    par(xpd=TRUE)
    boxed.labels((start+end)/2,ylim[1]-lineht*0.5,barnames[lastbar],
     bg=ifelse(is.na(barcol),"white",barcol[lastbar]),cex=labelcex)
    par(xpd=FALSE)
   }
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
  xdn <- barlabels[[length(barlabels)]]
  newlabels<-barlabels
  if(length(barlabels) > 1) newlabels[[length(barlabels)]]<-NULL
  for(slice in 1:lastdim) {
   end<-start+newwidth
   for(stat in 1:3) {
    sliceargs[[1]]<-x[[stat]]
    for(arg in 2:ndim) sliceargs[[arg]]<-TRUE
    sliceargs[[ndim+1]]<-slice
    xslice[[stat]]<-do.call('[',sliceargs)
   }
   if(showall) {
    rect(start+squeeze,0,end-squeeze,mean(unlist(xslice[[1]]),na.rm=TRUE),
     col=barcol[slice])
    par(xpd=TRUE)
    segments(c(start+squeeze,end-squeeze,start+squeeze),
     c(rep(ylim[1],2),ylim[1]-lineht*(ndim-0.5)),
     c(start+squeeze,end-squeeze,end-squeeze),
     rep(ylim[1],3)-lineht*(ndim-0.5))
    boxed.labels((start+end)/2,ylim[1]-lineht*(ndim-0.5),xdn[slice],
     bg=ifelse(is.na(barcol[slice]),"white",barcol[slice]),cex=labelcex)
    par(xpd=FALSE)
   }
   barNest(x=xslice,xlim=xlim,ylim=ylim,main=main,xlab=xlab,ylab=ylab,
    start=start+squeeze,end=end-squeeze,shrink=shrink*1.5,errbars=errbars,
    col=newcol,barlabels=newlabels,lineht=lineht,showall=showall,
    showbrklab=showbrklab,labelcex=labelcex)
   start<-end
  }
 }
 invisible(x)
}
