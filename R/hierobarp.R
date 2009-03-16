brkdnCrawler<-function (x,errbars=FALSE,
 retval=list(indx=vector("numeric",0),element=NULL,value=NA,depth=0)) {

 if(is.list(x)) {
  thisindx<-retindx<-thisdepth<-1
  retval$depth<-retval$depth+1
  for(lindex in 1:length(x)) {
   newretval<-brkdnCrawler(x[[lindex]],errbars=errbars,retval=retval)
   if(is.na(retval$value[1])) {
    retval$element<-newretval$element
    retval$value<-newretval$value
   }
   if(!is.na(newretval$value[1]) && newretval$value[1] > retval$value[1]) {
    retval$value<-newretval$value
    retval$element<-newretval$element
    retindx<-newretval$indx
    thisindx<-lindex
   }
   if(thisdepth < newretval$depth) thisdepth<-newretval$depth
  }
  if(retval$depth < thisdepth) retval$depth<-thisdepth
  retval$indx <- c(thisindx, retindx)
  return(retval)
 }
 else {
  retval$value<-ifelse(is.null(dim(x)),0,
   ifelse(errbars,max(x[1,]+x[2,]),max(x[1,])))
  retval$element<-x
 }
 return(retval)
}

hierobrk<-function(formula,data,maxlevels=10,
 num.desc=c("mean","std.error","valid.n")) {

 descnum<-function (x,num.desc=c("mean","std.error","valid.n")) {
  desclen<-length(num.desc)
  desc.vector<-rep(NA,desclen)
  for(i in 1:desclen)
   if(valid.n(x)) desc.vector[i]<-do.call(num.desc[i],list(x,na.rm=TRUE))
   else desc.vector[i]<-NA
  return(desc.vector)
 }

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

 valid.n<-function(x,na.rm) return(sum(!is.na(x)))

 if(!missing(data) && !missing(formula)) {
  bn<-as.character(attr(terms(formula),"variables")[-1])
  nbn<-length(bn)
  if(!is.numeric(data[[bn[1]]]))
   stop("\nbrkdn.num: variable on left of formula must be numeric")
  if(nbn > 2) {
   by.factor<-as.factor(data[[bn[nbn]]])
   factor.levels<-levels(by.factor)
   factor.labels<-attr(data[,bn[nbn]],"value.labels")
   if(is.null(factor.labels)) factor.labels<-factor.levels
   else 
    if(!is.null(names(factor.labels))) factor.labels<-names(factor.labels)
   nlevels<-length(factor.levels)
   if(nlevels > maxlevels) {
    nlevels<-maxlevels
    cat("brkdn.num: too many levels - only using first",maxlevels,"\n")
   }
   brkstats<-vector("list",nlevels+1)
   names(brkstats)<-c("Overall",factor.levels[1:nlevels])
   brkstats$Overall<-descnum(data[[bn[1]]],num.desc=num.desc)
   for(i in 1:nlevels) {
    currentdata<-subset(data,by.factor==factor.levels[i])
    brkstats[[i+1]]$Overall<-descnum(currentdata[[bn[1]]],num.desc=num.desc)
    for(j in 1:dim(currentdata)[2])
     attr(currentdata[,j],"value.labels")<-attr(data[,i],"value.labels")
    next.formula<-
     as.formula(paste(paste(bn[1],"~"),paste(bn[2:(nbn-1)],collapse="+")))
    brkstats[[i+1]]<-
     hierobrk(next.formula,currentdata,maxlevels=maxlevels,num.desc=num.desc)
   }
   return(brkstats)
  }
  else {
   by.factor<-as.factor(data[[bn[2]]])
   factor.levels<-levels(by.factor)
   factor.labels<-attr(data[,bn[2]],"value.labels")
   if(is.null(names(factor.labels))) factor.labels<-factor.levels
   else factor.labels<-names(factor.labels)
   nlevels<-length(factor.levels)
   if(nlevels > maxlevels){
    nlevels<-maxlevels
    cat("brkdn.num: Too many levels - only using first",maxlevels,"\n")
   }
   xstats<-vector("list",2)
   xstats[[1]]<-descnum(data[[bn[1]]],num.desc=num.desc)
   xstats[[2]]<-matrix(NA,ncol=nlevels,nrow=length(num.desc))
   colnames(xstats[[2]])<-factor.labels[1:nlevels]
   rownames(xstats[[2]])<-num.desc
   if(is.numeric(data[[bn[1]]])) {
    for(i in 1:nlevels) {
     currentdata<-subset(data[[bn[1]]],by.factor==factor.levels[i])
     if(length(currentdata)){
      xstats[[2]][,i]<-descnum(currentdata,num.desc=num.desc)
     }
    }
    rnames<-rownames(xstats)
   }
   names(xstats)<-c("Overall","Breakdown")
   return(xstats)
  }
 }
}

hierobarp<-function(formula,data,maxlevels=10,num.desc=c("mean","std.error","valid.n"),
 x=NULL,xinfo=NULL,space=0.1,xlim=NULL,ylim=NULL,main="",xlab="",ylab="",
 errbars=FALSE,col=NA,lineht=NA,showall=FALSE,showbrklab=FALSE,firstcall=TRUE) {

 if(firstcall) {
  x<-hierobrk(formula=formula,data=data,maxlevels=maxlevels,num.desc=num.desc)
  xinfo<-brkdnCrawler(x,errbars)
 }
 else {
  newxinfo<-brkdnCrawler(x)
  xinfo$depth<-newxinfo$depth
 }
 lenx<-length(x)
 if(is.null(xlim)) xlim<-c(0.5,lenx-0.5)
 if(is.null(ylim)) ylim<-c(0,xinfo$value*1.05)
 if(firstcall) {
  plot(0,xlim=xlim,ylim=ylim,main=main,xlab=xlab,ylab=ylab,xaxt="n",
   yaxs="i",type="n")
  parusr<-par("usr")
  if(is.na(lineht))
   lineht<-diff(parusr[3:4])*(par("mai")[1]/par("pin")[2])/par("mar")[1]
  xlim<-c(xlim[1]+space,xlim[2]-space)
  # display the overall value
  if(showall) {
   rect(xlim[1],0,xlim[2],x$Overall[1],col=col[[1]])
   newcol<-list()
   for(colind in 2:length(col)) newcol[[colind-1]]<-col[[colind]]
   col<-newcol
   par(xpd=TRUE)
   segments(c(xlim[1],xlim[2]),rep(0,2),
    c(xlim[1],xlim[2]),rep(0,2)-lineht*(xinfo$depth+1))
   par(xpd=FALSE)
   mtext(names(x)[1],side=1,line=xinfo$depth,at=mean(xlim))
  }
 }
 # if the second component of x is named "Breakdown", this is the final level
 if(match("Breakdown",names(x)[2],0)) {
  nbars<-dim(x$Breakdown)[2]
  if(length(col) < nbars) col<-rep(col,length.out=nbars)
  start<-xlim[1]+diff(xlim)*space
  barinc<-(diff(xlim)*(1-space*2))/nbars
  barnames<-colnames(x$Breakdown)
  # pull the vector of colors out of a list
  if(is.list(col)) col<-col[[1]]
  for(lev in 1:nbars) {
   rect(start,0,start+barinc,x$Breakdown[1,lev],col=col[lev])
   if(errbars) {
    # if there are more than three rows in the breakdown,
    # assume 2 and 3 are upper and lower limits respectively
    if(dim(x$Breakdown)[1] > 3)
     dispersion(start+barinc/2,x$Breakdown[1,lev],x$Breakdown[2,lev],
      x$Breakdown[3,lev],arrow.cap=barinc/(2.5*diff(par("usr")[1:2])))
    else
     dispersion(start+barinc/2,x$Breakdown[1,lev],x$Breakdown[2,lev],
      arrow.cap=barinc/(2.5*diff(par("usr")[1:2])))
   }
   if(showbrklab)
    mtext(barnames[lev],side=1,line=0,at=start+barinc/2)
   start<-start+barinc
  }
 }
 else {
  nbars<-lenx-1
  barspace<-diff(xlim)/nbars
  start<-xlim[1]
  barnames<-names(x)
  for(lev in 1:nbars) {
   barstart<-start+barspace*space
   barend<-barstart+barspace*(1-2*space)
   if(showall) {
    rect(barstart,0,barend,x[[lev+1]]$Overall[1],col=col[[1]][lev])
    nextcol<-list()
    for(colind in 2:length(col)) nextcol[[colind-1]]<-col[[colind]]
   }
   else nextcol<-col
   par(xpd=TRUE)
   segments(c(barstart,barend),rep(0,2),
    c(barstart,barend),rep(0,2)-lineht*(xinfo$depth))
   mtext(barnames[lev+1],side=1,line=xinfo$depth-1,at=(barstart+barend)/2)
   par(xpd=FALSE)
   hierobarp(x=x[[lev+1]],xinfo=xinfo,space=space,xlim=c(barstart,barend),
    ylim=ylim,errbars=errbars,col=nextcol,lineht=lineht,showall=showall,
    showbrklab=showbrklab,firstcall=FALSE)
   start<-start+barspace
  }
 }
}
