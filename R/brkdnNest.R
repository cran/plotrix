brkdnNest<-function(formula,data,maxlevels=10,mct=mean,lmd=std.error,umd=lmd,
 trueval=NA) {

 if(missing(data) || missing(formula))
  stop("brkdnNest must be called with a formula for breakdown and a data frame.")
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
 mctlist<-list()
 if(is.na(trueval)) {
  mctlist[[1]]<-do.call(mct,list(data[bn[1]],na.rm=TRUE))
  for(brklev in 2:nbn)
   mctlist[[brklev]]<-by(data[bn[1]],data[bn[2:brklev]],mct,na.rm=TRUE,
    simplify=FALSE)
 }
 else {
  mctlist[[1]]<-do.call(propbrk,list(data[bn[1]],trueval=trueval))
  for(brklev in 2:nbn)
   mctlist[[brklev]]<-by(data[bn[1]],data[bn[2:brklev]],propbrk,trueval=trueval,
    simplify=FALSE)
 }
 barlabels<-list(bn[1])
 for(brklev in 2:nbn) barlabels[[brklev]]<-dimnames(mctlist[[nbn]])[[brklev-1]]
 names(barlabels)<-c("Overall",names(dimnames(mctlist[[nbn]])))
 lmdlist<-list()
 if(!is.null(lmd)) {
  lmdlist[[1]]<-do.call(lmd,list(data[[bn[1]]],na.rm=TRUE))
  for(brklev in 2:nbn)
   lmdlist[[brklev]]<-by(data[[bn[1]]],data[bn[2:brklev]],lmd,na.rm=TRUE,
    simplify=FALSE)
 }
 umdlist<-list()
 if(!is.null(umd)) {
  umdlist[[1]]<-do.call(umd,list(data[[bn[1]]],na.rm=TRUE))
  for(brklev in 2:nbn)
   umdlist[[brklev]]<-by(data[[bn[1]]],data[bn[2:brklev]],umd,na.rm=TRUE,
    simplify=FALSE)
 }
 return(list(mctlist,lmdlist,umdlist,barlabels))
}
