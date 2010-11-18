valid.n<-function(x,na.rm=TRUE) return(ifelse(na.rm,sum(!is.na(x)),length(x)))

propbrk<-function(x,trueval=TRUE,na.rm=TRUE) {
 return(sum(x==trueval,na.rm=TRUE)/length(x))
}

brkdnNest<-function(formula,data,FUN=c("mean","sd"),label1="Overall",trueval=NA) {
 if(missing(data) || missing(formula))
  stop("brkdnNest must be called with a formula for breakdown and a data frame.")
 bn<-as.character(attr(terms(formula),"variables")[-1])
 nbn<-length(bn)
 nFUN<-length(FUN)
 brklist<-vector("list",nFUN)
 for(brkfun in 1:nFUN) {
  brklist[[brkfun]]<-vector("list",nbn)
  if(is.na(trueval))
   brklist[[brkfun]][[1]]<-data.frame(bn[1],
    do.call(FUN[brkfun],list(data[[bn[1]]],na.rm=TRUE)))
  else
   brklist[[brkfun]][[1]]<-data.frame(bn[1],
    do.call(FUN[brkfun],list(data[[bn[1]]],trueval=trueval,na.rm=TRUE)))
  names(brklist[[brkfun]][[1]])<-c(label1,FUN[brkfun])
  for(brk in 2:nbn) {
   if(is.na(trueval))
    brklist[[brkfun]][[brk]]<-
     aggregate(data[[bn[1]]],data[bn[2:brk]],FUN=FUN[brkfun],na.rm=TRUE)
   else
    brklist[[brkfun]][[brk]]<-
     aggregate(data[[bn[1]]],data[bn[2:brk]],FUN=FUN[brkfun],trueval=trueval,na.rm=TRUE)
   names(brklist[[brkfun]][[brk]])<-c(bn[2:brk],FUN[brkfun])  
  }
 }
 attr(brklist,"class")<-"brklist"
 return(brklist)
}

print.brklist<-function(x,...) {
 printbrk<-function(x,brklabel) {
  dimx<-dim(x)
  if(dimx[1]>1) cat(brklabel,"by\n")
  print(x)
 }
 printFUN<-function(x) {
  cat("\n")
  brklabel<-names(x[[1]][1])
  lapply(x,printbrk,brklabel)
 }
 lapply(x,printFUN)
}
