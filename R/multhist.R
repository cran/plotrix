multhist<-function(x,...) {
 allhist<-hist(unlist(x),plot=FALSE)
 combhist<-
  t(sapply(x,function(z) hist(z,breaks=allhist$breaks,plot=FALSE)$counts))
 barplot(combhist,beside=TRUE,names=signif(allhist$mids,2),...)
}
