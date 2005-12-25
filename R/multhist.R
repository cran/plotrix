multhist<-function(x,breaks="Sturges",...) {
 allhist<-hist(unlist(x),breaks=breaks,plot=FALSE)
 combhist<-
  t(sapply(x,function(z) hist(z,breaks=allhist$breaks,plot=FALSE)$counts))
 barplot(combhist,beside=TRUE,names=signif(allhist$mids,2),...)
}
