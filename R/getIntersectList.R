# getIntersectList
# Allows the user to manually enter the number of data objects with each
# combination of intersections of the nelem sets

getIntersectList<-function(nelem,xnames=NULL,sep="-") {
 if(is.null(xnames)) xnames<-LETTERS[1:nelem]
 xnamelen<-length(xnames)
 if(xnamelen < nelem) {
  extranames<-paste("extra",1:(nelem-xnamelen),sep=sep)
  cat("Not enough names in",xnames,"adding",extranames,"\n")
  xnames<-c(xnames,extranames)
 }
 intersectList<-vector("list",nelem+1)
 for(comb in 1:nelem) {
  nn<-choose(nelem,comb)
  intersectList[[comb]]<-rep(0,nn)
  currentnames<-names(intersectList[[comb]])<-
   pasteCols(combn(xnames,comb),sep=sep)
  for(intersect in 1:nn) {
   cat("Number of elements in",currentnames[intersect],"- ")
   intersectList[[comb]][intersect]<-scan(nmax=1,quiet=TRUE)
  }
 }
 cat("Total number of elements - ")
 intersectList[[nelem+1]]<-scan(nmax=1,quiet=TRUE)
 class(intersectList)<-"intersectList"
 return(intersectList)
}
