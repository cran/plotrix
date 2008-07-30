makeIntersectList<-function(x) {
 xnames<-names(x)
 if(!is.matrix(x)) x<-as.matrix(x)
 else xnames<-colnames(x)
 dimx<-dim(x)
 if(is.null(xnames)) xnames<-LETTERS[1:dimx[2]]
 intersectList<-vector("list",dimx[2] + 1)
 for(intersect in 1:dimx[2])
  intersectList[[1]][intersect]<-sum(rowSums(x) == 1 & x[,intersect])
 names(intersectList[[1]])<-xnames
 for(comb in 2:dimx[2]) {
  nn<-choose(dimx[2],comb)
  intersectList[[comb]]<-rep(0,nn)
  currentnames<-names(intersectList[[comb]])<-pasteCols(combn(xnames,comb))
  currentcombs<-combn(1:dimx[2],comb,simplify=TRUE)
  for(intersect in 1:nn) {
   combvec<-rep(0, dimx[2])
   combvec[currentcombs[,intersect]]<-1
   intersectList[[comb]][intersect]<-
    sum(colSums(apply(x,1,"==",combvec))==dimx[2])
  }
 }
 intersectList[[dimx[2] + 1]] <- dimx[1]
 names(intersectList[[dimx[2] + 1]]) <- "Total"
 return(intersectList)
}
 
