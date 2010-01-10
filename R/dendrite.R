makeDendrite<-function(x) {
 dimx<-dim(x)
 if (is.null(dimx)) {
  dend.tab<-table(as.character(x),useNA="ifany")
  tablen<-length(dend.tab)
  dendrite<-vector("list",tablen)
  for(i in 1:tablen) dendrite[[i]]<-list(dend.tab[i],NULL)
 }
 else {
  dend.tab<- table(as.character(x[, 1]),useNA="ifany")
  tablen<-length(dend.tab)
  tabname<-names(dend.tab)
  dendrite<-vector("list",tablen)
  for(i in 1:tablen) {
   if(is.na(tabname[i]))
    nextx<-x[is.na(x[,1]),2:dimx[2]]
   else
    nextx<-x[as.character(x[,1])==tabname[i]&!is.na(x[,1]),2:dimx[2]]
   dendrite[[i]] <- list(dend.tab[i], makeDendrite(nextx))
  }
 }
 class(dendrite) <- "dendrite"
 return(dendrite)
}

sumDendrite<-function(x) {
 dsum<-0
 for(i in 1:length(x)) dsum<-dsum+x[[i]][[1]]
 return(dsum)
}

furc<-function(x,xpos,yrange,toplevel,cex=1) {
 xlen<-length(x)
 if(xlen) {
  yinc<-diff(yrange)/xlen
  ypos<-seq(yrange[1] + yinc/2, yrange[2] - yinc/2,length.out=xlen)
  if(!toplevel) {
   segments(xpos-0.5,ypos[1],xpos-0.5,ypos[length(ypos)])
   segments(xpos-0.5,ypos,xpos,ypos)
  }
  for(i in 1:xlen) {
   if(is.list(x[[i]][[2]])) {
    segments(xpos,ypos[i],xpos+0.5,ypos[i])
    furc(x[[i]][[2]],xpos+1,c(ypos[i]-yinc/2, 
     ypos[i]+yinc/2),FALSE,cex=cex)
   }
   boxed.labels(xpos,ypos[i],paste(names(x[[i]][[1]]), 
    x[[i]][[1]]),cex=cex)
  }
 }
}

listDepth<-function(x) {
 if(is.list(x)) {
  maxdepth<-1
  for(lindex in 1:length(x)) {
   newdepth<-listDepth(x[[lindex]])+1
   if(newdepth > maxdepth) maxdepth<-newdepth
  }
 }
 else maxdepth<-0
 return(maxdepth)
}

plot.dendrite<-function(x,xlabels=NULL,main="",mar=c(1,0,3,0),cex=1,...) {

 oldmar<-par("mar")
 par(mar=mar)
 xmax<-listDepth(x)/2
 ymax<-sumDendrite(x)
 plot(0,main=main,xlim=c(0,xmax),ylim=c(1,ymax), 
  xlab="",ylab="",type="n",axes=FALSE,...)
 par(xpd=TRUE)
 text(seq(0.5,xmax),par("usr")[3],xlabels)
 par(xpd=FALSE)
 furc(x,0.5,c(1,ymax),TRUE,cex=cex)
 par(mar=oldmar)
}

