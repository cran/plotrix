plot.freq<-function(x,main=NULL,col=NULL,show.prop=TRUE,show.pc=FALSE,
 show.labels=FALSE,show.axis=FALSE,...) {
 if(class(x) != "freq")
  stop("Usage: plot.freq(x,...) where x is a \"freq\" object") 
 oldmar<-par("mar")
 par(mar=c(2,4,4,1))
 nbars<-length(x)
 maxn<-max(unlist(lapply(x,sum)))
 maxsegments<-max(unlist(lapply(x,length)))
 varnames<-names(x)
 if(is.null(main))
  main<-paste("Frequencies of",deparse(substitute(x)),collapse="")
 plot(c(0,maxn),c(0.5,nbars+0.5),xlab="",ylab="",type="n",axes=FALSE,
  main=main,...)
 labelspace<-maxn/30
 # set the default colors if none given
 if(is.null(col)) col<-1:maxsegments+1
 for(i in 1:nbars) {
  bottom<-nbars-i+0.6
  top<-bottom+0.8
  left<-0
  par(xpd=TRUE)
  text(-labelspace,nbars+1-i,varnames[i],adj=1)
  par(xpd=FALSE)
  nsegments<-length(x[[i]])
  xpos<-ypos<-rep(0,nsegments)
  for(j in 1:nsegments) {
   right<-left+x[[i]][j]
   rect(left,bottom,right,top,col=col[j],border="black")
   xpos[j]<-(left+right)/2
   ypos[j]<-nbars+1-i
   left<-right
  }
  if(show.prop) {
   props<-round(x[[i]]/sum(x[[i]]),2)
   if(show.pc) props<-paste(props*100,"%",sep="")
   if(show.labels) props<-paste(names(x[[i]]),"\n",props,sep="")
   boxed.labels(xpos,ypos,props)
  }
 }
 if(show.axis) axis(1,pos=0.4)
 par(mar=oldmar)
}
