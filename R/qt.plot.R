# qnt - a vector of positive numeric values
# qtime - a sequence of increasing positive numbers indicating the time
# dates are okay as long as they are numeric, not character representations
# non-integral values are rounded to the nearest integer

qt.plot<-function(qnt,qtime=NA,col=NULL,border="lightgray",
 main="Quantity x interval",xlab="Interval",ylab="Quantity",mar=c(5,4,4,4),...) {
 
 if(missing(qnt)) stop("Quantities must be supplied.")
 oldmar<-par("mar")
 par(mar=mar)
 # assume that quantities are sequential if not otherwise stated
 if(is.na(qtime[1])) qtime<-1:length(qnt)
 else {
  # get everything in increasing order of time
  qtorder<-order(qtime)
  qnt<-qnt[qtorder]
  qtime<-qtime[qtorder]
 }
 qint<-rep(0,length(qnt))
 minqnt<-round(min(qnt))
 maxqnt<-round(max(qnt))
 minint<-10000
 maxint<-0
 nqnt<-maxqnt-minqnt+1
 # have to start with a list as the maximum interval is yet to be calculated
 qtlist<-vector("list",nqnt)
 for(q in 1:nqnt) {
  qtlist[[q]]<-table(diff(qtime[qnt >= minqnt+q-1.5 & qnt < minqnt+q-0.5]))
  qint<-as.numeric(names(qtlist[[q]]))
  if(length(qtlist[[q]])) {
   thismin<-min(qint)
   thismax<-max(qint)
  }
  else {
   thismin<-10000
   thismax<-0
  }
  if(thismin < minint) minint<-thismin
  if(thismax > maxint) maxint<-thismax
 }
 plot(0,xlim=c(minint,maxint),ylim=c(minqnt-0.5,maxqnt+0.5),type="n",
  main=main,xlab=xlab,ylab=ylab,yaxt="n",...)
 unx<-unique(qnt)
 axis(2,unx,unx)
 nint<-maxint-minint+1
 qtmat<-matrix(0,nrow=nqnt,ncol=nint)
 # now drop the counts into the matrix
 for(q in 1:nqnt) {
  if(length(qtlist[[q]]))
   qtmat[q,as.numeric(names(qtlist[[q]]))]<-qtlist[[q]]
 }
 # scale the resulting counts so that they don't overlap
 maxcount<-max(qtmat)
 qfdiv<-2*maxcount
 if(is.null(col[1])) col<-color.scale(minqnt:maxqnt,c(0,1,1),c(1,1,0),0)
 if(length(col) < nqnt) col<-rep(col,length.out=nqnt)
 for(q in 1:nqnt) {
  polygon(c(minint:maxint,maxint:minint),
   c(minqnt+q+qtmat[q,]/qfdiv-1,minqnt+q-rev(qtmat[q,]/qfdiv)-1),
   col=col[q],border=border)
 }
 # put scale lines for a pretty quantity just larger than maxcount
 maxcount<-pretty(maxcount)[2]
 # put the scale on the right
 par(xpd=TRUE)
 xylim<-par("usr")
 x0<-rep(xylim[2]+(xylim[2]-xylim[1])/25,2)
 x1<-x0+(xylim[2]-xylim[1])/15
 y<-c((xylim[3]+xylim[4])/2-maxcount/qfdiv,
  (xylim[3]+xylim[4])/2+maxcount/qfdiv)
 segments(x0,y,x1,y)
 text((x0[1]+x1[1])/2,(y[1]+y[2])/2,maxcount)
 par(mar=oldmar,xpd=FALSE)
 invisible(qtmat)
}
