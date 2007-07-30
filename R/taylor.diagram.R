# Display a Taylor diagram
# Taylor K.E. (2001)
# Summarizing multiple aspects of model performance in a single diagram
# Journal of Geophysical Research, 106: 7183-7192.

# version 1.0
# progr. Olivier.Eterradossi, 12/2007
# 2007-01-12 - modifications and Anglicizing - Jim Lemon

taylor.diagram<-function(ref,model,add=FALSE,col="red",pch=19,pos.cor=TRUE,
 xlab="",ylab="",main="Taylor Diagram",show.gamma=TRUE,ref.sd=FALSE,
 pcex=1,normalize=FALSE,...) {
 
 grad.corr.full<-c(0,0.2,0.4,0.6,0.8,0.9,0.95,0.99,1)
 grad.corr.lines<-c(0.2,0.4,0.6,0.8,0.9)

 R<-cor(ref,model,use="pairwise")

 sd.r<-sd(ref)
 sd.f<-sd(model)
 if(normalize) {
  sd.f<-sd.f/sd.r
  sd.r<-1
 }
 maxsd<-1.5*max(sd.f,sd.r)
 oldpar<-par("mar","xpd")

 if(!add) {
  # display the diagram
  if(pos.cor) {
   if(nchar(ylab) == 0) ylab="Standard deviation"
   par(mar=c(5,4,6,6))
   plot(0,xlim=c(0,maxsd),ylim=c(0,maxsd),xaxs="i",yaxs="i",axes=FALSE,
    main=main,xlab=xlab,ylab=ylab,type="n",...)
   if(show.gamma) {
    gamma<-pretty(c(0,maxsd),n=3)[-1]
    if(gamma[length(gamma)] > maxsd) gamma<-gamma[-length(gamma)]
    labelpos<-seq(45,70,length.out=length(gamma))
    # do the gamma curves
    for(gindex in 1:length(gamma)) {
     xcurve<-cos(seq(0,pi,by=0.03))*gamma[gindex]+sd.r
     endcurve<-which(xcurve<0)
     endcurve<-ifelse(length(endcurve),min(endcurve)-1,105)
     ycurve<-sin(seq(0,pi,by=0.03))*gamma[gindex]
     maxcurve<-xcurve*xcurve+ycurve*ycurve
     startcurve<-which(maxcurve>maxsd*maxsd)
     startcurve<-ifelse(length(startcurve),max(startcurve)+1,0)
     lines(xcurve[startcurve:endcurve],ycurve[startcurve:endcurve],
      col="lightgray")
     boxed.labels(xcurve[labelpos[gindex]],ycurve[labelpos[gindex]],
      gamma[gindex],border=FALSE)
    }
   }
   axis(1,at=pretty(c(0,maxsd)))
   axis(2,at=pretty(c(0,maxsd)))
   # the outer curve for correlation
   xcurve<-cos(seq(0,pi/2,by=0.01))*maxsd
   ycurve<-sin(seq(0,pi/2,by=0.01))*maxsd
   lines(xcurve,ycurve)
   bigtickangles<-acos(seq(0.1,0.9,by=0.1))
   medtickangles<-acos(seq(0.05,0.95,by=0.1))
   smltickangles<-acos(seq(0.91,0.99,by=0.01))
   segments(cos(bigtickangles)*maxsd,sin(bigtickangles)*maxsd,
    cos(bigtickangles)*0.97*maxsd,sin(bigtickangles)*0.97*maxsd)
   par(xpd=TRUE)
   if(ref.sd) {
    # the inner curve for reference SD
    xcurve<-cos(seq(0,pi/2,by=0.01))*sd.r
    ycurve<-sin(seq(0,pi/2,by=0.01))*sd.r
    lines(xcurve,ycurve)
   }
   else points(sd.r,0)
   text(cos(c(bigtickangles,acos(c(0.95,0.99))))*1.05*maxsd,
    sin(c(bigtickangles,acos(c(0.95,0.99))))*1.05*maxsd,
    c(seq(0.1,0.9,by=0.1),0.95,0.99))
   text(maxsd*0.75,maxsd*0.8,"Correlation")
   segments(cos(medtickangles)*maxsd,sin(medtickangles)*maxsd,
    cos(medtickangles)*0.98*maxsd,sin(medtickangles)*0.98*maxsd)
   segments(cos(smltickangles)*maxsd,sin(smltickangles)*maxsd,
    cos(smltickangles)*0.99*maxsd,sin(smltickangles)*0.99*maxsd)
  }
  else {
   plot(c(-maxsd,maxsd),c(0,maxsd),type="n",asp=1,bty="n",xaxt="n",
    yaxt="n",xlab=xlab,ylab=ylab,main=main,...)
   discrete<-seq(180,0,by=-1)
   point.list<-NULL
   for(i in discrete)
    point.list<-cbind(point.list,maxsd*cos(i*pi/180),maxsd*sin(i*pi/180))
   point.list<-matrix(point.list,2,length(point.list)/2)
   point.list<-t(point.list)
   lines(point.list[,1],point.list[,2])

   # axes x,y
   lines(c(-maxsd,maxsd),c(0,0))
   lines(c(0,0),c(0,maxsd))

   # radial lines for correlation -0.8 -> 0.8
   for (i in grad.corr.lines){
    lines(c(0,maxsd*i),c(0,maxsd*sqrt(1-i^2)),lty=3)
    lines(c(0,-maxsd*i),c(0,maxsd*sqrt(1-i^2)),lty=3)
   }

   # labels for radial lines
   for (i in grad.corr.full){
    text(1.05*maxsd*i,1.05*maxsd*sqrt(1-i^2),i,cex=0.6)
    text(-1.05*maxsd*i,1.05*maxsd*sqrt(1-i^2),-i,cex=0.6)
   }

   # concentric lines about the reference standard deviation

   seq.sd<-seq.int(0,2*maxsd,by=(maxsd/10))
   for (i in seq.sd){
    xcircle<-sd.r+(cos(discrete*pi/180)*i)
    ycircle<-sin(discrete*pi/180)*i
    for (j in 1:length(xcircle)){
     if((xcircle[j]^2+ycircle[j]^2)<(maxsd^2)) {
      points(xcircle[j],ycircle[j],col="darkgreen",pch=".")
      if(j==10)
       text(xcircle[j],ycircle[j],signif(i,2),cex=0.5,col="darkgreen")
     }
    }
   }

   # concentric lines about the origin

   seq.sd<-seq.int(0,maxsd,length.out=5)
   for(i in seq.sd) {
    xcircle<-(cos(discrete*pi/180)*i)
    ycircle<-sin(discrete*pi/180)*i
    lines(xcircle,ycircle,lty=3,col="blue")
    text(min(xcircle),-0.03*maxsd,signif(i,2),cex=0.5,col="blue")
    text(max(xcircle),-0.03*maxsd,signif(i,2),cex=0.5,col="blue")
   }

   text(0,-0.08*maxsd,"Standard Deviation",cex=0.7,col="blue")
   text(0,-0.12*maxsd,"Centered RMS Difference",cex=0.7,col="darkgreen")
   points(sd.r,0,pch=22,bg="darkgreen",cex=1.1)

   text(0,1.1*maxsd,"Correlation Coefficient",cex=0.7)
  }
 }
 # display the points
 points(sd.f*R,sd.f*sin(acos(R)),pch=pch,col=col,cex=pcex)
 invisible(oldpar)
}
