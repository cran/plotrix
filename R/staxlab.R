# staxlab produces staggered axis tick labels
# note that barplot() tends to mess things up by plotting an X axis 
# even when axes=FALSE

staxlab<-function(side=1,at,labels,nlines=2,top.line=0.5,
 line.spacing=0.8,srt=NA,ticklen=0.05,...) {

 if(missing(labels)) labels<-at
 nlabels<-length(labels)
 if(missing(at)) at<-1:nlabels
 if(is.na(srt)) {
  linepos<-rep(top.line,nlines)
  for(i in 2:nlines) linepos[i]<-linepos[i-1]+line.spacing
  linepos<-rep(linepos,ceiling(nlabels/nlines))[1:nlabels]
  axis(side=side,at=at,labels=rep("",nlabels))
  mtext(text=labels,side=side,line=linepos,at=at,...)
 }
 else {
  xylim<-par("usr")
  ypos<-xylim[3]-ticklen*(xylim[4]-xylim[3])
  par(xpd=TRUE)
  text(at,ypos,labels,srt=srt,adj=1,...)
  par(xpd=FALSE)
 }
}
