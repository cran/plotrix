legend.outside<-function(side=1,x=NULL,y=NULL,look.for="plot",...) {
 # get the current margins
 oldmar<-par("mar")
 xyval<-par("usr")
 # call legend without plotting to get the size
 legend.info<-legend(x=xyval[1],y=xyval[4],...,plot=FALSE)
 # get the proportion of the plot taken up by the legend width
 legend.xprop<-legend.info$rect$w/(xyval[2]-xyval[1])
 # get the proportion of the plot taken up by the legend height
 legend.yprop<-legend.info$rect$h/(xyval[4]-xyval[3])
 newmar<-oldmar
 if(side == 1) {
  if(legend.yprop > 0.1)
   newmar[1]<-newmar[1]+floor((legend.yprop - 0.08)/0.015)
  # if no x position is given, right align to axis 4
  if(is.null(x)) x<-xyval[2]-legend.info$rect$w
  # if no y position, put it 1/10 plot span beneath
  if(is.null(y)) {
   y10<-(xyval[4]-xyval[3])/10
   y<-xyval[3]-y10
  }
  cat("x =",x," y =",y,"\n")
 }
 if(side == 2) {
  if(legend.xprop > 0.1)
   newmar[2]<-newmar[2]+floor((legend.xprop-0.08)/0.015)
  # if no x position is given, put it 1/8 plot span left
  if(is.null(x)) {
   x8<-(xyval[2]-xyval[1])/8
   x<-xyval[1]-(x8+legend.info$rect$w)
  }
  # if no y position, put it 3/8 plot span up
  if(is.null(y)) y<-xyval[3]+3*(xyval[4]-xyval[3])/8
 }
 if(side == 3) {
   if(legend.yprop > 0.1)
    newmar[3]<-newmar[3]+floor((legend.yprop - 0.08)/0.015)
  # if no x position is given, put it 3/4 along the axis
  if(is.null(x)) x<-3*sum(xyval[1:2])/4
  # if no y position, put it 1/20 plot span above
  if(is.null(y)) {
   y20<-(xyval[4]-xyval[3])/20
   y<-xyval[4]+y20+legend.info$rect$h
  }
 }
 if(side == 4) {
  if(legend.xprop > 0.05)
   newmar[4]<-newmar[4]+floor((legend.xprop-0.05)/0.02)
  # if no x position is given, put it 1/20 plot span right
  if(is.null(x)) {
   x20<-(xyval[2]-xyval[1])/20
   x<-xyval[2]+x20
  }
  # if no y position, put it halfway up
  if(is.null(y)) y<-xyval[3]+(xyval[4]-xyval[3])/2
 }
 par(mar=newmar)
 last.plot.calls<-last.call(look.for=look.for)
 if(!is.null(last.plot.calls))
  for(p in 1:(length(last.plot.calls)-1)) eval(parse(text=last.plot.calls[p]))
 if(!is.null(last.plot.calls)) {
  par(xpd=TRUE)
  legend.info<-legend(x,y,...)
  par(xpd=FALSE)
 }
 par(mar=oldmar)
 invisible(legend.info)
}
