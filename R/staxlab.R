# staxlab produces staggered axis tick labels
# note that barplot() tends to mess things up by plotting an X axis 
# even when axes=F

staxlab<-function(side=1,at,labels,nlines=2) {
 if(missing(labels)) stop("Usage: staxlab(side=1,at,labels,nlines=2)")
 nlabels<-length(labels)
 if(missing(at)) at<-1:nlabels
 linepos<-rep(1:nlines,ceiling(nlabels/nlines))[1:nlabels]
 axis(side=side,at=at,labels=rep("",nlabels))
 mtext(text=labels,side=side,line=linepos,at=at)
}

