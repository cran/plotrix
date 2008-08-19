# CLPLOT: to plot a curve w/ different colors at dif't levels,
# something Excel can't do *_*
# written by Carl Witthoft, carl@witthoft.com 
#  Parameters:
#	levels--	vector of desired cutpoints.
#	cols--	vector of desired color sequence (strings or numeric references)
#	x, y--		The data, of  course.  
#	'...' 		is intended for arguments to pass to PLOT or LINES calls
#	showcuts--	Set to TRUE to plot gridlines at color cut levels
#
# Note warning message if levels[j] is outside range(y), but won't terminate or
# modify levels
clplot<-function(x,y, ylab=deparse(substitute(y)), xlab=deparse(substitute(x)),
levels=seq(min(y)+(max(y)-min(y))/5, max(y)-(max(y)-min(y))/5, length.out=4),
cols=c("black","blue","green","orange","red"),showcuts=FALSE,...) {

#I create x and y if only one is submitted, because they're needed in the 
#pre-plotting calculations
if(missing(y)){
		ylab=deparse(substitute(x))
		y<-as.numeric(x)
		x<-seq(1,length(y))
		}
xx<-as.numeric(x)
yy<-as.numeric(y)
levels<-as.numeric(levels)
if (levels[1]<min(yy)|levels[length(levels)]>max(yy)) {
	cat('Warning: levels value(s) outside data range\n')
	}
# build color repeat if not enough submitted
if (length(levels)>=length(cols)) {
	cat("Warning: not enough colors. Will repeat.\n")
	cols<-rep(cols,length.out=length(levels)+1)
	}
# add "top" and "bottom" cut values to simplify loop; sort to
# cover badly ordered input level values
cuts<-sort(c(min(yy),levels,max(yy)))
#build 'empty' graph  (may need a Suppress for lines() args)
plot(xx,yy,type='n',xlab=xlab, ylab=ylab, ...)
#build array for sliced data; simplifies things to start at zero
slice<-array(data=0,dim=c(length(x),length(levels)+1))
#for output purposes, need to make slicex an array too
slicex<-array(dim=c(length(x),length(levels)+1))
for(j in 1 : (length(cuts)-1))	
	{
	#get cut of data desired 
	#  keep an intermediate "slicelog" don't need to redo the logicals
	# when filling in the interpolation points later on
	slicelog<-as.logical(cuts[j]<=yy & yy<=cuts[j+1])
	is.na(slice[,j])<-!slicelog
	slice[,j]<-slice[,j]+yy
	#more error catching: if no data in this slice, want to skip. 	# now that slice has NAs, need to tag the sum
	if(sum(slice[,j],na.rm=TRUE)==0) {
		cat('Warning: no data between ',cuts[j],' and ',cuts[j+1],'. Adjusting levels for you.\n')
		cuts[j+1]=cuts[j]  #pick up the cutlevel for next slice to cover zone
		#note: this makes colors[j] get skipped; here's a fix for that
		cols<-cols[c(length(cols),1:(length(cols)-1))]
		next #don't calc or plot this slice
		}
	#identify the lengths of data and nondata 
	#  rle doesn't like NAs so do this layered thing
	runs<-rle(!is.na(as.logical(slice[,j])))
	thepos<-0
	#note: this starts thepos at the END of the first 'length' so I
	#never bang into the x[1],y[1] data point.  Similarly, it stops at beginning
	#of last 'length'.
	#reset the values of x to use for this slice
	slicex[,j]<-xx
	for (i in 1:(length(runs$lengths)-1)) {
			thepos<-thepos+runs$lengths[i]
			# "add" interp point at the end of each run
			# whichway chooses which 'side' of interval to interpolate towards
			whichway<-slicelog[thepos]
			#pick correct cut value  - is subslice is going up or down
			#whichcut chooses the cut to use
			whichcut<-as.logical(yy[thepos]>=cuts[j]&yy[thepos+1]>=cuts[j])
			# use approx() instead of hard-coded linterp
			#note the interpolation is TO x FROM y
			xint3<-approx(c(yy[thepos:(thepos+1)]),c(x[thepos:(thepos+1)]),cuts[j+whichcut])
			slicex[thepos+whichway,j]<-xint3$y
			slice[thepos+whichway,j]<-cuts[j+whichcut] 
			}
	#suppression lets me pass '...' to both plot and lines calls
	suppressWarnings(lines(slicex[,j],slice[,j],col=cols[j],...))
	if(showcuts==TRUE & j < (length(cuts)-1)) {
		lines(slicex[,j],cuts[j+1]*rep(1,length(slicex[,j])),lty='dotted', col='black')
		}
	}
#save interesting stuff 
stuff<-list(xin=x,yin=y,cuts=cuts,slicex=slicex,slicey=slice)
return(invisible(stuff))
}
