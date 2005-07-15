# freq calculates a simple frequency table for a vector, or steps through
# the columns of a data frame or matrix and returns a list of the freqwuncy
# table(s).

freq<-function(x,variable.labels=NULL,display.na=TRUE) {
 
 if(missing(x))
  stop("Usage: freq(x,...) where x is a vector, dataframe or matrix")
 # get the variable label here or it might be clobbered
 if(is.null(dim(x))) {
  if(is.null(variable.labels)) variable.labels<-deparse(substitute(x))
  x<-list(x)
  nfreq<-1
 }
 else {
  nfreq<-dim(x)[2]
  if(is.null(variable.labels)) variable.labels<-names(x)
 }
 freq.list<-rep(list(0),nfreq)
 for(i in 1:nfreq) {
  # see if there are any NAs and if they should be displayed
  if(display.na) nna<-sum(is.na(x[[i]]))
  else nna<-0
  if(is.numeric(x[[i]])) {
   xrange<-range(na.omit(x[[i]]))
   # tabulate drops categories for integers having no observations
   categories<-seq(xrange[1],xrange[2])
   # another serious kludge - tabulate() always starts at 1
   if(xrange[1] != 1) x<-x-(xrange[1]-1)
  }
  if(is.character(x)) x<-as.factor(x[[i]])
  categories<-levels(as.factor(x[[i]]))
  nbins<-length(categories)
  # if NAs present, tack on a label
  if(nna) categories<-c(categories,"NA")
  # tabulate barfs with NAs
  freqs<-tabulate(na.omit(x[[i]]),nbins)
  # tack on the NA count
  if(nna) freqs<-c(freqs,nna)
  names(freqs)<-categories
  freq.list[[i]]<-freqs
 }
 names(freq.list)<-variable.labels
 class(freq.list)<-"freq"
 return(freq.list)
}

print.freq<-function(x,show.pc=TRUE,...) {
 nfreq<-length(x)
 variable.labels<-names(x)
 for(i in 1:nfreq) {
  categories<-names(x[[i]])
  maxchar<-max(c(nchar(categories),4))
  cat("\nFrequencies for",variable.labels[i],"\n")
  cat(" ",formatC(ifelse(categories=="","Missing",categories),width=maxchar),
   "\n")
  cat(" ",formatC(as.character(x[[i]]),width=maxchar),"\n")
  if(show.pc) {
   percentages<-round(100*x[[i]]/sum(x[[i]]),1)
   cat("%",formatC(as.character(percentages),width=maxchar),"\n")
  }
  cat("\n")
 }
}
