integer.frequency<-function(x,bins) {
 if(!is.numeric(x)) stop("x must be numeric")
 if(missing(bins)) bins<-sort(unique(x))
 nbins<-length(bins)
 fx<-rep(0,nbins)
 for(i in 1:nbins) fx[i]<-sum(x == bins[i])
 return(fx)
}

# freq calculates a simple frequency table for a vector, or steps through
# the columns of a data frame or matrix and returns a list of the frequency
# table(s).

freq<-function(x,variable.labels=NULL,display.na=TRUE,bin.range=NULL,
 include.empty=FALSE) {
 
 if(missing(x))
  stop("A vector, dataframe or matrix must be supplied")
 xdim<-dim(x)
 # get the variable label here or it might be clobbered
 if(is.null(xdim)) {
  if(is.null(variable.labels)) variable.labels<-deparse(substitute(x))
  x<-list(x)
  nfreq<-1
 }
 else {
  nfreq<-xdim[2]
  if(is.null(variable.labels)) variable.labels<-names(x)
  if(is.null(variable.labels))
   variable.labels<-paste("V",1:xdim[2],sep="",collapse="")
 }
 freq.list<-rep(list(0),nfreq)
 for(i in 1:nfreq) {
  # see if there are any NAs and if they should be displayed
  if(display.na) nna<-sum(is.na(x[[i]]))
  else nna<-0
  # integer.frequency barfs with NAs
  xt<-na.omit(x[[i]])
  categories<-levels(as.factor(xt))
  if(is.numeric(x[[i]])) {
   if(include.empty) {
    if(is.null(bin.range)) bins<-min(xt):max(xt)
    else bins<-bin.range[1]:bin.range[2]
    categories<-as.character(bins)
    freqs<-integer.frequency(xt,bins)
   }
   else freqs<-integer.frequency(xt)
  }
  else freqs<-table(xt)
  # if NAs present, tack on a label
  if(nna) categories<-c(categories,"NA")
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
  cat("    ",formatC(ifelse(categories=="","Missing",categories),width=maxchar),
   "\n")
  cat("    ",formatC(as.character(x[[i]]),width=maxchar),"\n")
  if(show.pc) {
   percentages<-round(100*x[[i]]/sum(x[[i]]),1)
   cat("%   ",formatC(as.character(percentages),width=maxchar),"\n")
   if(any(names(x[[i]]) == "NA")) {
    xlen<-length(x[[i]])
    # get the number of NAs
    nna<-x[[i]][xlen]
    percentages<-round(100*x[[i]][-xlen]/sum(x[[i]][-xlen]),1)
    cat("%!NA",formatC(as.character(percentages),width=maxchar),"\n")
   }
  }
  cat("\n")
 }
}
