# heart_count.R
# Jim Lemon 2008-10-21
# trial script for reading Presentation log file for the heartbeat counting task,
# calculating interbeat interval (IBI) and heart rate variability and
# responses to the items.
# Assumptions:
# Each log file event coded "Pulse" is a signal from the pulse sensor.
# The value is the number of ten-thousandths of a second since the last signal.
# A long interval signals the onset of the R wave.
# This is delayed by transmission through the arterial system to the finger.
# The R-R heart rate is the time from the beginning of one long interval (>100 ms)
# to the beginning of the next long interval.
#
# log file has already been processed into CSV with a header line
heart_count<-function(logfilename) {
 hcdata<-read.csv(logfilename)
 # get all the pulse intervals in milliseconds
 ibi<-diff(hcdata$Time[hcdata$EventType=="Pulse"])/10
 ibindex<-i<-1
 # discard any initial "ticks" to get to the first leading edge
 while(ibi[i] < 100) i<-i+1
 # set the first value to the interval at the leading edge of the first pulse
 ibi[1]<-ibi[i]
 while(i <= length(ibi)) {
  # if it's a "tick", just add the value to the current IBI
  if(ibi[i] < 100) ibi[ibindex]<-ibi[ibindex]+ibi[i]
  # otherwise it's a leading edge, so move to the next IBI element
  # and set it to the current value
  else {
   # increment the index
   ibindex<-ibindex+1
   # set the next pulse value to the R wave interval
   ibi[ibindex]<-ibi[i]
  }
  i<-i+1
 }
 # use the space after the interbeat interval values to
 hrvindex<-ibindex
 # calculate a rolling window variability for the series
 for(i in 1:(ibindex-99)) ibi[hrvindex+i]<-sd(ibi[i:(i+99)])
 # get a filename from the log file name
 pngfile<-paste(strsplit(logfilename,".",fixed=TRUE)[[1]][1],"png",sep=".")
 # plot it the lazy way
 require(plotrix)
 png(pngfile,width=1000)
 twoord.plot(1:ibindex,ibi[1:ibindex],
  50:(hrvindex-50),ibi[(hrvindex+1):(hrvindex+i)],
  main="Interbeat interval by time",xlab="Beat",ylab="IBI (ms)",
  rylab="Variability (sd)",type="l")
 dev.off()
}
