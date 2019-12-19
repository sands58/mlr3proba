#Histogram function

#Description: Finding the pdf using histogram

#Usage: histogram(traindata, binwidth, origin, numbin)

#Arguments
#1. traindata is the data that you want to find its density by histogram
#2. numbin: the size of each bins. If not given by users, a default
#           number is used using Sturge rule. If binwidth is given, then
#           the numbin is calculated using binwidth
#3. binwidth: the size of each bin, if not passed then is calculated by
#             using the numbin. If both numbin and binwidth is not given,
#             numbin that is calculated by Sturge rule is used to find the
#             binwidth.
#4. origin: the starting point of the historam. By default, it will start from
#           the minimum data.

#Details: require(pkgcond)

#values: a list that output
#1. density of the training data
#2. the bin and with its respective density
#3. the x-axis of the histogram


#Example:

data <- c(1,1,2,3,4,5,6,6,7,8)
newdata <- c(1, 4.5, 6)
histogram(data = data, numbin =5,binwidth = NULL, origin = 2)


require(pkgcond)
library(pkgcond)

histogram <- function(data, numbin=NULL, binwidth=NULL, origin=NULL){

  X <-  data
  # sort the data
  XSort <-  sort(X, decreasing = F)
  Xmin <-  min(XSort)
  Xmax <-  max(XSort)
  # find the difference between maximum and minimum
  XDif <- ifelse(is.null(origin),  Xmax- Xmin, Xmax - origin)

  if(is.null(c(binwidth, numbin))){
             NumBin <- ceiling(log(length(X), 2) +1)
  }  else if(is.null(numbin)){
             NumBin <- ceiling(XDif/binwidth)
    } else (NumBin <- numbin)

  BinWidth <- ifelse(is.null(binwidth), XDif/NumBin, binwidth)

  IndexNumBin <- c(1:NumBin)
  # find the interval of the bin
  XSort <- if(is.null(origin)){XSort} else(XSort[which(XSort > origin)])

  Interval <- as.vector(sapply(BinWidth, function(x, y) origin + y*x, y = IndexNumBin))
  XInterval <- c(origin, Interval)

  #Count the number of training data in each interval
  Bin <- table(cut(XSort, XInterval, include.lowest = T))
  ProbBin <- Bin/(length(XSort)*BinWidth)

  newtraindata <- ifelse(data %!in% XSort, 0, data)
  TrainIntervals <- findInterval(newtraindata, XInterval, rightmost.closed = T)

  Pdf <- ifelse(TrainIntervals != 0, ProbBin[TrainIntervals[]], 0)


  return(list(pdf = Pdf, binPdf = ProbBin, Intervals = XInterval, Binwidth = BinWidth))
}




