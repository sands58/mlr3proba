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

# data <- c(1,1,2.5,3.5,6.5,6.3,7.4,8.3)
# newdata <- c(1, 2.3, 6)
# binwidth <-NULL
# numbin <- NULL
# origin <- 2
# a <- histogram(data = data,  binwidth = 1, origin = 2)


require(pkgcond)
library(pkgcond)

histogram <- function(data, numbin= NULL, binwidth=NULL, origin=NULL){

  X <-  data
  # sort the data
  XSort <-  sort(X, decreasing = F)
  Xmin <-  min(XSort)
  Xmax <-  max(XSort)
  # find the difference between maximum and minimum
  XDif <- ifelse(is.null(origin),  Xmax- Xmin, Xmax - origin)

  origin <- ifelse(is.null(origin), 0, origin)

  p <- ifelse(is.null(binwidth) == is.null(numbin), 0,
            ifelse(!is.null(binwidth) == !is.null(numbin), 2, 1))
  #if p=0, both numbin = binwidth = NULL
  #if p= 1, either binwidth= NUll  OR numbin = NULL
  #if p=2, numbin != Null and binwidth = !=NULL


  if(p == 2){
       NumBin <- ceiling(log(length(X), 2) +1)
   } else if(p ==1){
         if(is.null(numbin)){
               NumBin <- ceiling(XDif/binwidth)
        } else{NumBin <- numbin}
   } else {NumBin <- NULL}

  if(p %in% c(1,2)){
  BinWidth <-  XDif/NumBin

  IndexNumBin <- c(1:NumBin)
  # find the interval of the bin
  XSort <- if(is.null(origin)){XSort} else(XSort[which(XSort >= origin)])

  Interval <- as.vector(sapply(BinWidth, function(x, y) origin + y*x, y = IndexNumBin))
  XInterval <- c(origin, Interval)

  #Count the number of training data in each interval
  Bin <- table(cut(XSort, XInterval, include.lowest = T))
  ProbBin <- as.vector(Bin/(length(XSort)*BinWidth))
  numBin <- as.vector(c(1:length(ProbBin)))
  newtraindata <- ifelse(data %in% XSort, data, 0)
  TrainIntervals <- as.numeric(findInterval(newtraindata, XInterval,
                                    rightmost.closed = T,left.open = F))
  num_zero <- length(which(TrainIntervals %in% !numBin))
  Pdf <- ifelse(TrainIntervals %in% numBin,
                        c(rep(0, num_zero),ProbBin[TrainIntervals]),
                        rep(0, length(TrainIntervals)))
  } else{Pdf = NULL
         ProbBin = NULL
         XInterval = NULL
         BinWidth = cat(paste("only provide binwidth OR numbin"))}

  return(list(pdf_data = Pdf, binPdf = ProbBin, Intervals = XInterval,
              Binwidth = BinWidth, numBin = numBin))
}


newdata_Intervals = as.numeric(findInterval(newdata, a$Intervals,
                                           rightmost.closed= TRUE, left.open = F))
num_zero <- length(which(newdata_Intervals %in% !a$numBin))
ifelse(newdata_Intervals %in% a$numBin, c(rep(0, num_zero),a$binPdf[newdata_Intervals]),
       rep(0, length(newdata_Intervals)))

