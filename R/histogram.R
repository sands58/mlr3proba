#Histogram function
#-------------------

#Description: Finding the pdf using histogram using the hist function in R

#Usage: histogram(traindata, binwidth, origin, numbin)

#Arguments
#1. data to estimate the density
#2. numbin: the size of each bins. Can be a single numberr, vector, a function (see hist)

#values: a list that output
#1. density of the training data
#2. the bin and with its respective density
#3. the x-axis of the histogram


#Example:

 # data <- c(1.5,1.5,2.5,3.5,6.5,6.3,7.4,8.3)
 # numbin <- 2
 # a <- .histogram(dat = data)

.histogram <- function(dat, breaks = "Sturges"){
  a <- graphics::hist(x = dat, breaks = breaks, include.lowest = TRUE, plot = FALSE, right = FALSE)
  dt <- data.table::data.table(Intervals = a$breaks, binPdf =  c(a$density, a$density[length(a$density)]))

  pdf = function(x1){}
  body(pdf) = substitute({
    as.numeric(unlist(data[findInterval(x1, data$Intervals, left.open = FALSE, rightmost.closed = TRUE), 2]))
   }, list(data = dt))

  cdf = function(x1){}
  body(cdf) = substitute({
    .histogram_cdf(val = x1, Intervals = data$Intervals, pdf = data$binPdf)
  }, list(data = dt))

  distr6::Distribution$new(name = "Histogram Estimator",
                           short_name = "Histogram",
                           pdf = pdf, cdf = cdf,
                           support = distr6::Interval$new(min(dt$Intervals), max(dt$Intervals)))
}



# Description: Compute the cdf of a histogram using the density and the
#              relative intervals. The lower limit of the cdf must
#              always be the lowest limit of the histogram. To find the
#              cdf of a histogram between the values must compute cdf twice
#              and substract.

# Arguments:
# 1. val: component of Intervval which the upper limit belong to
# 2. Intervals: The intervals/break of the histogram. A vector
# 3. Pdf: pdf for each interval. a vector

.histogram_cdf <- function(val, Intervals, pdf){
  sapply(findInterval(val, Intervals, rightmost.closed = TRUE, left.open = FALSE),
         function(x) sum(pdf[1:x] * (Intervals[2:(x+1)] - Intervals[1:x])))
}



