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
  fit <- graphics::hist(x = dat, breaks = breaks, include.lowest = TRUE, plot = FALSE, right = FALSE)

  pdf = function(x1){}
  body(pdf) = substitute({
    f[findInterval(x1, Intervals, left.open = F, rightmost.closed = T)]
   }, list(f = fit$density, Intervals = fit$breaks))

  cdf = function(x1){}
  body(cdf) = substitute({
    ccounts[findInterval(x1, Intervals, left.open = F, rightmost.closed = T)]/total
  }, list(ccounts = cumsum(fit$counts), Intervals = fit$breaks, total = sum(fit$counts)))

  list(distr = distr6::Distribution$new(name = "Histogram Estimator",
                           short_name = "Histogram",
                           pdf = pdf, cdf = cdf,
                           support = distr6::Interval$new(min(fit$breaks), max(fit$breaks))),
       hist = fit)
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

# .histogram_cdf <- function(val, Intervals, pdf){
#   sapply(findInterval(val, Intervals, rightmost.closed = TRUE, left.open = FALSE),
#          function(x) sum(pdf[1:x] * (Intervals[2:(x+1)] - Intervals[1:x])))
# }



