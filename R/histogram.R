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
# a <- .histogram(data = data,  numbin = numbin)

.histogram <- function(data, breaks = "Sturges", include.lowest = TRUE, right = TRUE){
  a <- hist(x= data, breaks = breaks, include.lowest = include.lowest, plot = FALSE, right = right)
  data.table::data.table(Intervals = a$breaks[-1], binPdf = a$density)

}




