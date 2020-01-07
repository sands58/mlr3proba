library(R6)


Hist<- R6Class("Hist", public = list(

  bin = NA,
  num =NA,
  or = NA,

  initialize = function(bin=NULL, num=NULL, or=NULL){

    self$bin <- bin
    self$num <- num
    self$or <- or

  },

  model = function(data){

  return(histogram(data =data, numbin = self$num, binwidth = self$bin, origin = self$or))

  },

    predict = function(newdata){

    newdata_Intervals = as.numeric(findInterval(newdata, self$model$Intervals,
                                                rightmost.closed= TRUE, left.open = F))
    num_zero <- length(which(newdata_Intervals %in% !self$model$numBin))
    pdf_hist <- ifelse(newdata_Intervals %in% self$model$numBin,
                       c(rep(0, self$num_zero),model$binPdf[newdata_Intervals]),
                       rep(0, length(newdata_Intervals)))

  }
))

x
a <- Hist$new(bin = 0.3, or =0)
a$model(x)
a$a(3.4)
a$model(x)
