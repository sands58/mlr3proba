LearnerDensityHist <- R6::R6Class("LearnerDensityHist", inherit = LearnerDensity,

                      public = list(initialize = function(){
                      super$initialize(
                      id = "density.Hist",
                      param_set = ParamSet$new(
                                  params = list(
                                  ParamInt$new(id = "numbin", lower = 0, tags = "train"),
                                  ParamDbl$new(id = "binwidth", tags = "train"),
                                  ParamInt$new(id = "origin", lower= 0, tags= "train")
                                  )),
                      feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
                      predict_types = "prob",
                      packages = "distr6")},

                      train_internal = function(task){

                      pars = self$param_set$get_values(tag="train")

                      data = as.vector(task$data(cols = task$feature_names))
                      #this is called self$model
                      #using histogram1.R
                      # change later histogram function name in histogram file = > .histogram
                      #invoke in packge purr
                      invoke(.histogram, traindata = data, .args = pars)
                      }

                      predict_internal = function(task){

                      pars = self$param_set$get_values(tags = "predict")

                      pdf = function(x1){

                      newdata_Intervals = as.numeric(findInterval(x1, self$model$Intervals,
                                                                    rightmost.closed= TRUE, left.open = F))
                      num_zero <- length(which(newdata_Intervals %in% !self$model$numBin))

                      pdf_hist <- ifelse(newdata_Intervals %in% self$model$numBin,
                             c(rep(0, num_zero),self$model$binPdf[newdata_Intervals]),
                             rep(0, length(newdata_Intervals)))

                      return(pdf_hist)

                      }


                      cdf <- function(pdf, upper_limit){

                        length_upper_limit <- length(which(a$Intervals <= 6.5))
                        area <- rep()
                        for(i in 1:(length_upper_limit - 1)){

                          area[i] <- (model$Intervals[i+1] - model$Intervals[i]) * model$binPdf[i]
                        }
                        return(sum(area))

                      }

                      support = Interval$new(min(truth), max(truth))
                      pdf =  Distribution$new(name= "Histogram", short_name = "hist", pdf = pdf, cdf = cdf,
                                              paramaters = pars, support = support)

                      }
                       ))


