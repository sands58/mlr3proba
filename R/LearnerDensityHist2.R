LearnerDensityHist <- R6::R6Class("LearnerDensityHist", inherit = LearnerDensity,

                      public = list(initialize = function(){
                      super$initialize(
                      id = "density.Hist",
                      param_set = ParamSet$new(
                                  params = list(
                                  ParamDbl$new(id = "numbin", lower = 0, tags = "train"),
                                  ParamDbl$new(id = "binwidth", tags = "train"),
                                  ParamDbl$new(id = "origin", tags = lower= 0, "train"),
                                  ParamDBl$new(id = "binwidth", lower = 0, tags = predict)
                                  )),
                      feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
                      predict_types = "prob",
                      packages = "distr6")},

                      train_internal = function(task){

                      pars = self$param_set$get_values(tag="train")

                      data = as.vector(task$data(cols = task$feature_names))

                      #this is called self$model
                      #using histogram1.R
                      invoke(histogram, traindata = data, .args = pars)
                      }

                      predict_internal = function(task){

                      pars = self$param_set$get_values(tags = "predict")

                      newdata_Intervals = as.vector(findInterval(newdata, self$XIntervals,
                                                    rightmost.closed= TRUE))
                      pdf = function(x1){

                      return(sapply(newdata_Intervals, function(x) self$ProbBin[[x]]/self$getParameterValue("binwidth")))

                      }

                  ##?   support = Interval$new(1, 10)
                      U =  Distribution$new(name= "Histogram", short_name = "hist", pdf = pdf, paramaters = pars, support = support)

                      }
                       ))


