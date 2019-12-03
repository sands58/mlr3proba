LearnerDensityHist <- R6::R6Class("LearnerDensityHist", inherit = LearnerDensity,
                                  public = list(initialize = function(){
                                    super$initialize(
                                      id = "density.Hist",
                                      param_set = ParamSet$new(
                                        params = list(
                                          ParamDbl$new(id = "numbin", tags = "train")
                                          #add parameter of origin (id .....)
                                          # ParamDbl$new(id = "binwidth", tags = "trains")
                                        )),
                                      feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
                                      predict_types = "prob",
                                      packages = "distr6")},

                                    train_internal = function(task){

                                      pars = self$param_set$get_values(tag="train")

                                      data = as.vector(task$data(cols = task$feature_names))
                                      # pdf <- function(data){}
                                      #   body(pdf) <- substitute({


                                      invoke(histogram, traindata = data, .args = pars)
                                    }

                                    #      predict_internal = function(task){

                                    #       pars = self$param_set$get_values(tags = "predict")
                                    #       newdata_Intervals = as.vector(findInterval(newdata, self$XIntervals,
                                    #                                    rightmost.closed= TRUE))
                                    #       test_pdf = sapply(newdata_Intervals, function(x) self$ProbBin[[x]]/self$BinWidth)}


                                  ))


