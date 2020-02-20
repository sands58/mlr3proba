LearnerDensPenLP <- R6::R6Class("LearnerDensPenLP", inherit = LearnerDens,
                                public = list(initialize = function(id = "dens.penLP"){
                                  super$initialize(
                                    id = id,
                                    param_set = ParamSet$new(
                                      params = list(
                                      #  ParamDbl$new(id = "maxknots", default = 0, tags = "train"),
                                        ParamUty$new(id = "knots",  tags = "train"),
                                       # ParamDbl$new(id = "nknots", default = 0, tags = "train"),
                                      #  ParamDbl$new(id = "mind", default = -1, tags ="train"),
                                      #  ParamUty$new(id = "silent", default = TRUE, tags = "train"),
                                      #  ParamUty$new(id = "error.action", default = 2, tags = "train"),
                                     #   ParamUty$new(id = "penalty", default = -1, tags = "train"),
                                        ParamUty$new(id = "fit", tags = "predict")
                                      )),
                                    feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
                                    predict_types = "pdf",
                                    packages = c("logspline", "distr6")
                                  )},

                                  train_internal = function(task){

                                    pars = self$param_set$get_values(tags = "train")

                                    data = as.numeric(unlist(task$data(cols = task$target_names)))

                                    invoke(logspline::logspline, x = data, .args = pars)

                                  },

                                  predict_internal = function(task){

                                    newdata = as.numeric(unlist(task$data(cols = task$target_names)))

                                    pars = self$param_set$get_values(tags = "predict")

                                    pdf  = invoke(logspline::dlogspline, q = newdata, fit = self$model)

                                    PredictionDens$new(task = task, pdf = pdf)

                                  }
                                ))
