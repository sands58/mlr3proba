LearnerDensPenLP <- R6::R6Class("LearnerDensPenLP", inherit = LearnerDens,
                    public = list(initialize = function(id = "dens.penLP"){
                      ps = ParamSet$new(
                        params = list(
                          # ParamDbl$new(id = "maxknots", default = 0, tags = "train"),
                           ParamUty$new(id = "knots", default = c(0, 0.1, 0.2, 0.3),
                                        tags = "train")
                          # ParamDbl$new(id = "nknots", default = 0, tags = "train"),
                          # ParamDbl$new(id = "mind", default = -1, tags ="train"),
                          # ParamLgl$new(id = "silent", default = TRUE, tags = "train"),
                          # ParamUty$new(id = "error.action", default = 2, tags = "train")
                          ))

                  #  ps$values = list(maxknots  = 0, nknots = 0, mind = -1, silent = TRUE, error.action = 2)
                    super$initialize(
                    id = id,
                    param_set = ps,
                    feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
                    predict_types = "pdf",
                    packages = c("logspline", "distr6")
                    )},

                    train_internal = function(task){

                    data = as.numeric(unlist(task$data(cols = task$target_names)))

                    pdf <- function(x1){}

                    body(pdf) <- substitute({

                    logspline::dlogspline(x1,
                            logspline::logspline(x= data,  knots = k))

                                                       #  nknots = nk,
                                                       # silent = s, maxknots = mk,
                                                       # mind = m, error.action = ea))

                    }, list(k =  self$param_set$values$knots))
                            # mk = self$param_set$values$maxknots,
                            # nk =  self$param_set$values$nknots,
                            # s =  self$param_set$values$silent,
                            # m =  self$param_set$values$mind,
                            # ea =  self$param_set$values$error.action))

                    Distribution$new(name = paste("Density Penalized"),
                                     pdf = pdf)

                   },

                  predict_internal = function(task){

                  newdata = as.numeric(unlist(task$data(cols = task$target_names)))

                  PredictionDens$new(task = task, pdf = self$model$pdf(newdata))

                  }
                  ))
