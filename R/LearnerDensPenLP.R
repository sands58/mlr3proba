LearnerDensPenLP <- R6::R6Class("LearnerDensPenLP", inherit = LearnerDens,
                    public = list(initialize = function(id = "dens.penLP"){
                    ps = ParamSet$new(
                    params = list(
                    # ParamDbl$new(id = "lbound", tags = "train"),
                    # ParamDbl$new(id = "ubound", tags = "train"),
                    ParamDbl$new(id = "maxknots", default = 0, tags = "train"),
                    ParamUty$new(id = "knots",  tags = "train"),
                    ParamDbl$new(id = "nknots", default = 0, tags = "train"),
                    ParamDbl$new(id = "mind", default = -1, tags ="train"),
                    ParamUty$new(id = "silent", default = TRUE, tags = "train"),
                    ParamUty$new(id = "error.action", default = 2, tags = "train"),
                    ParamUty$new(id = "penalty", default = -1, tags = "train")
                        ))

                    ps$values = list(maxknots  = 0,  nknots = 0, mind = -1,
                                     silent = TRUE, error.action = 2, penalty = -1)
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


                    logspline::dlogspline(q = x1, fit = logspline::logspline(data, #lbound = lb, ubound = ub,
                                                                  maxknots = mk,  knots = k, nknots = nk, penalty = p,
                                                                  silent = s, mind = m, error.action = ea))

                    },
                    list(lb = self$param_set$values$lbound, ub = self$param_set$values$ubound,
                         mk = self$param_set$values$maxknots,
                         p =  self$param_set$values$penalty,
                          k = self$param_set$values$knots,
                         nk = self$param_set$values$nknots, s = self$param_set$values$silent,
                         m = self$param_set$values$mind, ea = self$param_set$values$error.action)
                    )


                    Distribution$new(name = paste("Penalized Density"),
                                     short_name = paste0("PenDens"),
                                     pdf = pdf)
                    },

                    predict_internal = function(task){

                    newdata = as.numeric(unlist(task$data(cols = task$target_names)))

                    PredictionDens$new(task = task, pdf = self$model$pdf(newdata))

                    }
                                ))

