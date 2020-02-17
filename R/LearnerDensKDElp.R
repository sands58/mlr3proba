LearnerDensKDElp <- R6::R6Class("LearnerDensKDElp", inherit = LearnerDens,
                    public = list(initialize = function(id = "dens.kdeLP"){
                    super$initialize(
                    id = id,
                    param_set = ParamSet$new(
                    params = list(
                        #ParamDbl$new(id = "lbound", tags = "train"),
                        #ParamDbl$new(id = "ubound", tags = "train"),
                    #    ParamDbl$new(id = "maxknots", default = 0L, tags = "train"),
                      #  ParamDbl$new(id = "knots", tags = "train"),
                     #   ParamDbl$new(id = "nknots", default = 0, tags = "train"),
                      #  ParamDbl$new(id = "mind", default = -1, tags = "train"),
                      #  ParamUty$new(id = "silent", default = TRUE, tags ="train"),
                      #  ParamUty$new(id = "error_action", default = 2, tags ="train"),
                   #     ParamUty$new(id = "penalty", tags = "train"),
                        )),
                    feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
                    predict_types = "pdf",
                    packages = c("logspline", "distr6")
                    )},

                    train_internal = function(task){

                    data = as.numeric(unlist(task$data(cols = task$target_names)))

                    pdf <- function(x1){}

                    body(pdf) <- substitute({


                    invoke(logspline::dlogspline, q = x1, fit = logspline(data))

                    })


                    Distribution$new(name = paste("Penalized Density"),
                                     short_name = paste0("PenDens"),
                                     pdf = pdf)
                    },

                    predict_internal = function(task){

                    newdata = as.numeric(unlist(task$data(cols = task$target_names)))

                    PredictionDens$new(task = task, pdf = self$model$pdf(newdata))

                    }
                                ))

