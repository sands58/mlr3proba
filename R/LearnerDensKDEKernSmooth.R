LearnerDensKDEKernSmooth<- R6::R6Class("LearnerDensKernSmooth", inherit = LearnerDens,
                                public = list(initialize = function(id = "dens.kdeKernSmooth"){
                                  super$initialize(
                                  id = id,
                                  param_set = ParamSet$new(
                                    params = list(
                                      ParamDbl$new(id = "gridsize", default = 401L, tags = "train"),
                                      ParamUty$new(id = "truncate", default = TRUE, tags = "train"),
                                      ParamUty$new(id = "canonical", default = FALSE, tags = "train"),
                                      ParamFct$new(id = "kernel", level = c("normal", "box", "epanech", "biweight",
                                                                            "triweight"), default= "normal",
                                                   tags = "trains")
                                      )),
                                    feature_types =  c("integer", "numeric", "character", "factor", "ordered"),
                                    predict_types = "pdf",
                                    packages = c("KernSmooth", "distr6")
                                  )},

                                  train_internal = function(task){

                                    data = as.numeric(unlist(task$data(cols = task$target_names)))

                                    pars = self$param_set$get_values(tag="train")

                                    pdf <- function(x1){}

                                    body(pdf) <- substitute({

                                    invoke(KernSmooth::bkde, x = data, range.x = x1, .args = pars)$y

                                    })

                                    Distribution$new(name = paste("KDE",self$param_set$values$kernel),
                                                     short_name = paste0("KDE"),
                                                     pdf = pdf)
                                  },

                                  predict_internal = function(task){

                                    newdata = as.numeric(unlist(task$data(cols = task$target_names)))

                                    PredictionDens$new(task = task, pdf = self$model$pdf(newdata))

                                  }
                                ))

