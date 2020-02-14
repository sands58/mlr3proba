LearnerDensKDEpd <- R6::R6Class("LearnerDensKDEpd", inherit = LearnerDens,
                    public = list(initialize = function(id = "dens.kdePD"){
                    super$initialize(
                    id = id,
                    feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
                    predict_types = "pdf",
                    packages = c("plugdensity", "distr6")
                    )},

                    train_internal = function(task){

                    data = as.numeric(unlist(task$data(cols = task$target_names)))

                    pdf <- function(x1){}

                    body(pdf) <- substitute({

                    invoke(plugdensity::plugin.density, x = data, xout = x1)$y
                    })


                    Distribution$new(name = paste("Gaussian KDE"),
                                                          short_name = paste0("GausKDE"),
                                                          pdf = pdf)
                                  },

                      predict_internal = function(task){

                      newdata = as.numeric(unlist(task$data(cols = task$target_names)))

                      PredictionDens$new(task = task, pdf = self$model$pdf(newdata))

              }
))

