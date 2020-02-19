LearnerDensKDEnp <- R6::R6Class("LearnerDensKDEsm", inherit = LearnerDens,
                                public = list(initialize = function(id = "dens.kdeNP"){
                                  super$initialize(
                                    id = id,
                                    param_set = ParamSet$new(
                                      params = list(
                                        ParamFct$new(id = "ckertype", default = "gaussian",
                                                      levels = c("gaussian", "epanechnikov"),
                                                       tags = c("train", "predict")),
                                        ParamDbl$new(id = "bws", tags = "predict")
                                        )),
                                    feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
                                    predict_types = "pdf",
                                    packages = c("np", "distr6")
                                  )},
                    train_internal = function(task){

                    pars = self$param_set$get_values(tag="train")

                    data = as.data.frame(unlist(task$data(cols = task$target_names)))

                    # pdf <- function(x1){}
                    #
                    # body(pdf) <- substitute({


                    target = task$truth()

                    invoke(np::npudensbw, dat = data, edat = target, .args = pars)

                    # })
                    #
                    #
                    # Distribution$new(name = paste("Gaussian KDE"),
                    #                  short_name = paste0("GausKDE"),
                    #                  pdf= pdf)
                    },

                    predict_internal = function(task){

                    pars = self$param_set$get_values(tags = "predict")

                    newdata = as.data.frame(unlist(task$data(cols = task$target_names)))

                    pdf = as.numeric(invoke(np::npudens,  bws = self$model, edat = newdata, .args = pars)$dens)

                    PredictionDens$new(task = task, pdf = pdf)

                    }

                    ))

