LearnerDensKDEks <- R6::R6Class("LearnerDensKDEks", inherit = LearnerDens,
        public = list(initialize = function(id = "dens.kde.ks"){
        super$initialize(
        id = id,
        param_set = ParamSet$new(
        params = list(
        ParamDbl$new(id = "h",  tags = "train"))),
        feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("pdf","cdf"),
        packages = c("ks", "distr6")
        )},

        train_internal = function(task){


                pars = self$param_set$get_values(tag="train")

                data = as.numeric(unlist(task$data(cols = task$target_names)))

                pdf <- function(x1){}

                body(pdf) <- substitute({

                invoke(ks::kde, x = data, eval.points = x1, .args =pars)$estimate

                })

                list(distr = distr6::Distribution$new(name = "Gaussian KDe",
                                                      short_name = "GausKDE",
                                                      pdf = pdf))
                },

                predict_internal = function(task){

                newdata = as.numeric(unlist(task$data(cols = task$target_names)))

                PredictionDens$new(task = task, pdf = self$model$distr$pdf(newdata))

                }
                ))

