LearnerDensKDEstats <- R6::R6Class("LearnerDensKDEstats", inherit = LearnerDens,
                        public = list(initialize = function(id = "dens.kdeSTATS"){
                        super$initialize(
                        id = id,
                        param_set = ParamSet$new(
                        params = list(
                        ParamDbl$new(id = "bw",  lower = 0, tags = "train"),
                        ParamFct$new(id = "kernel", levels = c("gaussian", "epanechnikov", "uniform"),
                                                 default = "gaussian", tags = "train"),
                        ParamUty$new(id = "weights", default= NULL, tags = "train")
                        )),
                        feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
                        predict_types = "pdf",
                        packages = c("stats", "distr6")
                        )},

                        train_internal = function(task){

                        pars = self$param_set$get_values(tag="train")

                        data = as.numeric(unlist(task$data(cols = task$target_names)))

                        fit = invoke(.DensStats, x = data,  .args = pars)

                        set_class(list(distr = fit$distr, dens = fit$dens), "dens.kdeSTATS")
                        },

                        predict_internal = function(task){

                        newdata = as.numeric(unlist(task$data(cols = task$target_names)))

                        PredictionDens$new(task = task, pdf = self$model$pdf(newdata))

                        }

                                ))

