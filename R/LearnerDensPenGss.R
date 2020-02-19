LearnerDensPenGss <- R6::R6Class("LearnerDensPenGss", inherit = LearnerDens,
                    public = list(initialize = function(id = "dens.penGSS"){
                    super$initialize(
                    id = id,
                    param_set = ParamSet$new(
                    params = list(
                    ParamUty$new(id = "data", default =list(), tags = "train"),
                    ParamDbl$new(id = "alpha", default =1.4, tags = "train"),
                    ParamUty$new(id = "weights", default =NULL, tags = "train"),
                    ParamUty$new(id = "na.action", default =na.omit, tags = "train"),
                    ParamUty$new(id = "nbasis", default =NULL, tags = "train"),
                    ParamUty$new(id = "seed", default =NULL, tags = "train"),
                    ParamUty$new(id = "domain", default = as.list(NULL), tags = "train"),
                    ParamUty$new(id = "quad", default =NULL, tags = "train"),
                    ParamUty$new(id = "qdsz.depth", default =NULL, tags = "train"),
                    ParamUty$new(id = "bias", default =NULL, tags = "train"),
                    ParamDbl$new(id = "prec", default = 1e-7, tags = "train"),
                    ParamDbl$new(id = "maxiter", default = 30, tags = "train"),
                    ParamLgl$new(id = "skip.iter", default = FALSE, tags = "train"),
                    ParamUty$new(id = "object", tags = "predict")
                   )),
                    feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
                    predict_types = "pdf",
                    packages = c("gss", "distr6")
                    )},

                    train_internal = function(task){

                    pars = self$param_set$get_values(tags = "train")

                    data = as.numeric(unlist(task$data(cols = task$target_names)))

                    invoke(gss::ssden, formula = ~data, .args = pars)

                    },

                    predict_internal = function(task){

                    newdata = as.numeric(unlist(task$data(cols = task$target_names)))

                    pars = self$param_set$get_values(tags = "predict")

                    pdf  = invoke(gss::dssden, x = newdata, object = self$model)

                    PredictionDens$new(task = task, pdf = pdf)

                    }
                    ))
