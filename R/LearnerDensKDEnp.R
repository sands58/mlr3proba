LearnerDensKDEnp <- R6::R6Class("LearnerDensKDEnp", inherit = LearnerDens,
                    public = list(initialize = function(id = "dens.kdeNP"){
                    super$initialize(
                    id = id,
                    param_set = ParamSet$new(
                    params = list(
                    ParamDbl$new(id = "bws",  lower = 0, tags = "train"),
                    ParamFct$new("ckertype", levels = c("gaussian", "epanechnikov", "uniform"),
                                 default = "gaussian", tags = "train"),
                    ParamDbl$new(id = "ckorder", default = 2, tags = "train"),
                    ParamFct$new(id = "bwmethod", default= "cv.ml",
                                 levels = c("cv.ml", "cv.ls"," normal-reference"), tags = "train")
                    )),
                    feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
                    predict_types = "pdf",
                    packages = c("np", "distr6")
                    )},

                    train_internal = function(task){

                    pars = self$param_set$get_values(tag="train")

                    data = as.data.frame(unlist(task$data(cols = task$target_names)))

                    pdf <- function(x1){}

                    body(pdf) <- substitute({


                    invoke(.DensNp, tdat = data, edat = x1,  .args = pars)$dens

                    })

                    Distribution$new(name = paste("KDE", self$param_set$values$ckertype),
                                                          pdf = pdf)
                    },

                    predict_internal = function(task){

                    newdata = as.numeric(unlist(task$data(cols = task$target_names)))

                    PredictionDens$new(task = task, pdf = self$model$pdf(newdata))

                    }

                    ))

