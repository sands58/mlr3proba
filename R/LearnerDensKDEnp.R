LearnerDensKDEnp <- R6::R6Class("LearnerDensKDEnp", inherit = LearnerDens,
                    public = list(initialize = function(id = "dens.kdeNP"){
                     ps = ParamSet$new(list(
                          ParamDbl$new(id = "bws", default= 0.1, lower = 0, tags = "train"),
                          ParamFct$new(id = "ckertype", default = "gaussian",
                                       levels = c("gaussian", "epanechnikov", "uniform"),
                                                 tags = "train"),
                          ParamDbl$new(id = "ckerorder", default= 2, tags = "train")
                     ))

                    ps$values = list(bws = 0.1, ckertype = "gaussian", ckerorder = 2)
                    super$initialize(
                    id = id,
                    param_set = ps,
                    feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
                    predict_types = "pdf",
                    packages = c("np", "distr6")
                    )},

                    train_internal = function(task){

                    data = as.numeric(unlist(task$data(cols = task$target_names)))

                    pdf <- function(x1){}

                    body(pdf) <- substitute({

                    np::npudens(tdat = data, edat = x1, bws = bw, ckertype = ctype, ckeroder = corder)$dens

                    }, list(bw = self$param_set$values$bws,
                            ctype = self$param_set$values$ckertype,
                            corder = self$param_set$values$ckertype)
                    )


                    Distribution$new(name = paste("KDE", self$param_set$values$ckertype),
                    short_name = paste0("KDE",self$param_set$values$ckertype),
                    pdf= pdf)
                    },

                    predict_internal = function(task){

                    newdata = as.numeric(unlist(task$data(cols = task$target_names)))

                    PredictionDens$new(task = task, pdf = self$model$pdf(newdata))

                    }

                    ))

