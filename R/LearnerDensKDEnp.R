LearnerDensKDEnp <- R6::R6Class("LearnerDensKDEnp", inherit = LearnerDens,
                    public = list(initialize = function(id = "dens.KDE.np"){
                    super$initialize(
                    id = id,
                    param_set = ParamSet$new(
                    params = list(
                    ParamDbl$new(id = "bws",  tags = "train"),
                    ParamFct$new("ckertype", tags = "train",
                                 levels = subset(distr6::listKernels(),
                                                 select="ShortName")[[1]]))),
                    feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
                    predict_types = c("pdf","cdf"),
                    packages = c("np", "distr6")
                    )},

                    train_internal = function(task){


                    pars = self$param_set$get_values(tag="train")

                    data = as.data.frame(unlist(task$data(cols = task$target_names)))

                    pdf <- function(x1){}

                    body(pdf) <- substitute({

                    invoke(np::npudens, tdat = data, edat = x1, .args = pars)$estimate

                    })


                    list(distr = distr6::Distribution$new(name = paste("Kernel", self$param_set$values$ckertype),
                                                          short_name = paste0("KDE",self$param_set$values$ckertype),
                                                          pdf = pdf))
                    },

                    predict_internal = function(task){

                    newdata = as.numeric(unlist(task$data(cols = task$target_names)))

                    PredictionDens$new(task = task, pdf = self$model$distr$pdf(newdata))

                                  }
                                ))

