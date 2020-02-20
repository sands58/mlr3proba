LearnerDensKDEnp <- R6::R6Class("LearnerDensKDEnp", inherit = LearnerDens,
                    public = list(initialize = function(id = "dens.kdeNP"){
                    super$initialize(
                    param_set = ParamSet$new(list(
                          ParamDbl$new(id = "bws",  default = 0.1,  tags = c("train", "predict")),
                        ParamFct$new(id = "ckertype", default = "gaussian",
                                      levels = c("gaussian", "epanechnikov", "uniform"),
                                               tags = "train"),
                       ParamFct$new(id = "bwmethod", default = "cv.ml",
                                    levels = c("cv.ml", "cv.ls"),
                                   tags = "train"),
                       ParamLgl$new(id = "bandwidth.compute", default = FALSE, tags = "train"),
                       ParamDbl$new(id = "ckerorder", default= 2, tags = "train"),
                      ParamFct$new(id = "bwtype", default= "fixed",
                                   levels = c("fixed", "generalized_nn", "adaptive_nn"), tags = "train"),
                      ParamLgl$new(id = "bwscaling", default= TRUE, tags = "train")
                     )),

                  #  ps$values = list(ckertype = "gaussian", ckerorder = 2)

                    id = id,
                    # param_set = ps,
                    feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
                    predict_types = "pdf",
                    packages = c("np", "distr6")
                    )},

                    train_internal = function(task){

                    data = as.numeric(unlist(task$data(cols = task$target_names)))

                    pars = self$param_set$get_values(tag="train")

                  #  pdf <- function(x1){}

                 #   body(pdf) <- substitute({

                  # np::npudens(tdat = data, edat = x1, bws = bw)$dens

                    target = task$truth()

                    invoke(np::npudensbw, dat = data, edat = target,  .args = pars)

                      # fit = invoke(.DensNp, tdat = data, .args = pars)
                      #
                      # set_class(list(distr = fit$distr, bws = bws$npudensbw), "dens.kdeNP")

                  # }, #,
                  # list(bw = self$param_set$values$bws)
                   #          ctype = self$param_set$values$ckertype,
                   #          corder = self$param_set$values$ckertype)


#
#                     Distribution$new(name = paste("KDE", self$param_set$values$ckertype),
#                     short_name = paste0("KDE",self$param_set$values$ckertype),
#                     pdf= pdf)

                    },

                    predict_internal = function(task){

                    newdata = as.numeric(unlist(task$data(cols = task$target_names)))

                    pars = self$param_set$get_values(tag="predict")

                    pdf = invoke(np::npudens, edat = newdata, bws = self$model)$dens

                    PredictionDens$new(task = task, pdf = pdf)

                    }

                    ))

