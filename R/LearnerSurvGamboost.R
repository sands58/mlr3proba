#' @template surv_learner
#' @templateVar title Gradient Boosting for Additive Models
#' @templateVar fullname LearnerSurvGamboost
#' @templateVar caller [mboost::gamboost()]
#' @templateVar distr by [mboost::survFit()] which assumes a PH fit with a Breslow estimator
#' @templateVar lp by [mboost::predict.mboost()]
#'
#' @template learner_boost
#' @description
#' The only difference between [LearnerSurvGamboost] and [LearnerSurvMboost] is that the latter function
#' allows one to specify default degrees of freedom for smooth effects specified via
#' \code{baselearner = "bbs"}. In all other cases, degrees of freedom need to be set manually via a
#' specific definition of the corresponding base-learner.
#'
#' @references
#' \cite{mlr3proba}{buehlmann_2003}
#'
#' \cite{mlr3proba}{buehlmann_2007}
#'
#' \cite{mlr3proba}{kneib_2008}
#'
#' \cite{mlr3proba}{schmid_2008}
#'
#' \cite{mlr3proba}{hothorn_2010}
#'
#' \cite{mlr3proba}{hofner_2012}
#'
#' @export
#' @examples
#' library(mlr3)
#' task = tgen("simsurv")$generate(20)
#' learner = lrn("surv.gamboost")
#' learner$param_set$values = mlr3misc::insert_named(learner$param_set$values,
#'      list(dfbase = 3, center = TRUE, baselearner = "bols"))
#' resampling = rsmp("cv", folds = 2)
#' resample(task, learner, resampling)
LearnerSurvGamboost = R6Class("LearnerSurvGamboost", inherit = LearnerSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamFct$new(id = "family", default = "coxph",
                       levels = c("coxph", "weibull", "loglog", "lognormal", "gehan",
                                  "custom"), tags = "train"),
          ParamUty$new(id = "nuirange", default = c(0, 100), tags = "train"),
          ParamUty$new(id = "custom.family", tags = "train"),
          ParamUty$new(id = "offset", tags = "train"),
          ParamLgl$new(id = "center", default = TRUE, tags = "train"),
          ParamInt$new(id = "mstop", default = 100L, lower = 0L, tags = "train"),
          ParamDbl$new(id = "nu", default = 0.1, lower = 0, upper = 1, tags = "train"),
          ParamFct$new(id = "risk", levels = c("inbag", "oobag", "none"), tags = "train"),
          ParamLgl$new(id = "stopintern", default = FALSE, tags = "train"),
          ParamLgl$new(id = "trace", default = FALSE, tags = "train"),
          ParamUty$new(id = "oobweights", tags = "train"),
          ParamFct$new(id = "baselearner", default = "bbs",
                       levels = c("bbs", "bols", "btree"), tags = "train"),
          ParamInt$new(id = "dfbase", default = 4, lower = 0, tags = "train")
        )
      )

      ps$values = list(family = "coxph")

      super$initialize(
        id = "surv.gamboost",
        param_set = ps,
        feature_types = c("integer", "numeric", "factor", "logical"),
        predict_types = c("distr","crank","lp"),
        # properties = "weights",
        packages = c("mboost","distr6","survival")
      )
    }
  ),

  private = list(
    .train = function(task) {

      pars = self$param_set$get_values(tags = "train")

      # Save control settings and return on exit
      saved_ctrl = mboost::boost_control()
      on.exit(invoke(mboost::boost_control, .args = saved_ctrl))
      is_ctrl_pars = (names(pars) %in% names(saved_ctrl))

      # ensure only relevant pars passed to fitted model
      if (any(is_ctrl_pars)) {
        pars$control = do.call(mboost::boost_control, pars[is_ctrl_pars])
        pars = pars[!is_ctrl_pars]
      }

      # convert data to model matrix
      # x = model.matrix(~., as.data.frame(task$data(cols = task$feature_names)))

      # if ("weights" %in% task$properties)
      #   pars$weights = task$weights$weight

      family = switch(pars$family,
                      coxph = mboost::CoxPH(),
                      weibull = mboost::Weibull(nuirange = pars$nuirange),
                      loglog = mboost::Loglog(nuirange = pars$nuirange),
                      lognormal = mboost::Lognormal(nuirange = pars$nuirange),
                      gehan = mboost::Gehan(),
                      custom = pars$custom.family
      )

      pars = pars[!(names(pars) %in% c("family", "nuirange", "custom.family"))]

      with_package("mboost", {
        invoke(mboost::gamboost, formula = task$formula(task$feature_names),
               data = task$data(), family = family, .args = pars)
      })
    },

    .predict = function(task) {
      newdata = task$data(cols = task$feature_names)
      # predict linear predictor
      lp = as.numeric(invoke(predict, self$model, newdata = newdata, type = "link"))

      # predict survival
      surv = invoke(mboost::survFit, self$model, newdata = newdata)
      surv$cdf = 1 - surv$surv

      # define WeightedDiscrete distr6 object from predicted survival function
      x = rep(list(data = data.frame(x = surv$time, cdf = 0)), task$nrow)
      for(i in 1:task$nrow)
        x[[i]]$cdf = surv$cdf[, i]

      distr = distr6::VectorDistribution$new(distribution = "WeightedDiscrete", params = x,
                                             decorators = c("CoreStatistics", "ExoticStatistics"))

      PredictionSurv$new(task = task, crank = lp, distr = distr, lp = lp)
    }
  )
)
