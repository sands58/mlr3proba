#' @title Gradient Boosting with Regression Trees Survival Learner
#'
#' @usage NULL
#' @aliases mlr_learners_surv.blackboost
#' @format [R6::R6Class()] inheriting from [LearnerSurv].
#' @include LearnerSurv.R
#'
#' @section Construction:
#' ```
#' LearnerSurvBlackboost$new()
#' mlr_learners$get("surv.blackboost")
#' lrn("surv.blackboost")
#' ```
#'
#' @description
#' Gradient boosting for optimizing arbitrary loss functions where regression trees are
#' utilized as base-learners.
#' Calls [mboost::blackboost()] from package \CRANpkg{mboost}.
#'
#' @details
#' The \code{distr} return type is composed by using[mboost::survFit()] which assumes a PH fit with
#' a Breslow estimator.
#' The \code{crank} return type is defined by the expectation of the survival distribution. \cr
#' The \code{lp} return type is given by [mboost::predict.blackboost()].
#'
#' If the value given to the \code{Family} parameter is "custom.family" then an object of class
#' [mboost::Family()] needs to be passed to the \code{custom.family} parameter.
#'
#' @references
#' Peter Buehlmann and Torsten Hothorn (2007),
#' Boosting algorithms: regularization, prediction and model fitting.
#' Statistical Science, 22(4), 477–505.
#'
#' Torsten Hothorn, Kurt Hornik and Achim Zeileis (2006).
#' Unbiased recursive partitioning: A conditional inference framework.
#' Journal of Computational and Graphical Statistics, 15(3), 651–674.
#'
#' Yoav Freund and Robert E. Schapire (1996), Experiments with a new boosting algorithm.
#' In Machine Learning: Proc. Thirteenth International Conference, 148–156.
#'
#' Jerome H. Friedman (2001), Greedy function approximation: A gradient boosting machine.
#' The Annals of Statistics, 29, 1189–1232.
#'
#' Greg Ridgeway (1999), The state of boosting.
#' Computing Science and Statistics, 31, 172–181.
#'
#' @export
#' @template seealso_learner
#' @examples
#' library(mlr3)
#' task = tgen("simsurv")$generate(200)
#' learner = lrn("surv.blackboost")
#' resampling = rsmp("cv", folds = 3)
#' resample(task, learner, resampling)
LearnerSurvBlackboost = R6Class("LearnerSurvBlackboost", inherit = LearnerSurv,
                            public = list(
                              initialize = function() {
                                super$initialize(
                                  id = "surv.blackboost",
                                  param_set = ParamSet$new(
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
                                      ParamFct$new(id = "teststat", default = "quadratic", levels = c("quadratic", "maximum"), tags = "train"),
                                      ParamFct$new(id = "splitstat", default = "quadratic", levels = c("quadratic", "maximum"), tags = "train"),
                                      ParamLgl$new(id = "splittest", default = FALSE, tags = "train"),
                                      ParamFct$new(id = "testtype", default = "Bonferroni",
                                                   levels = c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic"), tags = "train"),
                                      ParamInt$new(id = "maxpts", default = 25000L, lower = 1L, tags = "train"),
                                      ParamDbl$new(id = "abseps", default = 0.001, tags = "train"),
                                      ParamDbl$new(id = "releps", default = 0, tags = "train"),
                                      ParamUty$new(id= "nmax", tags = "train"),
                                      ParamDbl$new(id = "alpha", default = 0.05, lower = 0, upper = 1, tags = "train"),
                                      ParamDbl$new(id = "mincriterion", default = 0.95, lower = 0, upper = 1, tags = "train"),
                                      ParamDbl$new(id = "logmincriterion", default = log(0.95), upper = 0, tags = "train"),
                                      ParamInt$new(id = "minsplit", default = 20L, lower = 0L, tags = "train"),
                                      ParamInt$new(id = "minbucket", default = 7L, lower = 0L, tags = "train"),
                                      ParamDbl$new(id = "minprob", default = 0.01, lower = 0, upper = 1, tags = "train"),
                                      ParamLgl$new(id = "stump", default = FALSE, tags = "train"),
                                      ParamLgl$new(id = "lookahead", default = FALSE, tags = "train"),
                                      ParamLgl$new(id = "MIA", default = FALSE, tags = "train"),
                                      ParamInt$new(id = "nresample", default = 9999L, lower = 1L, tags = "train"),
                                      ParamDbl$new(id = "tol", default = sqrt(.Machine$double.eps), lower = 0, tags = "train"),
                                      ParamInt$new(id = "maxsurrogate", default = 0L, lower = 0L, tags = "train"),
                                      ParamInt$new(id = "mtry", lower = 0L, tags = "train"),
                                      ParamInt$new(id = "maxdepth", lower = 0L, tags = "train"),
                                      ParamLgl$new(id = "multiway", default = FALSE, tags = "train"),
                                      ParamInt$new(id = "splittry", default = 2L, lower = 1L, tags = "train"),
                                      ParamLgl$new(id = "intersplit", default = FALSE, tags = "train"),
                                      ParamLgl$new(id = "majority", default = FALSE, tags = "train"),
                                      ParamLgl$new(id = "caseweights", default = TRUE, tags = "train")
                                    )
                                  ),
                                  feature_types = c("integer", "numeric", "factor"),
                                  predict_types = c("distr","crank","lp"),
                                  packages = c("mboost","distr6","survival","partykit","mvtnorm")
                                )
                                self$param_set$add_dep("nuirange", "family", CondAnyOf$new(c("weibull", "loglog", "lognormal")))
                                self$param_set$add_dep("custom.family", "family", CondAnyOf$new(c("custom")))
                              },

                              train_internal = function(task) {

                                pars = self$param_set$get_values(tags = "train")

                                # mboost control
                                # Save control settings and return on exit
                                saved_ctrl = mboost::boost_control()
                                is_ctrl_pars = (names(pars) %in% names(saved_ctrl))
                                # ensure only relevant pars passed to fitted model
                                if (any(is_ctrl_pars)) {
                                  pars$control = do.call(mboost::boost_control, pars[is_ctrl_pars])
                                  pars = pars[!is_ctrl_pars]
                                }

                                # GenzBretz control
                                # Save control settings and return on exit
                                saved_ctrl = mvtnorm::GenzBretz()
                                is_ctrl_pars = (names(pars) %in% names(saved_ctrl))
                                # ensure only relevant pars passed to fitted model
                                if (any(is_ctrl_pars)) {
                                  pars$pargs = do.call(mvtnorm::GenzBretz, pars[is_ctrl_pars])
                                  pars = pars[!is_ctrl_pars]
                                }

                                # ctree control
                                # Save control settings and return on exit
                                saved_ctrl = partykit::ctree_control()
                                is_ctrl_pars = (names(pars) %in% names(saved_ctrl))
                                # ensure only relevant pars passed to fitted model
                                if (any(is_ctrl_pars)) {
                                  pars$tree_controls = do.call(partykit::ctree_control, pars[is_ctrl_pars])
                                  pars = pars[!is_ctrl_pars]
                                }

                                # if ("weights" %in% task$properties)
                                #   pars$weights = task$weights$weight

                                if(length(pars$family) == 0)
                                  pars$family = "coxph"
                                if(length(pars$nuirange) == 0)
                                  pars$nuirange = c(0, 100)

                                family = switch(pars$family,
                                                coxph = mboost::CoxPH(),
                                                weibull = mboost::Weibull(nuirange = pars$nuirange),
                                                loglog = mboost::Loglog(nuirange = pars$nuirange),
                                                lognormal = mboost::Lognormal(nuirange = pars$nuirange),
                                                gehan = mboost::Gehan(),
                                                custom = pars$custom.family
                                )

                                pars = pars[!(names(pars) %in% c("family", "nuirange", "custom.family"))]

                                invoke(mboost::blackboost, formula = task$formula(task$feature_names),
                                       data = task$data(), family = family, .args = pars)
                              },

                              predict_internal = function(task) {
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

                                crank = as.numeric(sapply(x, function(y) sum(y[,1] * c(y[,2][1], diff(y[,2])))))

                                PredictionSurv$new(task = task, crank = crank, distr = distr, lp = lp)
                              }
                            )
)