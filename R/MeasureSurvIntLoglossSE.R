#' @template surv_measure
#' @templateVar title Standard Error of Integrated Log loss
#' @templateVar fullname MeasureSurvIntLoglossSE
#'
#' @description
#' Calculates the standard error of [MeasureSurvIntLogloss].
#'
#' @template learner_integratedSE
#' @template param_integrated
#' @template param_times
#' @template param_eps
#' @template field_eps
#'
#' @references
#' \cite{mlr3proba}{graf_1999}
#'
#' @family Probabilistic survival measures
#' @family distr survival measures
#' @export
MeasureSurvIntLoglossSE = R6::R6Class("MeasureSurvIntLoglossSE",
    inherit = MeasureSurvIntegrated,
    public = list(
      #' @description
      #' Creates a new instance of this [R6][R6::R6Class] class.
      initialize = function(integrated = TRUE, times, eps = 1e-15) {
        super$initialize(
          integrated = integrated,
          times = times,
          id = "surv.intloglossSE",
          range = c(0, Inf),
          minimize = TRUE,
          packages = "distr6",
          predict_type = "distr",
          properties = character()
        )

        assertNumeric(eps)
        private$.eps <- eps
      }
    ),

    active = list(
      eps = function(eps){
        if(missing(eps))
          return(private$.eps)
        else {
          assertNumeric(eps)
          private$.eps <- eps
        }
      }
    ),

    private = list(
      .eps = numeric(0),
      .score = function(prediction, ...) {
        integrated_se(score = weighted_logloss(truth = prediction$truth,
                                               distribution = prediction$distr,
                                               times = self$times,
                                               eps = self$eps),
                      integrated = self$integrated)
      }
    )
)
