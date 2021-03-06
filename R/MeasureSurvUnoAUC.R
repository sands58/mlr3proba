#' @template surv_measure
#' @templateVar title Uno's AUC
#' @templateVar fullname MeasureSurvUnoAUC
#'
#' @description
#' Calls [survAUC::AUC.uno()].
#'
#' Assumes random censoring.
#'
#' @template measure_survAUC
#' @template param_integrated
#' @template param_times
#'
#' @references
#' \cite{mlr3proba}{uno_2007}
#'
#' @family AUC survival measures
#' @family lp survival measures
#' @export
MeasureSurvUnoAUC = R6Class("MeasureSurvUnoAUC",
  inherit = MeasureSurvAUC,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(integrated = TRUE, times) {
      super$initialize(integrated = integrated,
                       times = times,
                       id = "surv.unoAUC",
                       properties = c("requires_task", "requires_train_set"))
    }
  ),

  private = list(
    .score = function(prediction, task, train_set, ...) {
      super$.score(prediction = prediction,
                           task = task,
                           train_set = train_set,
                           FUN = survAUC::AUC.uno,
                           ...)
    }
  )
)
