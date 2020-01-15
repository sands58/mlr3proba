#' @title Density Task
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Task]/[TaskSupervised].
#'
#' @description
#' This task specializes [Task] and [TaskSupervised] for density estimation problems.
#' The target column is assumed to be numeric.
#' The `task_type` is set to `"density"`.
#'
#' Predefined tasks are stored in the [mlr3misc::Dictionary] [mlr_tasks].
#'
#' @section Construction:
#' ```
#' t = TaskDens$new(id, backend, target)
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier for the task.
#'
#' * `backend` :: ([DataBackend] | `data.frame()` | ...)\cr
#'   Either a [DataBackend], or any object which is convertible to a DataBackend with `as_data_backend()`.
#'   E.g., a `data.frame()` will be converted to a [DataBackendDataTable].
#'
#' * `target` :: `character(1)`\cr
#'   Name of the target column.
#'
#' @section Fields:
#' See [TaskSupervised].
#'
#' @section Methods:
#' See [TaskSupervised].
#'
#' @family Task
#' @export
#' @examples
#' task = TaskDens$new("precip", backend = data.frame(target = precip), target = "target")
#' task$task_type
#' task$truth()
TaskDens <- R6::R6Class("TaskDens", inherit = TaskSupervised)
TaskDens$set("public","initialize", function(id, backend, target) {
                       #libraray::function_name()
                       checkmate::assert_string(target)
                       super$initialize(id = id, task_type = "dens", backend = backend, target = target)
                       type = subset(self$col_info, id == target, "type")
                       if (!(type %in% c("integer", "numeric"))) {
                         stop("Target column '%s' must be numeric", target)
                       }
                     })

TaskDens$set("public","truth",function(rows = NULL) {
                                          #row: input_name
                       super$truth(rows)[[1L]]
                     })
                        #1L === 1
                        #[[1L]] == [[1]]
