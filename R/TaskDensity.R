TaskDensity <- R6::R6Class("TaskDensity", inherit = TaskSupervised)

TaskDensity$set("public","initialize", function(id, backend, target) {
                       #libraray::function_name()
                       checkmate::assert_string(target)
                       super$initialize(id = id, task_type = "density", backend = backend, target = target)
                       type = subset(self$col_info, id == target, "type")
                       if (!(type %in% c("integer", "numeric"))) {
                         stop("Target column '%s' must be numeric", target)
                       }
                     })

TaskDensity$set("public","truth",function(rows = NULL) {
                                          #row: input_name
                       super$truth(rows)[[1L]]
                     })
                        #1L === 1
                        #[[1L]] == [[1]]
