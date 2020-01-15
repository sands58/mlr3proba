MeasureDensLogloss = R6::R6Class("MeasureDensLogloss",
                            inherit = MeasureDens,
                            public = list(
                              initialize = function() {
                                super$initialize(
                                  id = "dens.logloss",
                                  range = c(0, Inf),
                                  minimize = TRUE,
                                  predict_type = "prob"
                          #        task_properties = "twoclass",
                          #        packages = "Metrics"
                                )
                              },

                              score_internal = function(prediction, ...) {
                                return(mean(-log(prediction$prob)))
                              }
                            )
)
