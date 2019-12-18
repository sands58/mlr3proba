
MeasureDensityLogloss = R6::R6Class("MeasureDensityLogloss",
                                    inherit = MeasureDensity,
                                    public = list(
                                      initialize = function() {
                                        super$initialize(
                                          id = id,
                                          range = c(0, Inf),
                                          minimize = TRUE,
                                          predict_type = "distr",
                                          packages = "distr6"
                                        )

                                        assertNumeric(eps)
                                        private$.eps <- eps
                                      },

                                      score_internal = function(prediction, ...) {
                                        mean(density_logloss(prediction))
                                      }

))

