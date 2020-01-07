LearnerDensityHist <- R6::R6Class("LearnerDensityHist", inherit = LearnerDensity,
                            public = list(initialize = function(){
                            super$initialize(
                            id = "density.Hist",
                            param_set = ParamSet$new(
                            params = list(
                            ParamDbl$new(id = "numbin", tags = "train")
                          # ParamDbl$new(id = "binwidth", tags = "trains")
                            )),
                            feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
                            predict_types = "prob",
                            properties  = "missing",
                            packages = "distr6")},

                            train_internal = function(task){

                            pars = self$param_set$get_values(tag="train")

                            data = as.vector(task$data(cols = task$feature_names))
                          # pdf <- function(data){}
                        #   body(pdf) <- substitute({
                            pdf <- function(data, pars){

                            #rearrange the data and get the maximum and minimum
                            data_max <- max(sort(data, decreasing = F))
                            data_min <- min(sort(data, decreasing = F))
                            # finding the difference between minimum data and the maximum data
                            data_dif <- data_max - data_min
                            #finding the binwidth of the data
                            BinWidth <- data_dif/pars
                            #getting the intervals of the of the bins
                            Index_num_bin <- c(1:pars)
                            Interval <- sapply(BinWidth, function(x, y) data_min + y*x,
                                          y = Index_num_bin)
                            XIntervals <- c(data_min, Interval)
                           #Count the number of training data in each interval
                            Bin <- table(cut(sort(task, decreasing = F),
                                             XIntervals, include.lowest = T))
                            #find the probability the data lies inside the bin
                            ProbBin <- Bin/(length(data)*BinWidth)
                            # finding the which bin the data belongs to
                            TrainIntervals <- as.vector(findInterval(data, XIntervals,
                                                                     rightmost.closed = TRUE))
                            # density of the data
                            histPdf <- sapply(TrainIntervals, function(x) ProbBin[[x]])
                            return(list(ProbBin = ProbBin, Pdf = HistPdf))
                             }

                            fit = invoke(pdf, data = data, .args = pars)

                            set_class(fit = fit, "Density.Hist")

                            }

                      #      predict_internal = function(task){

                      #       pars = self$param_set$get_values(tags = "predict")
                      #       newdata_Intervals = as.vector(findInterval(newdata, self$XIntervals,
                      #                                    rightmost.closed= TRUE))
                      #       test_pdf = sapply(newdata_Intervals, function(x) self$ProbBin[[x]]/self$BinWidth)}


  ))


