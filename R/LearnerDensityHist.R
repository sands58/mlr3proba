LearnerDensityHist <- R6::R6Class("LearnerDensityHist", inherit = LearnerDensity,
  public = list(initialize = function(id = "density.hist"){
    super$initialize(
      id = id,
      param_set = ParamSet$new(
        params = list(
          ParamInt$new(id = "breaks", lower = 0, tags = "train"),
          ParamLgl$new(id = "include.lowest", default = TRUE, tags = "train"),
          ParamLgl$new(id = "right", default = TRUE, tags = "train"),
          ParamLgl$new(id = "freq", default = TRUE, tags = "train")
        )),
      feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
      predict_types = c("pdf","cdf"),
      packages = "distr6")},

    train_internal = function(task){

      pars = self$param_set$get_values(tag="train")

      data = as.numeric(unlist(task$data(cols = task$target_names)))

      #this is called self$model
      #using histogram1.R
      # change later histogram function name in histogram file = > .histogram
      #invoke in packge mlr3misc
      dt = invoke(.histogram, data = data, .args = pars)
      pdf = function(x1){}
      body(pdf) = substitute({
        as.numeric(unlist(data[findInterval(x1, data$Intervals), 2]))
      }, list(data = dt))

      cdf = function(x1){}
      body(cdf) = substitute({

        length_val <- length(which(data$Intervals <= x1))
        area <- rep()
        for(i in 1:(length_val - 1)){

          area[i] <- (data$Intervals[i+1] - data$Intervals[i]) * data$binPdf[i]
        }
        return(area)
      }, list(data = dt))

      distr6::Distribution$new(name = "Histogram Estimator",
                               short_name = "Histogram",
                               pdf = pdf, cdf = cdf,
                               support = distr6::Interval$new(min(data), max(data)))

    },

    predict_internal = function(task){
      newdata = as.numeric(unlist(task$data(cols = task$target_names)))
      PredictionDensity$new(task = task, pdf = self$model$pdf(newdata),
                            cdf = self$model$cdf(newdata))
    }
  ))


