LearnerDensityHist <- R6::R6Class("LearnerDensityHist", inherit = LearnerDensity,
  public = list(initialize = function(){
    super$initialize(
      id = "density.Hist",
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

      data = as.vector(task$data())

      #this is called self$model
      #using histogram1.R
      # change later histogram function name in histogram file = > .histogram
      #invoke in packge purr
      dt = invoke(.histogram, data = data, .args = pars)
      pdf = function(x1){}
      body(pdf) = substitute({
        as.numeric(unlist(data[findInterval(x1, data$Intervals), 2]))
      }, list(data = dt))

      cdf = function(val){}
      body(cdf) = substitute({

        length_val <- length(which(data$Intervals <= val))
        area <- rep()
        for(i in 1:(length_val - 1)){

          area[i] <- (data$Intervals[i+1] - data$Intervals[i]) * data$binPdf[i]
        }
      }, list(data = dt))

      distr6::Distribution$new(name = "Histogram Estimator",
                               short_name = "Histogram",
                               pdf = pdf, cdf = cdf,
                               support = Interval$new(min(truth), max(truth)))

    },

    predict_internal = function(task){
      newdata = task$data()
      PredictionDensity$new(task = task, pdf = self$model$pdf(newdata),
                            cdf = self$model$cdf(newdata))
    }
  ))


