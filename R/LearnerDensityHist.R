LearnerDensityHist <- R6::R6Class("LearnerDensityHist", inherit = LearnerDensity,
  public = list(initialize = function(id = "density.Hist"){
    super$initialize(
      id = id,
      param_set = ParamSet$new(
        params = list(
        ParamUty$new(id = "breaks", default = "Sturges", tags = "train"),
          ParamLgl$new(id = "include.lowest", default = TRUE, tags = "train"),
          ParamLgl$new(id = "right", default = TRUE, tags = "train"),
          ParamDbl$new(id = "Intervals", tag = "train"),
        ParamDbl$new(id = "pdf", tag = "train")
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

      as.numeric(sapply(x1, function(x) .histogram_cdf(val = x, Intervals = data$Intervals,
                                pdf = data$binPdf)))

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


