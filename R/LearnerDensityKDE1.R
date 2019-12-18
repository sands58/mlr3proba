LearnerDensityKDE = R6::R6Class("LearnerDensityKDE", inherit = LearnerDensity)
LearnerDensityKDE$set("public", "initialize", function(id = "density.KDE") {
  super$initialize(
    id = id,
    param_set = ParamSet$new(list(ParamFct$new("kernel",
                                               levels = subset(distr6::listKernels(),
                                                               select="ShortName")[[1]],
                                               tags = "train"),
                                  ParamDbl$new("bandwidth", lower = 0, tags = "train"))),
    predict_types = "prob",
    feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
    properties = "missings",
    packages = "distr6"
  )
})

LearnerDensityKDE$set("public", "train_internal", function(task){

  pdf <- function(dif){}
  body(pdf) <- substitute({
    dif <- sapply(test, function(x, y) (x-y)/bw,  y = train)
    #output: a matrix, ncol = length(test), nrow = length(train)
    if(nrow(dif) == 1)
      return(1/(length(train) * bw) * sum(kernel$pdf((dif))))
    else
      return(1/(nrow(dif) * bw) * colSums(kernel$pdf(dif)))
    #assume kernel$pdf is a function so input == dif
  }, list(bw = self$param_set$values$bandwidth,
          kernel = get(as.character(subset(distr6::listKernels(),
                                           ShortName == self$param_set$values$kernel,
                                           ClassName)))$new()))
  # why is it Gaussian?
  distribution = distr6::Distribution$new(name = "KDE Gaussian Estimate", short_name = "KDEGauss", pdf = pdf)
  set_class(list(distribution = distribution, features = task$feature_names), "density.KDE_model")

})


LearnerDensityKDE$set("public", "predict_internal", function(task){
  newdata = task$truth()
  prob = self$model$distribution$pdf(newdata)
  PredictionDensity$new(task = task, prob = prob)
})
