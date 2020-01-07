hist_logloss = function(new_data, distribution){

  bins = distribution$binPdf
  interval = distribution$Intervals
  new_interval <-  as.vector(findInterval(traindata,
                                          interval,
                                          rightmost.closed = T))
  predict_pdf = sapply(new_interval, function(x) bins[[x]])
  return(-log(predict_pdf))

}
