.DenLogspline <- function(x, knots){

  fit = logspline::oldlogspline(x, knots = knots)

  pdf = function(x1){}

  body(pdf) = substitute({

    logspline::doldlogspline(x1,  f)

  }, list (f = fit))


  list(distr = distr6::Distribution$new(name = "Penalized Density",
                                        short_name = "PenalizedDens",
                                        pdf = pdf))

}



