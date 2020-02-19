.DenLogspline <- function(xgrid,x, maxknots = 0, knots, nknots = 0, penalty = log(length(x))){

  fit = logspline::logspline(x, maxknots = maxknots, knots, nknots =nknots, penalty= penalty,
                  silent = TRUE, mind = -1, error.action = 2)

  pdf = function(x1){}
  body(pdf) = substitute({

    logspline::dlogspline(xgrid, f)

  }, list (f = fit))

  Distribution$new(name = "Histogram Estimator")

}



