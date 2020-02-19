.DensStats <- function(x, bw = "nrd0", kernel = "gaussian", weights = NULL){

  fit = stats::density(x = x, bw=bw, kernel = kernel, adjust = 1, window = kernel, weights = weights)

  pdf = function(x1){}
  body(pdf) = substitute({

    kernel <- if(kernel == "gaussian"){"Normal"} else(1)

    1/(length(data) * bws) * sum(kernel$pdf((x1 - x)/bws))

    }, list(bws = fit$bw))

  list(distr = distr6::Distribution$new(name = "KDE",
                   short_name = "KDE",
                   pdf = pdf),
                   dens = fit)


}
