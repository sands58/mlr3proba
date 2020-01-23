.histogram <- function(x1, data, breaks = "Sturges", include.lowest = TRUE){
  a <- graphics::hist(x = data, breaks = breaks, include.lowest = include.lowest, plot = FALSE)
  dt <- data.table::data.table(Intervals = head(a$breaks,-1), binPdf = a$density)

  pdf <-  as.numeric(unlist(dt[findInterval(x1, dt$Intervals, rightmost.closed = FALSE), 2]))

  return(pdf)
}


data = data.frame("A" = as.numeric(c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6)))
.histogram(2, unlist(data), breaks = 5)
.histogram(0.1, unlist(data), breaks = 5)
