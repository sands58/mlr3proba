
newdata_Intervals = as.numeric(findInterval(newdata, a$Intervals,
                                            rightmost.closed= TRUE, left.open = F))
num_zero <- length(which(newdata_Intervals %in% !a$numBin))
ifelse(newdata_Intervals %in% a$numBin, c(rep(0, num_zero),a$binPdf[newdata_Intervals]),
       rep(0, length(newdata_Intervals)))


cdf_histogram <- function(pdf, upper_limit){


  length_upper_limit <- length(which(a$Intervals <= upper_limit))
  sum_upper_limit <- length_upper_limit - pdf$Intervals[1]
  cdf <- sum_upper_limit * a$bi

}

cdf_histogram <- function(pdf, upper_limit){

        length_upper_limit <- length(which(a$Intervals <= 6.5))

        area <- rep()
        for(i in 1:(length_upper_limit - 1)){

        area[i] <- (a$Intervals[i+1] - a$Intervals[i]) * a$binPdf[i]

        }
        return(sum(area))
}
