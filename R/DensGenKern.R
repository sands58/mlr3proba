.DensGenKern <- function(x, range.x, xbandwidth){

    return(sapply(range.x, function(y) GenKern::KernSec(x = x, xbandwidth = xbandwidth,
                                            range.x = y)$yden))



}
