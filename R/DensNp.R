.DensNp <- function(tdat, edat, bws, ckertype = "gaussian", ckeorder = 2, bwmethod = "cv.ml"){

  library(np)

  bw <- if(missing(bws)){

    np::npudensbw(dat = tdat, ckertype = ckertype, ckeorder =ckeorder,
                          bwmethod=bwmethod, bandwidth.compute = TRUE)$bw
  } else{bws}

  return(np::npudens(bws = bw, tdat = tdat, edat = edat, ckertype = ckertype, ckeorder =ckeorder))


}

