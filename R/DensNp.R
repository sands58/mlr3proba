.DensNp <- function(tdat, edat, bws, ckertype = "gaussian", ckeorder = 2, bwmethod = "cv.ml"){

  bw <- if(missing(bws)){

    np::npudensbw(dat = tdat, ckertype = ckertype, ckeorder =ckeorder,
                          bwmethod=bwmethod, bandwidth.compute = TRUE)$bw
  } else{bws}

  pdf <- function(x1){}
  body(pdf) = substitute({

  np::npudens(bws = bw, tdat = tdat, edat = edat, ckertype = ckertype, ckeorder =ckeorder)

  }, list = )
}



