.DensNp <- function(tdat, edat, bws, ckertype = "gaussian", ckeorder = 2, bwmethod = "cv.ml"){

 if(missing(bws)){

     bws <- np::npudensbw(dat = tdat, ckertype = ckertype, ckeorder =ckeorder,
                          bwmethod=bwmethod, bandwidth.compute = TRUE)$bw
  } else{bws == bws}

  dens <- np::npudens(bws = bws, tdat = tdat, edat = edat, ckertype = ckertype, ckeorder =ckeorder)

  return(dens)
}



