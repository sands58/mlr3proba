.DensNp <- function(tdat, edat, bws, ckertype = "gaussian", ckeorder = 2, bwmethod = "cv.ml"){

  if(missing(bws)){

    bws <- np::npudensbw(dat = tdat,  ckertype = ckertype, ckeorder =ckeorder,
                          bwmethod=bwmethod)$bw
    return(np::npudens(bws = bws, tdat = tdat, edat = edat, ckertype = ckertype, ckeorder =ckeorder)$dens)

  } else{

    bws <- np::npudensbw(dat = tdat,  bws = bws,
                         bandwidth.compute = FALSE)$bw
    return(np::npudens(bws = bws, tdat = tdat, edat = edat, ckertype = ckertype, ckeorder =ckeorder)$dens)
  }






}

