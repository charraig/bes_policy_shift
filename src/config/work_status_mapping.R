grades <- c(-1,-2,-3,0,0,-3,NA_integer_,
            -1,-2,0,0,-2,NA_integer_,
            -1,0,0,-1,NA_integer_,
            1,1,0,NA_integer_,
            0,-1,NA_integer_,
            -1,NA_integer_,
            NA_integer_)
blankmat <- matrix(0,8,8)
blankmat[lower.tri(blankmat, diag=FALSE)] <- grades
mat <- t(blankmat)
shift_mapping <- mat + -1*blankmat
shift_mapping
work_status_comb <- tibble::tibble(
  t0 = rep(1:8, times=8),
  t1 = rep(1:8, each=8),
  work_status_shift = as.vector(shift_mapping)
)