get_smi <- function(pop_cw, ind_cw, ind_wt, bOLS, r){
  ind_wt*(pop_cw/ind_cw)^(bOLS/r)
}
