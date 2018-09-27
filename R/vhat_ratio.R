
vhat_ratio_strata <- function(numerator, denominator, weights, nh = NULL){
  Rhat <- sum(numerator * weights) / sum(denominator * weights)
  
  r <- (numerator - Rhat * denominator) / sum(denominator * weights)
  
  x <- as.matrix(r * weights)
  
  if(is.null(nh)){
    nh <- NROW(x)
  }
  nPSU <- nh[1]   # unique(nh)[1]
  f <- rep(1, NROW(x))
  scale <- f * nPSU / (nPSU - 1)
  x <- sweep(x, 2, colMeans(x), "-")
  
  v <- crossprod(x * sqrt(scale))
  v
}


vhat_ratio <- function(numerator, denominator, weights, nh = NULL, strata){
  strata_unique <- unique(strata)
  v_strata <- vector(mode = "numeric", length = length(strata_unique))
  
  for(i in seq_along(v_strata)){
    k <- strata == strata_unique[i]
    v_strata[[i]] <- vhat_ratio_strata(numerator[k], denominator[k], weights[k], nh[k])
  }
  
  res <- sum(v_strata)
  
  res
}