
# Stima della varianza della stima di una media (di uno strato)

#' Estimate the variance of a mean estimate (of a strata)
#'
#' @param y numeric vector
#' @param weights numeric vector of weights
#' @param nPSU number of observations of the sample (default: NULL)
#' @param na.rm a logical value indicating whether NA values in y should be 
#'   stripped before the computation proceeds. Default: TRUE
#' @param fpc finite population correction. Default: TRUE
#'
#' @examples
#' vhat_mean_strata(d$w, d$w, nPSU = nrow(d))
#'
#' @export
vhat_mean_strata <- function(y, weights, nPSU = NULL, na.rm = TRUE, fpc = TRUE) {
  if (missing(weights)) weights <- rep(1, length(y))
  
  if (na.rm) {
    k <- is.na(y) | is.na(weights)
    y <- y[!k]
    weights <- weights[!k]
  }
  
  ybar <- stats::weighted.mean(y, weights)

  x <- (y - ybar) * weights / sum(weights)

  if (is.null(nPSU)) nPSU <- length(x)

  if (fpc) scale <- nPSU / (nPSU - 1) else scale <- 1L
  
  as.numeric(crossprod(x * sqrt(scale)))
}


# Prova
# nPSU is not used at the moment. Should it be different for each strata?
vhat_mean <- function(y, weights, nPSU = NULL, strata = NULL) {
  stopifnot(length(y) == length(weights))
  
  if (is.null(strata)) strata <- rep(1, length(y))
  
  strata_unique <- unique(strata)
  v_strata <- strata_weights <-
    vector(mode = "numeric", length = length(strata_unique))

  Nhat <- sum(weights)

  for (i in seq_along(v_strata)) {
    k <- strata == strata_unique[i]
    
    v_strata[[i]] <- vhat_mean_strata(y[k], weights[k])  # , nPSU = nPSU
    
    strata_weights[[i]] <- sum(weights[k]) / Nhat
  }

  res <- sum(v_strata * strata_weights^2, na.rm = TRUE)
  res
}

# vhat_mean(d$w, d$w, strata = NULL)
# vhat_mean(d$w, d$w, strata = d$s)




# Old stuff -------
# Come vhat_strata5 ma per la stima della varianza di una media (* 1 / Nhat^2)
# No non va bene, devo includere anche una variabile numerica y (tipo l'affitto),
# oltre al peso (che al momento è x)
# vhat_mean_strata <- function(x, mh, Nh){
#   k <- length(x)
#   mh <- unique(mh)
#   Nh <- unique(Nh)
#   stopifnot(length(mh) == 1 || length(Nh) == 1)
#   Nhat <- sum(x)
#   primo_termine <- (mh - k) * (Nhat/mh)^2
#   secondo_termine <- sum((x - Nhat/mh)^2)
#   termine <- primo_termine + secondo_termine
#   res <- mh/(mh - 1) * (1 - mh/Nh) * termine
#   res / Nhat^2
# }

# Provo una variante senza fpc
# vhat_mean_strata2 <- function(y, weights){
#   ybar <- stats::weighted.mean(y, weights)
#   Nhat <- sum(weights)
#
#   sigma2 <- sum(weights * (y - ybar)^2) / Nhat # * Nhat / (Nhat - 1)
#   sigma2 / Nhat^2
# }
