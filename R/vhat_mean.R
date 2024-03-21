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


#' Stima della varianza della stima di una media (di uno strato)
#'
#' @param y vettore numerico
#' @param weights vettore numerico di pesi
#' @param nPSU uno scalare con il numero di osservazioni di tutto il campione
#'
#' @examples
#' vhat_mean_strata(d$w, d$w, nPSU = nrow(d))
#'
#' @export
vhat_mean_strata <- function(y, weights, nPSU = NULL) {
  ybar <- stats::weighted.mean(y, weights)

  x <- (y - ybar) * weights / sum(weights)

  if (is.null(nPSU)) nPSU <- length(x)

  scale <- nPSU / (nPSU - 1)

  as.numeric(crossprod(x * sqrt(scale)))
}


# Prova

vhat_mean <- function(y, weights, nPSU = NULL, strata) {
  strata_unique <- unique(strata)
  v_strata <- strata_weights <-
    vector(mode = "numeric", length = length(strata_unique))

  Nhat <- sum(weights)

  for (i in seq_along(v_strata)) {
    k <- strata == strata_unique[i]
    v_strata[[i]] <- vhat_mean_strata(y[k], weights[k])
    strata_weights[[i]] <- sum(weights[k]) / Nhat
  }

  res <- sum(v_strata * strata_weights^2, na.rm = TRUE)
  res
}
