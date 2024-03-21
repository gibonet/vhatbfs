vhat_ratio_strata <- function(numerator, denominator, weights, nh = NULL) {
  Rhat <- sum(numerator * weights) / sum(denominator * weights)

  r <- (numerator - Rhat * denominator) / sum(denominator * weights)

  x <- as.matrix(r * weights)

  if (is.null(nh)) {
    nh <- NROW(x)
  }
  nPSU <- nh[1] # unique(nh)[1]
  f <- rep(1, NROW(x))
  scale <- f * nPSU / (nPSU - 1)
  x <- sweep(x, 2, colMeans(x), "-")

  v <- crossprod(x * sqrt(scale))
  v
}

# with(d, vhat_ratio_strata(numerator = (s == 1),
#                           denominator = (s %in% c(1, 2)),
#                           weights = w)
# )

vhat_ratio <- function(numerator, denominator, weights, nh = NULL, strata) {
  strata_unique <- unique(strata)
  v_strata <- strata_weights <-
    vector(mode = "numeric", length = length(strata_unique))

  Nhat <- sum(weights)

  for (i in seq_along(v_strata)) {
    k <- strata == strata_unique[i]
    v_strata[[i]] <- vhat_ratio_strata(numerator[k], denominator[k], weights[k], nh[k])
    strata_weights[[i]] <- sum(weights[k]) / Nhat
  }

  res <- sum(v_strata * strata_weights^2, na.rm = TRUE)

  res
}

# d$numeratore <- (d$s == 1)
# d$denominatore <- (d$s %in% c(1, 2))
# with(d, vhat_ratio(numerator = numeratore,
#                    denominator = denominatore,
#                    weights = w,
#                    strata = xcat))

# with(d, vhat_ratio(numerator = (s == 1),
#                    denominator = (s %in% c(1, 2)),
#                    weights = w,
#                    strata = xcat)
# )
