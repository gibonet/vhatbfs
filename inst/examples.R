
library(vhatbfs)

str(d)

# Basic example (vector) ----
sum(d$w)
vhat3(x = d$w, mh = d$mh, Nh = d$Nh, strata = d$s)

# Confidence interval (vector) --------
sum(d$w) + c(-1, 1) *
  qnorm(0.975) *
  sqrt(
    vhat3(x = d$w, mh = d$mh, Nh = d$Nh, strata = d$s)
  )

# Variance by group, with split and lapply ------
lapply(
  split(d, f = d$xcat), 
  FUN = function(x) {
    vhat3(x = x$w, mh = x$mh, Nh = x$Nh, strata = x$s)
  }
)

split(d, f = d$xcat) |>
  lapply(
    function(x) {
      vhat3(x = x$w, mh = x$mh, Nh = x$Nh, strata = x$s)
    }
  )

