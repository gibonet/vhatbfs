
library(vhatbfs)

str(d)

sum(d$w)
vhat3(x = d$w, mh = d$mh, Nh = d$Nh, strata = d$s)

sum(d$w) + c(-1, 1) *
  qnorm(0.975) *
  sqrt(
    vhat3(x = d$w, mh = d$mh, Nh = d$Nh, strata = d$s)
  )

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

