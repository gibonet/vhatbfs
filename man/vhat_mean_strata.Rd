% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vhat_mean.R
\name{vhat_mean_strata}
\alias{vhat_mean_strata}
\title{Stima della varianza della stima di una media (di uno strato)}
\usage{
vhat_mean_strata(y, weights, nPSU = NULL)
}
\arguments{
\item{y}{vettore numerico}

\item{weights}{vettore numerico di pesi}

\item{nPSU}{uno scalare con il numero di osservazioni di tutto il campione}
}
\description{
Stima della varianza della stima di una media (di uno strato)
}
\examples{
vhat_mean_strata(d$w, d$w, nPSU = nrow(d))

}
