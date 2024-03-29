% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vhat.R
\name{vhat_strata}
\alias{vhat_strata}
\title{Estimated variance of a total estimation for a strata}
\usage{
vhat_strata(data, INDEX, weights)
}
\arguments{
\item{data}{a data frame of individual data. In this version, data should be
the complete dataset of the sample.}

\item{INDEX}{a right hand formula whose result is a logical vector that
indicates the observations for which the variance has to be estimated
(see the examples below)}

\item{weights}{character string with the name of the column of weights}
}
\description{
Estimated variance of a total estimation for a strata
}
\examples{
str(d)

vhat_strata(d, INDEX = ~ xcat == "cat1", weights = "w")

}
