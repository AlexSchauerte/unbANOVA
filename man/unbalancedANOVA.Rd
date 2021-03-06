% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unbalancedANOVA.R
\name{unbalancedANOVA}
\alias{unbalancedANOVA}
\title{Show differences between all ANOVA types}
\usage{
unbalancedANOVA(
  means,
  freq,
  k.levels = NULL,
  type = c("I", "II", "III", "ATE"),
  fixed = NULL
)
}
\arguments{
\item{means}{matrix of means, the columns denote the covariates and the rows the different x levels}

\item{freq}{matrix of frequencies, the columns denote the covariates and the rows the different x levels}

\item{k.levels}{numeric vector with the number of levels for all covariates (K1, K2, ...)}

\item{type}{numeric or character vector denoting the ANOVA types to be calculated (allowed values are any of the following: "I", 1, "II", 2, "III", 3, "ATE")}

\item{fixed}{numeric vector with the levels the corresponding covariates should be fixed to (NA for no fixing)}
}
\description{
This function calculates the marginal means, effects (differences in marginal means) and sum of squares of x according to the different ANOVA types. Additionaly it calculates different attributes (Is data balanced? Is data proportional? ...) and function call information (What was the means- and freq-matrix? ...)
}
\keyword{anova}
