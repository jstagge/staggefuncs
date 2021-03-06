% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gof_bootstrap.R
\name{gof_stats}
\alias{gof_stats}
\title{Goodness of Fit Statistics for Univariate Distributions with Bootstrapping}
\usage{
gof_stats(x, dist_name, param_list)
}
\arguments{
\item{x}{vector of observations}

\item{dist_name}{name of univariate distribution}

\item{param_list}{list with parameter estimates for the univariate probability distribution}
}
\value{
gof_results list with KS, AD, and CVM test statistic
}
\description{
Calculates goodness of fit statatistics (K-S, AD, and CVM) for a chosen univariate distribution.
}
