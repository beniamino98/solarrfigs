% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fig_acf.R
\name{fig_acf}
\alias{fig_acf}
\title{Plot the autocorrelation function of the first 3 moments}
\usage{
fig_acf(
  x,
  lag_max = 400,
  label = "x",
  caption = NULL,
  ci = 0.05,
  ci_color = "blue"
)
}
\arguments{
\item{ci_color}{}
}
\description{
Plot the autocorrelation function of the first 3 moments
}
\examples{
fig_acf(rnorm(5000), label = "\\\\tilde{Y}_t", ci_color = "black", lag_max = 100)

}
