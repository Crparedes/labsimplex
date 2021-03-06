% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exampleOptim.R
\name{cntr}
\alias{cntr}
\title{Contour plot of example response surfaces}
\usage{
cntr(surface, length = 150, noise = 0, x1lim = c(278, 365), x2lim = c(0, 14))
}
\arguments{
\item{surface}{example response surface to use. See
\code{\link{exampleSurfaceR2}} and
\code{\link{exampleSurfaceR2.2pks}}.}

\item{length}{number of levels to use in each explanatory variables}

\item{noise}{absolute noise to be included in the results}

\item{x1lim}{limits for the first variable (temperature in
\code{\link{exampleSurfaceR2}} and
\code{\link{exampleSurfaceR2.2pks}})}

\item{x2lim}{limits for the second variable (pH in
\code{\link{exampleSurfaceR2}} and
\code{\link{exampleSurfaceR2.2pks}})}
}
\description{
Plots a \code{\link[ggplot2]{ggplot}} with the contour of the
bivariate example response surfaces included in the package.
}
\examples{
  p <- cntr(surface = exampleSurfaceR2, length = 200)
  print(p)
}
\references{
H. Wickham. ggplot2: Elegant Graphics for Data Analysis.
  Springer-Verlag New York, 2016.
}
\author{
Cristhian Paredes, \email{craparedesca@unal.edu.co}

Jesús Ágreda, \email{jagreda@unal.edu.co}
}
