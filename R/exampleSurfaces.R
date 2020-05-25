#' Multivariate functions that define hypothetical response surfaces.
#'
#' The functions in this section simulate the yield of hypothetical chemical
#' reactions as a function of temperature, pH, and concentration (the latter
#' only for \code{exampleSurfaceR3()}). Those functions are useful to
#' illustrate most concepts of the simplex optimization algorithms
#' implemented in the \code{labsimplex} package, as shown in the vignentte of
#' the package. This vignette can be visualized by running
#' \code{vignette('labsimplex')}.
#'
#' Parameters \code{x1}, \code{x2}, and \code{x3} may be supplied as
#' vectors in which case all must have the same length. \cr
#' Boundary values are proposed consistently with real-life limitations
#' in aqueous media. If such boundaries are violated in the variables
#' input, a negative result without physical meaning is returned. This
#' negative value represents an \emph{infinitely bad response} that will
#' force the simplex to move in another direction.
#'
#' @name ExampleSurfaces
#' @aliases exampleSurfaceR2.2pks
#' @aliases exampleSurfaceR2
#' @aliases exampleSurfaceR3
#' @param  x1    temperature in Kelvin. Numeric between 278 and 365.
#' @param  x2    pH. Numeric between 0 and 14.
#' @param  x3    concentration in arbitrary units. Numeric between 0 and 1.
#'               Only used in \code{exampleSurfaceR3()}.
#' @param  noise absolute noise included in the response surface result.
#'               Default to zero.
#' @seealso \code{\link{cntr}}, \code{\link{prspctv}} and
#' \code{\link{exampleOptimization}}
#' @rdname ExampleSurfaces
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Jesús Ágreda, \email{jagreda@@unal.edu.co}
#' @return \code{exampleSurfaceR2(x1, x2, noise = 0)} defines a response
#'   surface with one maximum at pH 10 and 300 K.
#' @examples
#'   exampleSurfaceR2(x1 = 320, x2 = 4.5)
#'   exampleSurfaceR2(x1 = c(310, 320), x2 = c(4.5, 5.8))
#'   exampleSurfaceR2(x1 = c(310, 320), x2 = c(4.5, 5.8), noise = 5)
#' @export
exampleSurfaceR2 <- function (x1, x2, noise = 0) {
  if (length(x1) != length(x2)) {
    stop('Vector parameters x1 and x2 must have same length')
  }
  if (any(any(x1 > 365), any(x1 < 278), any(x2 > 14), any(x2 < 0))) return(-1)
  return(94 * (exp(-(0.03*(x2 - 10)^2 + 0.001 * (x1 - 300)^2))) +
           rnorm(length(x1), 0, noise))
}

#' @rdname ExampleSurfaces
#' @return \code{exampleSurfaceR2.2pks(x1, x2, noise = 0)} defines a response
#'   surface with global and local maxima at pH 10 and 300 K and pH 4.5 and
#'   340 K, respectively.
#' @examples
#'   exampleSurfaceR2.2pks(x1 = 320, x2 = 4.5)
#'   exampleSurfaceR2.2pks(x1 = c(310, 320), x2 = c(4.5, 5.8))
#'   exampleSurfaceR2.2pks(x1 = c(310, 320), x2 = c(4.5, 5.8), noise = 5)
#' @export
exampleSurfaceR2.2pks <- function (x1, x2, noise = 0) {
  if (length(x1) != length(x2)) {
    stop('Vector parameters x1 and x2 must have same length')
  }
  if (any(any(x1 > 365), any(x1 < 278), any(x2 > 14), any(x2 < 0))) return(-1)
  return(80 * exp(-(0.05*(x2 - 4.5)^2 + 0.0025 * (x1 - 340)^2)) +
         94 * (exp(-(0.035*(x2 - 10)^2 + 0.002 * (x1 - 300)^2))) +
         rnorm(length(x2), 0, noise))
}

#' @rdname ExampleSurfaces
#' @return \code{exampleSurfaceR3(x1, x2, x3, noise = 0)} defines a response
#'   surface with one maximum at pH 10, 300 K and a concentration of 0.5.
#' @examples
#'   exampleSurfaceR3(x1 = 320, x2 = 4.5, x3 = 0.3)
#'   exampleSurfaceR3(x1 = c(310, 320), x2 = c(4.5, 5.8), x3 = c(0.3, 0.5))
#'   exampleSurfaceR3(x1 = c(310, 320), x2 = c(4.5, 5.8), x3 = c(0.3, 0.5),
#'                    noise = 5)
#' @export
exampleSurfaceR3 <- function (x1, x2, x3, noise = 0) {
  if (length(x1) != length(x2) || length(x3) != length(x2)) {
    stop('Vector parameters x1, x2 and x3 must have same length')
  }
  if (any(any(x1 > 365), any(x1 < 278), any(x2 > 14), any(x2 < 0),
          any(x3 < 0), any(x3 > 1))) return(-1)
  return(94 * (exp(-(0.035*(x2 - 10)^2 + 0.002 * (x1 - 300)^2 +
                         0.3*(x3 - 0.5)^2))) +
           rnorm(length(x2), 0, noise))
}
