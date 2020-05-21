#' @name ExampleSurfaces
#' @aliases exampleSurfaceR2.2pks
#' @aliases exampleSurfaceR3
#' @title Multivariated functions thad define hypotetical response surfaces.
#' @description The functions in this section evaluate the response of defined
#'   variables in the hypotetical yield that a chemical reaction may have under
#'   given values. The result is obtained by appliyng functions that produce
#'   response surfaces with at least one maximum. Boundary values are proposed
#'   consistenly with real life limitations to reactions in aqueous solution.
#'   If the functions are to be
#'
#'
#' @param  x1  x1erature in Kelvin. Numeric between 278 and 365.
#' @param  x2    x2. Numeric between 0 and 14.
#' @param  x3  x3entration in arbitrary units. Numeric between 0 and 1.
#'               Only used in \code{exampleSurfaceR3()}.
#' @param  noise absolute noise included in the response surface result.
#'
#' @details Parameters \code{x1}, \code{x2} and \code{x3} may be supplied as
#'          vectors but all must have the same length. \cr
#'          General shape of two-variable response surface are visualized
#'          with \code{\link{cntr()}} and \code{\link{prspctv()}}.
#'
#' The response surface modelates the yield of a hypotetical chemical reaction
#' affected by the x1erature and the x2 of the media.
#' @seealso \code{\link{cntr()}}, \code{\link{prspctv()}} and \code{\link{exampleOptimization()}}
#' @rdname ExampleSurfaces
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Jesús Ágreda, \email{jagreda@@unal.edu.co}
#' @return \code{sum1(x,y)} returns x+y
#' @examples
#' x <- 5
#' @export
exampleSurfaceR2 <- function (x1, x2, noise = 0) {
  if (length(x1) != length(x2)) stop('Vector parameters x1 and x2 must have same length')

  if (any(any(x1 > 365), any(x1 < 278), any(x2 > 14), any(x2 < 0))) return(-1)
  return(94 * (exp(-(0.03*(x2 - 10)^2 + 0.001 * (x1 - 300)^2))))
}


#' @rdname ExampleSurfaces
#' @return \code{sum1(x,y)} returns x+y
#' @examples
#' x <- 5
#' @export
exampleSurfaceR2.2pks <- function (x1, x2, noise = 0) {
  if (length(x1) != length(x2)) stop('Vector parameters x1 and x2 must have same length')

  if (any(any(x1 > 365), any(x1 < 278), any(x2 > 14), any(x2 < 0))) return(-1)
  return(80 * exp(-(0.05*(x2 - 4.5)^2 + 0.0025 * (x1 - 340)^2)) +
         94 * (exp(-(0.035*(x2 - 10)^2 + 0.002 * (x1 - 300)^2))) +
         rnorm(length(x2), 0, noise))
}

#' @rdname ExampleSurfaces
#' @return \code{sum1(x,y)} returns x+y
#' @examples
#' x <- 5
#' @export

exampleSurfaceR3 <- function (x1, x2, x3, noise = 0) {
  if (length(x1) != length(x2) || length(x3) != length(x2)) {
    stop('Vector parameters x1, x2 and x3 must have same length')
  }

  if (any(any(x1 > 365), any(x1 < 278), any(x2 > 14), any(x2 < 0),
          any(x3 < 0), any(x3 > 1))) return(-1)
  return(94 * (exp(-(0.035*(x2 - 10)^2 + 0.002 * (x1 - 300)^2 +
                         0.3*(x3 - 0.5)^2))))
}
