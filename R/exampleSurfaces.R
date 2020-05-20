#' @name ExampleSurfaces
#' @aliases exampleSurfaceR2.2pks
#' @aliases exampleSurfaceR3
#' @title Bivariated and trivariated functions thad define hypotetical response surfaces
#'
#' @param  temp  Numeric value (may be a vector) for temperature.
#' @param  pH    Numeric value (may be a vector) for pH.
#' @param  conc value
#' @param  noise Noise used in the response surface as absolute percentaje.
#'
#' @note \code{funs} is a generic name for the functions documented.
#' \cr
#' The response surface modelates the yield of a hypotetical chemical reaction
#' affected by the temperature and the pH of the media.
#'
#' @rdname ExampleSurfaces
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Jesús Ágreda, \email{jagreda@@unal.edu.co}
NULL

#' @rdname ExampleSurfaces
#' @return \code{sum1(x,y)} returns x+y
#' @examples
#' x <- 5
#' @export
exampleSurfaceR2 <- function (temp, pH, noise = 0) {
  if (length(temp) != length(pH)) stop('Vector parameters x and y must have same length')

  if (any(any(temp > 365), any(temp < 278), any(pH > 14), any(pH < 0))) return(-1)
  return(94 * (exp(-(0.035*(pH - 10)^2 + 0.002 * (temp - 300)^2))))
}


#' @rdname ExampleSurfaces
#' @return \code{sum1(x,y)} returns x+y
#' @examples
#' x <- 5
#' @export
exampleSurfaceR2.2pks <- function (temp, pH, noise = 0) {
  if (length(temp) != length(pH)) stop('Vector parameters x and y must have same length')

  if (any(any(temp > 365), any(temp < 278), any(pH > 14), any(pH < 0))) return(-1)
  return(80 * exp(-(0.05*(pH - 4.5)^2 + 0.0025 * (temp - 340)^2)) +
         94 * (exp(-(0.035*(pH - 10)^2 + 0.002 * (temp - 300)^2))) +
         rnorm(length(pH), 0, noise))
}

#' @rdname ExampleSurfaces
#' @return \code{sum1(x,y)} returns x+y
#' @examples
#' x <- 5
#' @export

exampleSurfaceR3 <- function (temp, pH, conc, noise = 0) {
  if (length(temp) != length(pH) || length(conc) != length(pH)) {
    stop('Vector parameters temp, pH and Conc must have same length')
  }

  if (any(any(temp > 365), any(temp < 278), any(pH > 14), any(pH < 0),
          any(conc < 0), any(conc > 1))) return(-1)
  return(94 * (exp(-(0.035*(pH - 10)^2 + 0.002 * (temp - 300)^2 +
                         0.3*(conc - 0.5)^2))))
}
