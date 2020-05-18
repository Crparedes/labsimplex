#' Evaluates two variables in a hypothetical response surface
#'
#' The response surface modelates the yield of a hypotetical chemical reaction
#' affected by the temperature and the pH of the media.
#'
#' @param  temp  Numeric value (may be a vector) for temperature.
#' @param  pH    Numeric value (may be a vector) for pH.
#' @param  noise Noise used in the response surface as absolute percentaje.
#' @param  seed  Seed for the noise generated
#'
#' @return The yield of the reaction
#' @examples
#' exmplsurface(temp = 290, pH = 4)
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Jesús Ágreda, \email{jagreda@@unal.edu.co}
#' @export

exmplsurface <- function (temp, pH, noise = 0, seed = NULL) {
  if (length(temp) != length(pH)) stop('Vector parameters x and y must have same length')
  # good range for Matlab's peaks function is -2, 2 for both x and y. We want 10, 90 for x and 1, 13 for y.
  x <- temp * 0.05 - 16
  y <- pH / 3 - 2.33
  if (!missing(seed)) set.seed(seed)
  return (3.8 * (8 + 10 * (1 - x)^2 * exp((- x^2 - (y + 1)^2)) -
                   10 * (x/5 - x^3 - y^5) * exp(- x^2 - y^2) -
                   1/3 * exp(- (x + 1)^2 - y^2)) + rnorm(n = length(x), mean = 0, sd = noise))
}
