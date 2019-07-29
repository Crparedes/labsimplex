#' Generates an empirical polynomial response surface based on vertexes responses
#'
#' Given the response of some vertexes, it is posible to fit a polynomial
#' surface to the data to loosely predict the response surface.
#'
#' @param  simplex   \code{'chsmplx'} type object containig simplex information
#' @return An empirical polinomia that approaches the response surface and a plot
#' @examples
#' simplex3D <- labsimplex(N = 3)
#' simplex3D <- generateVertex(simplex = simplex3D, qflv = rnorm(4))
#' @export

empiricalSurface <- function(simplex){

  name <- deparse(substitute(simplex))

  # Error handling
  checkMain(simplex)

}
