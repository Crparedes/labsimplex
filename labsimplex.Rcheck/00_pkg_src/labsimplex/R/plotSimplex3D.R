#' Makes a three dimentional plot of a simplex object.
#'
#' The function generates a 3D plot for a \code{smplx} object having
#' dimensionality of at least 3. When dimensionality is higher than 3, the
#' function plots a 3D proyection of selected dimensions.
#'
#' @param  sel.dim numeric or char vector for variables to be considered when
#'                 simplex dimensionality is higher than 3. If \code{numeric}
#'                 form it must contain dimensions ordinal number. If
#'                 \code{char}, it must contain dimensions names.
#' @param  main    title for the plot.
#' @param  angle   angle for perspective between x and y axis.
#' @param  ...     other arguments passed to scatterplot3d::scatterplot3d
#' @inheritParams generateVertex
#' @inheritParams plot.smplx
#' @return 3D proyection of the simplex coordinates.
#' @examples
#'   plotSimplex3D(simplex = labsimplex(N = 3))
#'
#'   plotSimplex3D(simplex = labsimplex(N = 8))
#'   plotSimplex3D(simplex = labsimplex(N = 8), sel.dim = c(4, 6, 8))
#'
#'   ## Simulation of the real proccess where a simplex is made and measured,
#'   ## plotted nad the new vertex is measuresd after it is generated.
#'   set.seed(12)
#'   simplex3D <- labsimplex(N = 3)
#'   plotSimplex3D(simplex = simplex3D)
#'   generateVertex(simplex = simplex3D, qflv = rnorm(4), overwrite = TRUE)
#'   plotSimplex3D(simplex = simplex3D)
#'   generateVertex(simplex = simplex3D, qflv = rnorm(1), overwrite = TRUE)
#'   plotSimplex3D(simplex = simplex3D)
#'   generateVertex(simplex = simplex3D, qflv = rnorm(1), overwrite = TRUE)
#'   plotSimplex3D(simplex = simplex3D)
#' @importFrom graphics lines par plot segments title
#'
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Jesús Ágreda, \email{jagreda@@unal.edu.co}
#' @export

plotSimplex3D <- function(simplex, sel.dim = NULL, all.ver = TRUE,
                          all.lin = TRUE, main = NULL, angle = 30, ...){

  # Error handling
  checkMain(simplex = simplex)
  if (simplex$dim < 3){
    stop("Simplex dimension must be at least 3")
  }

  var.plt <- 1:3
  if (!missing(sel.dim)) {
    if (length(sel.dim) != 3) {
      stop("Only 3 coordinates can be plotted. Length of sdim vector differs
           from 3: ", length(sel.dim))
      }
  } else {
    if (simplex$dim > 3) {
      message("No selected dimensions for ploting. Default is the first three
              ones.")
    }
  }

  if (!missing(sel.dim)) {
    if (is.numeric(sel.dim)) {
      var.plt <- sel.dim
      if (any(var.plt > simplex$dim)){
        stop("At least one ingresed variable is not available in data")
      }
    } else {
      var.plt <- (1:ncol(simplex$coords))[match(sel.dim,
                                                colnames(simplex$coords))]
    }
    if (length(var.plt) != 3) {
      stop("At least one ingresed variable is not available in data")
    }
  }


  if (all.ver) {
    s <- scatterplot3d::scatterplot3d(x = simplex$coord[, var.plt], type = 'p',
                                      pch = 16, grid = FALSE, angle = angle,
                                      main = main, lty.hide = 3, ...)
  } else {
    s <- scatterplot3d::scatterplot3d(x = simplex$coord[(nrow(simplex$coords) -
             simplex$dim):nrow(simplex$coords), var.plt], type = 'p', pch = 16,
             grid = FALSE, angle = angle, main = main, lty.hide = 3, ...)
  }
  vertex.xy <- cbind(s$xyz.convert(simplex$coord[, var.plt[1]],
                       simplex$coord[,var.plt[2]],
                       simplex$coord[, var.plt[3]])$x,
                     s$xyz.convert(simplex$coord[, var.plt[1]],
                       simplex$coord[, var.plt[2]],
                       simplex$coord[, var.plt[3]])$y)
  if (all.lin) {
    for (ii in 1:(nrow(simplex$coords) - simplex$dim)) {
      segments(x0 = vertex.xy[ii, 1], x1 = vertex.xy[(ii + 1):(ii + 3), 1],
               y0 = vertex.xy[ii, 2], y1 = vertex.xy[(ii + 1):(ii + 3), 2],
               col = "grey", lwd = 0.7)
    }
  }

  for (ii in (nrow(simplex$coords) - simplex$dim):(nrow(simplex$coords) - 1)) {
    segments(x0 = vertex.xy[ii, 1], x1 = vertex.xy[(ii + 1):
                                                     nrow(simplex$coords), 1],
             y0 = vertex.xy[ii, 2], y1 = vertex.xy[(ii + 1):
                                                     nrow(simplex$coords), 2],
             col = "blue")
  }
}
