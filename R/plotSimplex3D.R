#' Draws a three dimentional plot of the vertexes in a simplex
#'
#' The function generates a 3D plot of the vertexes in a simplex optimization
#' when simplex dimensionality is at least 3. When dimensionality is higher
#' than 3, the plot produced is a projection of the selected variables.
#'
#' @param  sel.dim \code{numeric} or \code{char} vector for variables to be
#'                 considered when simplex dimensionality is higher than 3.
#'                 By default, the first three are chosen. If the vector is
#'                 \code{numeric}, it must contain the ordinal numbers
#'                 corresponding to the desired variables. If the vector is of
#'                 class \code{char}, it must contain the names of such
#'                 dimensions.
#' @param  main    title for the plot.
#' @param  angle   angle for perspective between x and y axis.
#' @param  ...     other arguments passed to
#'                 \code{\link[scatterplot3d]{scatterplot3d}}
#' @inheritParams generateVertex
#' @inheritParams plot.smplx
#' @return 3D plot of the simplex coordinates.
#' @examples
#'   plotSimplex3D(simplex = labsimplex(N = 3, centroid = c(350, 11, 0.7),
#'                                      stepsize = c(10, 0.5, 0.1),
#'                                      var.name = c('temperature', 'pH',
#'                                                   'concentration')))
#'
#'   ## Several options are posible when visualizing higher order simplexes
#'   plotSimplex3D(simplex = labsimplex(N = 8))
#'   plotSimplex3D(simplex = labsimplex(N = 8), sel.dim = c(4, 6, 8))
#'
#'   ## Simplex movements can be visualized after some experiments has been
#'   ## performed
#'   simplex <- exampleOptimization(surface = exampleSurfaceR3,
#'                                  centroid = c(350, 11, 0.7),
#'                                  stepsize = c(10, 0.5, 0.1),
#'                                  experiments = 18)
#'   plotSimplex3D(simplex = simplex, angle = 80)
#' @seealso \code{\link{plot.smplx}}
#' @importFrom graphics lines par plot segments title
#' @import scatterplot3d
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Jesús Ágreda, \email{jagreda@@unal.edu.co}
#' @export

plotSimplex3D <- function(simplex, sel.dim = NULL, all.ver = TRUE,
                          all.lin = TRUE, main = NULL, angle = 30, ...) {

  # Error handling
  checkMain(simplex = simplex)
  if (simplex$dim < 3) stop("Simplex dimension must be at least 3")

  var.plt <- 1:3
  if (!missing(sel.dim)) {
    if (length(sel.dim) != 3) {
      stop("Only 3 coordinates can be plotted. Length of sdim vector differs",
           "from 3: ", length(sel.dim))
      }
  } else {
    if (simplex$dim > 3) {
      message("No selected dimensions for ploting. Default is the first three ones.")
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
    s <- scatterplot3d(x = simplex$coord[, var.plt], type = 'p', pch = 16,
                       grid = FALSE, angle = angle, main = main,
                       lty.hide = 3, ...)
  } else {
    s <- scatterplot3d(x = simplex$coord[(nrow(simplex$coords) -
                       simplex$dim):nrow(simplex$coords), var.plt],
                       type = 'p', pch = 16, grid = FALSE, angle = angle,
                       main = main, lty.hide = 3, ...)
  }
  vertex.xy <- cbind(s$xyz.convert(simplex$coord[, var.plt[1]],
                     simplex$coord[, var.plt[2]],
                     simplex$coord[, var.plt[3]])$x,
                     s$xyz.convert(simplex$coord[, var.plt[1]],
                     simplex$coord[, var.plt[2]],
                     simplex$coord[, var.plt[3]])$y)
  if (all.lin) {
    V.pos <- as.numeric(gsub("Vertex.", "", row.names(simplex$coords)))
    #if (!is.null(nrow(x$families))) {
    for (ii in 1:length(simplex$families)) {
      for (jj in 1:(length(simplex$families[[ii]]) - 1)) {
        for (kk in (jj + 1):length(simplex$families[[ii]])) {
          jj. <- which(simplex$families[[ii]][jj] == V.pos)
          kk. <- which(simplex$families[[ii]][kk] == V.pos)
          lines(vertex.xy[c(jj., kk.),  1],
                vertex.xy[c(jj., kk.),  2], col = "grey")
        }
      }
    }
    #}
  }

  for (ii in (nrow(simplex$coords) - simplex$dim):(nrow(simplex$coords) - 1)) {
    segments(x0 = vertex.xy[ii, 1],
             x1 = vertex.xy[(ii + 1):nrow(simplex$coords), 1],
             y0 = vertex.xy[ii, 2],
             y1 = vertex.xy[(ii + 1):nrow(simplex$coords), 2], col = "blue")
  }
}
