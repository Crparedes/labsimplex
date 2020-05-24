#' Plots the response versus the vertex number of a simplex optimization.
#'
#' The function generates a plot for an object with class \code{smplx}
#' if the vertexes has responses assigned. The response is plotted
#' against the vertex number.
#'
#' If the simplex object being ploted was obtained using a variable size
#' algorithm, some experimental points could be disregarded and will be
#' shown with a red mark indicating that the vertex was not used in the
#' obtention of new vertexes.
#'
#' @param  x   object with class \code{smplx} containig the coordinates of
#'             the vertices and their responses.
#' @param  ... other graphical parameters used in \code{\link[graphics]{plot}}
#' @return Plot of response against vertex number.
#' @examples
#'   simplex <- exampleOptimization(surface = exampleSurfaceR3,
#'                                  centroid = c(350, 11, 0.7),
#'                                  stepsize = c(10, 0.5, 0.1),
#'                                  experiments = 18, algor = 'variable')
#'   plotSimplexResponse(simplex)
#' @importFrom graphics lines points plot segments title
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Jesús Ágreda, \email{jagreda@@unal.edu.co}
#' @export


plotSimplexResponse <- function(x, ...){
  # Error handling
  checkMain(simplex = x)
  if (length(x$qual.fun) < (nrow(x$coords) - 1)) {
    if (all(x$vertex.nat == 'S')) {
      stop("All starting vertices must have response values assigned",
           " before ploting the responses")
    } else {
      stop("Only the last vertex generated is allowed to not have a",
           " response assigned")
    }
  }

  VertexNumber <- as.numeric(gsub("Vertex.", "", dimnames(x$coords)[[1]]))
  if (length(x$qual.fun) == (nrow(x$coords) - 1)) {
    VertexNumber <- VertexNumber[-length(VertexNumber)]
    x$qual.fun   <- x$qual.fun[!is.na(x$qual.fun)]
  }

  plot(VertexNumber, x$qual.fun, ylab = 'Response', xlab = 'Vertex number')

  if (any(x$vertex.label == 'D')) {
    Dis.pos <- which(x$vertex.label == 'D')
    points(VertexNumber[Dis.pos], x$qual.fun[Dis.pos], col = 'gray')
    points(VertexNumber[Dis.pos], x$qual.fun[Dis.pos], col = 2, pch = 4)
    lines(VertexNumber[-Dis.pos][order(VertexNumber[-Dis.pos])],
          x$qual.fun[-Dis.pos][order(VertexNumber[-Dis.pos])], col = 4)
  } else {
    lines(VertexNumber, x$qual.fun, col = 4)
  }
}

