#' Plots the response versus the vertex number of a simplex optimization.
#'
#' The function generates a plot for a \code{smplx} class object whose vertices
#' must have a response value assigned. The response is plotted
#' against the vertex number. The only vertex allowed to not having response
#' assigned is the last one.
#'
#' If the simplex object being ploted was obtained using a variable size
#' algorithm, some experimental points could be disregarded and will be
#' shown with a red mark indicating that the vertex was not used in
#' new vertices calculations. Those points are also ignored by the line that
#' links the data in the scaterplot and pretends to show a tendency to
#' better values as more vertices are evaluated.
#'
#' @param  x \code{smplx} class object containig the coordinates of
#'                 the vertices and their response values.
#' @param  ...     other graphical parameters used in plot()
#' @return Plot of response against vertex number.
#' @examples
#' set.seed(1)
#' # Generate a simplex and add some responses to the vertices
#' simplex <- labsimplex(N = 3)
#' generateVertex(simplex, qflv = rnorm(4), algor = 'variable',
#'   overwrite = TRUE)
#' generateVertex(simplex, qflv = 2.75, algor = 'variable', overwrite = TRUE)
#' generateVertex(simplex, qflv = 0.8,  algor = 'variable', overwrite = TRUE)
#' generateVertex(simplex, qflv = 2.94, algor = 'variable', overwrite = TRUE)
#' generateVertex(simplex, qflv = 3.14, algor = 'variable', overwrite = TRUE)
#'
#' # Plot the response versus the vertex function
#' plotSimplexResponse(simplex)
#' @importFrom graphics lines points plot segments title
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Jesús Ágreda, \email{jagreda@@unal.edu.co}
#' @export


plotSimplexResponse <- function(x, ...){

  # Error handling
  checkMain(simplex = x)
  if (length(x$qual.fun) < (nrow(x$coords) - 1)) {
    if (all(x$vertex.nat == 'S')) {
      stop("All starting vertices must have response values assigned
           before ploting the responses")
    } else {
      stop("Only the last generated vertex is allowed to not having a
           response assigned")
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

