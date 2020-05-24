#' Generates a simplex object
#'
#' The simplex (a list with class \code{smplx}) contains the coordinates
#' of the N+1 vertices that define a simplex in an \emph{n}-dimensional space.
#' By default, the function produces a regular simplex centered at the origin.
#' The coordinates of the regular simplex are transformed into the real
#' variables space by using the information of the start or centroid and
#' step-size. The only non-optional parameter is \code{N} that relates the
#' simplex dimensionality. Once the simplex
#' is generated, the experiments under the conditions indicated for each
#' variable at each vertex must be carried and the response obtained.
#' Those responses are assigned to the \code{smplx} object at the moment of
#' generating the new vertex (see \code{\link{generateVertex}}).
#'
#' The regular simplex coordinates are generated following the general
#' algorithm for the cartesian coordinates of a regular n-dimensional simplex.
#' This algorithm considers that all vertices must be equally distanced from
#' simplex centroid and all angles subtended between any two vertexes and the
#' centroid of a simplex are equal to \emph{arccos(-1/n)}.\cr
#' If the vertexes coordinates are manually given (in \code{usr.def}
#' parameter), the function checks if the faces produced belong to different
#' hyperplanes. This avoids the generation of a degenerated simplex.
#'
#' @param  N         dimensionality of the simplex (i.e. number of variables)
#' @param  start     numeric vector of size \code{N} with coordinates of the
#'                   first vertex
#' @param  centroid  numeric vector of size \code{N} with coordinates of the
#'                   centroid
#' @param  stepsize  numeric vector of size \code{N} with the step-sizes for
#'                   each coordinate
#' @param  usrdef    \code{(N+1)xN} matrix containig in (N+1) rows the N
#'                   coordinates for each vertex
#' @param  var.name  vector containing the names for the variables
#' @return  An object of class \code{smplx} with the information of the new
#'   simplex.
#' @examples
#'   simplex <- labsimplex(N = 3)
#'   simplex <- labsimplex(N = 3, centroid = c(350, 7, 0.4),
#'                         stepsize = c(35, 2, 0.3),
#'                         var.name = c('temperature', 'pH', 'concentration'))
#'   simplex <- labsimplex(N = 3, usrdef = rbind(c(390, 8, 0.2), c(330, 8, 0.2),
#'                                    c(330, 6, 0.6), c(330, 6, 0.1)))
#'   \dontrun{
#'     ## User defined coordinates may define a degenerated simplex:
#'     simplex <- labsimplex(N = 3,
#'                           usrdef = rbind(c(390, 8, 0.3), c(340, 8, 0.3),
#'                                          c(355, 8, 0.3), c(340, 5, 0.1)))
#'   }
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Jesús Ágreda, \email{jagreda@@unal.edu.co}
#' @references Nelder, J. A., and R. Mead. 1965. “A Simplex Method for Function
#' Minimization.” The Computer Journal 7 (4): 308–13.
#' @references Spendley, W., G. R. Hext, and F. R0. Himsworth. 1962. “Sequential
#' Application of Simplex Designs in Optimization and Evolutionary Operation.”
#' Technometrics 4 (4): 441–61.
#' @export
labsimplex <- function(N, start = NULL, centroid = NULL, stepsize = NULL,
                       usrdef = NULL, var.name = NULL){

  main.list <- list(dim = N, coords = NULL, centroid = NULL,
                    qual.fun = NULL, vertex.label = NULL, tim.ret = NULL,
                    vertex.nat = NULL, P.eval = FALSE)
  class(main.list) <- 'smplx'

  # Error handling
  # Checks for redundant parameters definition
  if (!missing(centroid) && !missing(start)) {
    stop("Redundant definition: start and centroid parameters are incompatible.",
         " Only one must be defined")
  }
  # Possible errors when usrdef is provided
  if (!missing(usrdef)) {
    # If provided, usrdef must be an appropiate matrix
    if (ncol(usrdef) != nrow(usrdef) - 1) {
      stop("Parameter usrdef must be a N+1 x N matrix containig in each row",
            " the N coordinates for each vertex.")
    }
    # If both provided, usrdef and N must match. If not provided,
    # N is defined using usrdef
    if (!missing(N)) {
      if (ncol(usrdef) != N) {
        stop("Number of coordinates in user defined matrix differs from",
             " dimensionality: ", N, " and ", ncol(usrdef), ".")
      }
    } else {
      N <- ncol(usrdef)
    }
    # Checking if points define a simplex
    if (abs(det(cbind(usrdef, rep(1, nrow(usrdef))))) < 1e-9) {
      stop("Given coordinates of vertex define hyperfaces that share at least",
           " one hyperplane!")
    } else {
      message("Provided points define a simplex:")
    }
    # Some parameters must not be provided if the coordinates are given
    if (!missing(stepsize) || !missing(start) || !missing(centroid)) {
      stop("Parameters such as start, centroid, and stepsize must not",
           " be provided when simplex coordinates are provided in usrdef.")
    }
  }
  # Possible errors when N is provided
  if (!missing(N)) {
    # If provided, stepsize must have appropiated format
    if (!missing(stepsize)) {
      if (length(stepsize) == 1) {
        message("Vector for stepsize is length 1, the same step size will be",
                " used in all dimensions.")
        stepsize <- rep(stepsize, N)
      } else {
        if (length(stepsize) != N){
          stop("Vector stepsize is expected to be of length 1 or equal to the",
               " dimensionality: ", N)
        }
      }
    }
    # If provided, centroid vector must have appropiated format
    if (!missing(centroid)) {
      if (length(centroid) == 1) {
        message("Vector for centroid is length 1, the same center will be used",
                " in all dimensions.")
        centroid <- rep(centroid, N)
      } else {
        if (length(centroid) != N) {
          stop("Vector centroid is expected to be of length 1 or equal to the",
               " dimensionality: ", N)
        }
      }
    }
    # If provided, start vector must have appropiated format
    if (!missing(start)) {
      if (length(start) == 1) {
        message("Vector for start is length 1, the same starting point will be",
                " used in all dimensions.")
        start <- rep(start, N)
      } else {
        if (length(start) != N) {
          stop("Vector start is expected to be of length 1 or equal to the",
               "dimensionality: ", N)
        }
      }
    }
  }
  # If provided the variable names, there must be a value for each dimension
  if (!missing(var.name)) {
    if (length(var.name) != N) {
      stop("Vector containing names for variables does not coincide in length",
           " with dimensionality")
    }
  }

  # Start of functions --------------------------------------------------------
  main.list$dim <- N
  main.list$lsimplex <- 1
  # Set vertex coordinates
  if (!missing(usrdef)) {
    V <- t(usrdef)
  } else {
    V <- matrix(0, nrow = N, ncol = N + 1)
    for (nc in 1:N) {
      V[nc, nc] <- sqrt(1 - sum(V[1:(nc - 1), nc] ** 2))
      for (nc1 in (nc + 1):(N + 1)) {
        V[nc, nc1] <- - (sum(V[1:nc, nc] * V[1:nc, nc1]) + 1 / N) / V[nc, nc]
      }
    }
  }

  coords <- t(V)

  if (missing(var.name)) var.name <- paste0("Var.", 1:ncol(coords))

  if (!missing(stepsize)) {
    s.size0  <- apply(coords, 2, max) - apply(coords, 2, min)
    c.factor <- stepsize / s.size0
    for (ii in 2:nrow(coords)) {
      coords[ii, ] <- coords[1, ]  + (coords[ii, ] - coords[1, ]) * c.factor
    }
  }

  if (!missing(start)) coords <- sweep(coords, 2, - (start - coords[1, ]))

  if (!missing(centroid)) {
    coords <- sweep(coords, 2, - (centroid - (colSums(coords) / (N + 1))))
  }

  main.list$coords            <- coords
  colnames(main.list$coords)  <- var.name
  row.names(main.list$coords) <- paste0("Vertex.", 1:nrow(coords))
  main.list$vertex.nat        <- rep("S", nrow(coords))
  main.list$vertex.his        <- rep("1.", nrow(coords))

  main.list$families <- list(1:(N + 1))

  main.list$centroid <- colSums(main.list$coords) / (N + 1)
  main.list$tim.ret  <- rep(1, (N+1))

  return(main.list)
}
