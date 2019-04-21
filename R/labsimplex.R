#' Generates simplex object with coordinates of starting vertexes.
#'
#' Generates a \code{smplx} class object containing the coordinates
#' of the N+1 vertexes of a simplex in a N-dimentional space.
#' Addionally can check if faces for given coordinates of a used defined
#' simplex are in different hyperplanes.
#'
#' All parameters are optional. If no parameters are provided a void
#' \code{smplx} class object is generated. Once the simplex is genereated,
#' the experiments under the conditions indicated for each variable at each
#' vertex must be carried and the quality function evaluated. Those quality
#' function values are assigned to the \code{smplx} object at the moment of
#' generating the new vertex (see \code{\link{generateVertex}}). The initial simplex
#' coordinates are generated following the
#' \href{https://bit.ly/2u8HlYt}{general algorithm} for the cartesian
#' coordinates for regular n-dimensional simplex.
#'
#' @param  N         number of dimentions in the space.
#' @param  start     numeric vector of size \code{N} with initial coordinates for the first vertex.
#' @param  centroid  coordinates of centroid for initial simplex.
#' @param  stepsize  numeric vector of size \code{N} with the step size for each coordinate.
#' @param  neg.con   logical argument indicating if there is a constraing for only non-negative coordinates.
#' @param  zer.con   logical argument indicating if there is a constraing for zero values in the coordinates.
#' @param  min.val   numeric vector of size \code{N} containing the minimal that a variable can
#'                   approach zero if \code{zer.con = TRUE}.
#' @param  usrdef    \code{(N+1)xN} matrix containig in (N+1) rows the N coordinates for each vertex.
#' @param  var.name  vector containing the names for the variables.
#' @return  A \code{smplx} type object with the information of the simplex.
#' @examples
#'   labsimplex(N = 3)
#'   labsimplex(N = 3, centroid = c(66, 1, 12), stepsize = c(10, 0.1, 2),
#'              neg.con = TRUE, var.name = c('potential', 'pH', 'T'))
#'
#'   labsimplex(usrdef = rbind(c(2, 0, 0), c(-0.5, 1, 0), c(-0.5, -0.7, 1),
#'                             c(-0.5, -0.4, -0.6)))
#' \dontrun{
#' ## A user defined coordinates may define faces that rely on same hyperplane:
#'   labsimplex(usrdef = rbind(c(2, 0, 0), c(-0.5, 0, 0), c(0, 0, 0), c(-0.5, -0.4, -0.6)))
#' }
#' @export

labsimplex <- function(N = NULL, start = NULL, centroid = NULL,
                       stepsize = NULL, neg.con = FALSE, zer.con = FALSE,
                       min.val = NULL, usrdef = NULL, var.name = NULL){

  main.list <- list(dim = N, coords = NULL, centroid = NULL,
                    qual.fun = NULL, vertex.label = NULL, tim.ret = NULL,
                    vertex.nat = NULL, P.eval = FALSE)
  class(main.list) <- 'smplx'

  #Void output when no data are provided
  if (missing(N) && missing(usrdef)) {
    return(main.list)
  }

  # Error handling
  # Checks for redundant parameters definition
  if (!missing(centroid) && !missing(start)) {
    stop("Redundant definition: only one between start or centroid can be defined")
  }
  # Possible errors when usrdef is provided
  if (!missing(usrdef)) {
    # If provided, usrdef must be an appropiate matrix
    if (ncol(usrdef) != nrow(usrdef) - 1) {
      stop("Parameter usrdef must be a N+1 x N matrix containig in each row
           the N coordinates for each vertex")
    }
    # If both provided, usrdef and N must match. If not provided,
    # N is defined using usrdef
    if (!missing(N)) {
      if (ncol(usrdef) != N) {
        stop("Number of coordinates in user defined matrix (", ncol(usrdef), ") differs from
             indicated dimensionality (", N, ")")
      }
    } else {
      N <- ncol(usrdef)
    }
    # Checking if points define a simplex
    if (abs(det(cbind(usrdef, rep(1, nrow(usrdef))))) < 1e-9) {
      stop("Given coordinates of vertex define hyperfaces that share at least one hyperplane!")
    } else {
      message("Provided points define a simplex:")
    }
    # Some parameters must not be provided if the coordinates are explicitly given
    if (!missing(stepsize) || !missing(start) || !missing(centroid) || !missing(min.val)) {
      stop("Parameters such as start, centroid, stepsize and min.val must not
           be provided when usrdef is defined")
    }
  }
  # Possible errors when N is provided
  if (!missing(N)) {
    # If provided, stepsize must have appropiated format
    if (!missing(stepsize)) {
      if (length(stepsize) == 1) {
        message("Vector for stepsize is length 1, the same step size will be used in all dimensions")
        stepsize <- rep(stepsize, N)
      } else {
        if (length(stepsize) != N){
          stop("Vector stepsize is expected to be of length 1 or equal to the dimensionality: ", N)
        }
      }
    }
    # If provided, centroid vector must have appropiated format
    if (!missing(centroid)) {
      if (length(centroid) == 1) {
        message("Vector for centroid is length 1, the same center will be used in all dimensions")
        centroid <- rep(centroid, N)
      } else {
        if (length(centroid) != N) {
          stop("Vector centroid is expected to be of length 1 or equal to the dimensionality: ", N)
        }
      }
    }
    # If provided, start vector must have appropiated format
    if (!missing(start)) {
      if (length(start) == 1) {
        message("Vector for start is length 1, the same starting point will be used in all dimensions")
        start <- rep(start, N)
      } else {
        if (length(start) != N) {
          stop("Vector start is expected to be of length 1 or equal to the dimensionality: ", N)
        }
      }
    }
  }
  # If provided the variable names, there must be a value for each dimension
  if (!missing(var.name)) {
    if (length(var.name) != N) {
      stop("Vector containing names for variables does not coincide in length with dimensionality")
    }
  }
  # If provided. min.val must have an addequate size
  if (zer.con) {
    if (missing(min.val)) {
      message("Default minimal value for each dimension when zer.con = TRUE is 10% of
              its maximal value ")
    } else {
      if (length(min.val) == 1) {
        message("Vector min.val is length 1, the same value will be used in all dimensions")
        min.val <- rep(min.val, N)
      } else {
        if (length(min.val) != N) {
          stop("Vector min.val is expected to be of length 1 or equal to the dimensionality: ", N)
        }
      }
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
      suppressWarnings(
        V[nc, nc] <- sqrt(1 - sum(rbind(V[1:nc, nc], V[nc:N, nc]) ** 2)))
      for (nc1 in (nc + 1):(N + 1)) {
        suppressWarnings(
          V[nc, nc1] <- - (sum(V[1:nc, nc] * V[1:nc, nc1]) + 1 / N) / V[nc, nc])
      }
    }
  }

  coords <- t(V)

  if (missing(var.name)) {
   var.name <- paste0("Var.", 1:ncol(coords))
  }

  if (!missing(stepsize)) {
    s.size0  <- apply(coords, 2, max) - apply(coords, 2, min)
    c.factor <- stepsize / s.size0
    for (ii in 2:nrow(coords)) {
      coords[ii, ] <- coords[1, ]  + (coords[ii, ] - coords[1, ]) * c.factor
    }
  }

  if (!missing(start)) {
      coords <- sweep(coords, 2, - (start - coords[1, ]))
  }

  if (!missing(centroid)) {
    coords <- sweep(coords, 2, - (centroid - (colSums(coords) / (N + 1))))
  }

  if (neg.con) {
    if (any(coords < 0)) {
      message("Centroid could get displaced due to a negative coordinates constraint")
      coords[coords < 0] <- 0.01
    }
  }

  if (zer.con) {
    if (any(coords == 0)) {
      message("Centroid could get displaced due to a non-zero coordinates constraint")
      if (missing(min.val)) {
        min.val <- 0.1 * apply(coords, 2, max)
      }
      for (ii in 1:ncol(main.list$coords)) {
        coords[which(coords[, ii] == 0), ii] <- min.val[ii]
      }
    }
  }

  main.list$coords            <- coords
  colnames(main.list$coords)  <- var.name
  row.names(main.list$coords) <- paste0("Vertex.", 1:nrow(coords))
  main.list$vertex.nat        <- rep("S", nrow(coords))
  main.list$vertex.his        <- rep("1.", nrow(coords))


  main.list$centroid <- colSums(main.list$coords) / (N + 1)
  main.list$tim.ret  <- rep(1, (N+1))

  return(main.list)
}
