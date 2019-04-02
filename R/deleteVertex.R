#' Delete vertex of a simplex (a \code{smplx} class object)
#'
#' Delete vertex (vertexes) of a simplex.
#'
#' If the object without the undesired vertex/vertexes is not pretended to replace the original simplex but
#' only to be visualized or assigned to a new object, overwrite = FALSE must be used.
#' The manipulation of vertexes of the current simplex should be avoided since it may lead to anomalous behabiour.
#' For a vertex whose coordinates are not evaluable because of practical or security restrictions,
#' an infinitevely bad response may be assigned in order to force the simplex moving in another direction.
#'
#' @param simplex    simplex object to be modified
#' @param vertex     numeric vector of integers as the identificators of the vertexes to be removed
#' @param overwrite  logical. Should the simplex without the undesired vertexes replace the current simplex?
#'
#' @export

deleteVertex <- function (simplex, vertex, overwrite = FALSE) {
  name <- deparse(substitute(simplex))
  checkMain(simplex = simplex)
  if (class(vertex) != 'numeric') {
    stop("vertex must be a numeric vector containg only the integer of the vertex identificators")
  }

  names <- as.integer(gsub("Vertex.", "", rownames(simplex$coords)))
  if (!all(vertex %in% names)) {
    stop("At least one of the vertex to be eliminated is not present in the simplex")
  }

  pos <- which((names %in% vertex) == TRUE)
  simplex$coords       <- simplex$coords[-pos, ]
  simplex$tim.ret      <- simplex$tim.ret[-pos]
  simplex$qual.fun     <- simplex$qual.fun[-pos]
  simplex$vertex.nat   <- simplex$vertex.nat[-pos]
  simplex$vertex.label <- simplex$vertex.label[-pos]

  if (any(pos >= (nrow(simplex$coords) - simplex$dim))) {
    warning("At least one of the vertexes of the current simplex were deleted")
  }

  if (overwrite) {
    assign(name, simplex, envir = parent.frame())
  } else {
    return(simplex)
  }
}
