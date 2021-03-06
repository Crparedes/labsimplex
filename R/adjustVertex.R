#' Modify the coordinates of given vertexes of a simplex
#'
#' Changes the coordinates of previously generated vertexes when slight
#' differences were impossible to avoid at the moment of setting the experiment
#' variables (e.g. small differences in mass components when preparing a
#' mixture). This function allows the correction of the vertexes of a simplex
#' in order to produce movements of the simplex based on the actual
#' coordinates.
#'
#' @param  newcoords List with elements named like the vertexes to be modified.
#'                   Each element must have a vector with the actual (ordered)
#'                   coordinates used in the experiment. \code{NA} values may
#'                   be used to indicate unchanged coordinates
#' @inheritParams generateVertex
#'
#' @return An object of class \code{smplx} with the modified simplex information.
#' @examples
#' simplex <- labsimplex(n = 3, start = c(7, 25, 0.15),
#'                       stepsize = c(0.2, 5, 0.02))
#' adjustVertex(simplex = simplex, newcoords = list(Vertex.1 = c(7.1, NA, NA),
#'                                                  Vertex.3 = c(6.9, NA, 0.155)),
#'              overwrite = TRUE)
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Jesús Ágreda, \email{jagreda@@unal.edu.co}
#' @export

adjustVertex <- function(simplex, newcoords, overwrite = FALSE) {

  name <- deparse(substitute(simplex))
  checkMain(simplex = simplex)

  if (any(summary(newcoords)[, 3] != "numeric")) {
    stop("Only numeric or NA values are allowed for the adjusted coordinates")
  }

  # Vertices to be adjusted:
  VerTBA <- attr(summary(newcoords), "dimnames")[[1]]
  for (i in 1:length(VerTBA)) {
    pos.RM <- match(tolower(gsub('\\.', '', VerTBA[i])),
                    tolower(gsub('\\.', '', attr(simplex$coords,
                                                 "dimnames")[[1]])))
    if (is.na(pos.RM)) {
      stop("At least one of submited vertex (list elements names) is not in ",
           "the simplex object to be adjusted")
    }
    VerTBA[i] <- pos.RM
  }
  VerTBA <- as.numeric(VerTBA)

  for (i in 1:length(VerTBA)) {
    # Coordinates that remain unchanged
    newcoords[[i]][is.na(newcoords[[i]])] <- simplex$coords[VerTBA[i],
                                               is.na(newcoords[[i]])]
    # Replacement:
    simplex$coords[VerTBA[i], ] <- newcoords[[i]]
  }

  if (overwrite) {
    assign(name, simplex, envir = parent.frame())
  } else {
    cat("\n")
    return(simplex)
  }
}
