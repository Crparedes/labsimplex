#' Modify given coordinates of given vertices of a simplex
#'
#' Changes the coordinates of generated vertices when slightly differences
#' were impossible to avoid at the moment of setting the experiment
#' (e.g. small differences is mass components when preparing a mixture).
#'
#'
#' @param  simplex   \code{'chsmplx'} type object containig most information
#'                   of simplex.
#' @param  newcoords List with elements named like the vertices to be modified.
#'                   Each element must have a vector with the (ordered) coordinates
#'                   setted for the experiment. containig a dataframe in which the variables
#'                   to be modified to that vertex are provided
#' @param  overwrite logical argument indicating if the simplex must be
#'                   automatically overwrited
#'
#' @return A 'chsmplx' type object with the information of the simplex
#'        including the conditions for the new experiment to be permormed.
#' @examples
#' simplex <- labsimplex(N = 3)
#' simplex <- generateVertex(simplex = simplex, qflv = rnorm(4))
#' \dontrun{
#' ## Optional form:
#' ## After obtaining quality function for the last vertex generated:
#' NV <- rnorm(1)
#' generateVertex(simplex = simplex3D, qflv = NV, overwrite = TRUE)
#' @export

adjustVertex <- function(simplex, newcoords, overwrite = FALSE) {

  name <- deparse(substitute(simplex))
  checkMain(simplex = simplex)

  if (any(summary(newcoords)[, 3] != "numeric")) {
    stop("Only numeric (or NA's) values are allowed for the adjusted coordinates")
  }

  # Vertices to be adjusted:
  VerTBA <- attr(summary(newcoords), "dimnames")[[1]]
  for (i in 1:length(VerTBA)) {
    pos.RM <- match(tolower(gsub('\\.', '', VerTBA[i])), tolower(gsub('\\.', '', attr(simplex$coords, "dimnames")[[1]])))
    if (is.na(pos.RM)) {
      stop("At least one of submited vertex (list elements names) is not in the
         simplex object to be adjusted")
    }
    VerTBA[i] <- pos.RM
  }
  VerTBA <- as.numeric(VerTBA)

  for (i in 1:length(VerTBA)) {
    # Coordinates that remain unchanged
    newcoords[[i]][is.na(newcoords[[i]])] <- simplex$coords[VerTBA[i], is.na(newcoords[[i]])]
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
