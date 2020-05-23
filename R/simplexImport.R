#' Imports the information contained in a \code{.smplx} file.
#'
#' The function reads and (optionally) loads into the environment the simplex
#' (object of class \code{smplx}) contained in a \code{.smplx} file previously
#' created using \code{\link{simplexExport}}.
#'
#' @param  filename  string with the name of the file (without extension) to
#'                   be imported. This file must be generated using
#'                   \code{\link{simplexExport}}. The path must be included if
#'                   the file is not in the current directory.
#' @param  aut.load  logical. Should the imported simplex object be directly
#'                   loaded on the Environment? Default to TRUE.
#' @param  name      name for the simplex object to be created if
#'                   \code{auth.ld = FALSE}. When not provided, the ID in the
#'                   file will be used.
#' @return A \code{smplx} class object with the complete information of the
#'         simplex
#' @seealso \code{\link{simplexImport}}
#' @examples
#' \dontrun{
#'   simplexExport(labsimplex(N = 4), name = "simplex4D")
#'   simplexImport("simplex4D.smplx")
#'   # Other way:
#'   simplex4D <- simplexImport("simplex4D.smplx", aut.load = FALSE)
#' }
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Jesús Ágreda, \email{jagreda@@unal.edu.co}
#' @export

simplexImport <- function(filename, aut.load = TRUE, name = NULL){
  filename <- paste0(filename, '.smplx')
  inlist <- strsplit(readLines(filename), "[[:space:]]+")
  fnd <- function(word) return(which(inlist == word))
  if (missing(name)) name <- inlist[grep("ID", inlist)][[1]][2]

  N <- as.numeric(inlist[fnd("$dim") + 1][[1]][2])
  lsimplex <- as.numeric(inlist[fnd("$lsimplex") + 1][[1]][2])

  coords <- matrix(ncol = N)
  rnames <- vector()
  coord0 <- fnd("$coords") + 2
  coord1 <- fnd("$centroid") - 2

  for (ii in coord0:coord1) {
    coords <- rbind(coords, inlist[[ii]][2:(N+1)])
    rnames <- c(rnames, inlist[[ii]][1])
  }
  coords <- apply(coords[-1, ], 2, as.numeric)
  row.names(coords) <- rnames
  colnames(coords)  <- inlist[fnd("$coords") + 1][[1]][2:
                         length(inlist[fnd("$coords") + 1][[1]])]

  if (inlist[fnd("$qual.fun") + 1][[1]][1] == "NULL") {
    qual.fun <- NULL
  } else {
    qual.fun <- as.numeric(inlist[fnd("$qual.fun") + 1][[1]][2:
                             length(inlist[fnd("$qual.fun") + 1][[1]])])
  }

  simplex <- labsimplex(N = N)

  simplex$coords   <- coords
  simplex$qual.fun <- qual.fun
  simplex$lsimplex <- lsimplex

  if (inlist[fnd("$vertex.label") + 1][[1]][1] == "NULL") {
    simplex$vertex.label <- NA
  } else {
    simplex$vertex.label <-
      gsub("\"", "", inlist[fnd("$vertex.label") + 1][[1]][2:
                       length(inlist[fnd("$vertex.label") + 1][[1]])])
  }
  P.eval <- as.logical(inlist[fnd("$P.eval") + 1][[1]][2])
  simplex$P.eval <- P.eval

  simplex$tim.ret <- as.numeric(inlist[fnd("$tim.ret") + 1][[1]][2:
                                  length(inlist[fnd("$tim.ret") + 1][[1]])])
  simplex$vertex.nat <- gsub("\"", "", inlist[fnd("$vertex.nat") + 1][[1]][2:
                                  length(inlist[fnd("$vertex.nat") + 1][[1]])])

  if (aut.load) {
    if (exists(name)) {
      message(paste0("Object '", name, "' already on envorinment.",
                     "Do you want to overwrite it? Y/n"))
      res <- readline()
      if (res %in% c("n", "N", "not", "Not", "NOT")) {
        return(simplex)
      }
    }
    assign(name, simplex, envir = parent.frame())
  } else {
    return(simplex)
  }
}
