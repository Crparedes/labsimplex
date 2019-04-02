#' Exports the information contained on a \code{smplx} class object.
#'
#' Creates a \code{.smplx} file (text file) that stores the information
#' contained in the simplex (a \code{smplx} class object, see
#' \code{\link{labsimplex}}). It allows to continue with the optimization
#' proccess after experiment(s) indicated by a vertex(es) has been carried and
#' a measurement has been obtained.
#'
#' @param  simplex     \code{smplx} class object containing the simplex to be stored.
#' @param  filename    string with the name (without extention) of the file that will be created.
#'                     If not provided the name of the simplex provided will be used.
#' @param  direc       directory in which the file will be saved. If not provided,
#'                     the actual directory will be used.
#' @return A \code{.smplx} file containing all the information required to
#'         continue with the optimization process once all vertex experiments
#'         have been carried.
#' @examples
#' \dontrun{
#'   simplex <- labsimplex(N = 5, qual.fun = rnorm(6, 2, 1))
#'   simplexExport(simplex = simplex)
#' }
#' @export

simplexExport <- function(simplex, filename = NULL, direc = NULL){
  checkMain(simplex = simplex)
  if (missing(direc)) {
    direc <- getwd()
  }

  ID <- deparse(substitute(simplex))
  # To avoid anomalous behavior when simplex is generated inside the function
  fragmentedID <- strsplit(ID, '')[[1]]
  if (length(fragmentedID) > 10) {
    if ("(" == fragmentedID[10] && "x" == fragmentedID[9]) {
      ID <- paste0("NNSimplexDim", simplex$dim)
    }
  }

  if (missing(filename)){
    filename <- ID
  }

  class(simplex) <- "list"
  op.ms <- "\n IMPORTANT: \n\n This file contains all the information required to continue with the optimization process.
            DO NOT EDIT BY HAND!
            \n\n\n"
  utils::capture.output(cat(op.ms), file = paste0(direc, "/", filename, ".smplx"))
  utils::capture.output(cat(paste0("ID: ", ID, "\n\n")), file = paste0(direc, "/", filename, ".smplx"),
                        append = TRUE)
  utils::capture.output(simplex, file = paste0(direc, "/", filename, ".smplx"), append = TRUE)
}
