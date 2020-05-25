#' Exports the information contained in an object of class \code{smplx}.
#'
#' Creates a text file with extension \code{.smplx} that contains the complete
#' information contained in a simplex (an object with class \code{smplx},
#' see \code{\link{labsimplex}}). This file allows the continuation of an
#' optimization proccess when the experiments take too long and multiple
#' \code{R} sessions are required. The file produced is also useful to share
#' the information of the optimization process. The exported simplex can be
#' later imported with \code{\link{simplexImport}}.
#'
#' @param  simplex  object of class \code{smplx} containing the simplex to be
#'                  exported
#' @param  filename string with the name (without extention) of the file that
#'                  will be created. If not provided, the name of the simplex
#'                  object is used.
#' @param  direc    directory in which the file will be saved. If not provided,
#'                  the current working directory is used.
#' @return Generates a \code{.smplx} file containing all the information
#'         required to continue with the optimization process after the
#'         experiments have been carried.
#' @seealso \code{\link{simplexImport}}
#' @examples
#' \dontrun{
#'   simplex <- labsimplex(n = 5, qual.fun = rnorm(6, 2, 1))
#'   simplexExport(simplex = simplex)
#' }
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Jesús Ágreda, \email{jagreda@@unal.edu.co}
#' @importFrom utils capture.output
#' @export

simplexExport <- function(simplex, filename = NULL, direc = NULL) {
  checkMain(simplex = simplex)
  if (missing(direc)) direc <- getwd()

  ID <- deparse(substitute(simplex))
  # To avoid anomalous behavior when simplex is generated inside the function
  fragmentedID <- strsplit(ID, '')[[1]]
  if (length(fragmentedID) > 10) {
    if ("(" == fragmentedID[10] && "x" == fragmentedID[9]) {
      ID <- paste0("NNSimplexDim", simplex$dim)
    }
  }

  if (missing(filename)) filename <- ID

  class(simplex) <- "list"
  op.ms <- "\n IMPORTANT: \n\n This file contains all the information required
            to continue with the optimization process.
            DO NOT EDIT BY HAND!
            \n\n\n"
  capture.output(cat(op.ms), file = paste0(direc, "/", filename, ".smplx"))
  capture.output(cat(paste0("ID: ", ID, "\n\n")),
                 file = paste0(direc, "/", filename, ".smplx"), append = TRUE)
  capture.output(simplex, file = paste0(direc, "/", filename, ".smplx"),
                 append = TRUE)
}
