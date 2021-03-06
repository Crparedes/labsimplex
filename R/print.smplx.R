#' S3 method \code{print} for simplex objects
#'
#' Prints simplex information.
#'
#' @param x           simplex (object of class to be printed
#' @param extended    logical, if \code{TRUE}, the object is printed as a
#'                    list containing all hiden elements
#' @param conventions logical, if \code{TRUE} (default), the conventions
#'                    used are printed
#' @param ...         more parameters passed to \code{\link[base]{print}}
#' @method print smplx
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Jesús Ágreda, \email{jagreda@@unal.edu.co}
#' @export
# S3 method for smplx class object

print.smplx <- function(x, extended = FALSE, conventions = TRUE, ...){
  if (extended) {
    class(x) <- "list"
    print(x)
  } else {
    if (is.null(x$coords)) x$coords <- matrix()

    NvertexSim <- x$dim + 1
    NvertexTot <- nrow(x$coords)
    if (x$P.eval) {
      FvertexAct <- NvertexTot - NvertexSim
    } else {
      FvertexAct <- NvertexTot - NvertexSim + 1
    }

    lab <- shape(x = x$vertex.label, simplex = x)
    QF  <- shape(x = x$qual.fun, simplex = x)
    nat <- shape(x = x$vertex.nat, simplex = x)
    row.names(x$coords) <- paste0(row.names(x$coords), ":")

    cat("Current simplex:\n")
    print(data.frame(x$coords[FvertexAct:NvertexTot, ], . = "|",
                     Response = QF[FvertexAct:NvertexTot],
                     Label = lab[FvertexAct:NvertexTot],
                     Nature = nat[FvertexAct:NvertexTot]))

    if (FvertexAct > 1) {
      cat("\nHistorical Vertices:\n")
      M <- x$coords[(FvertexAct - 1):1, ]
      if (FvertexAct == 2) {
        M <- t(M)
        rownames(M) <- rownames(x$coords)[1]
      }
      print(data.frame(M, . = "|",
                       Response = QF[(FvertexAct - 1):1],
                       Label = lab[(FvertexAct - 1):1],
                       Nature = nat[(FvertexAct - 1):1]))
    }

    if (conventions) {
      cat("\nConventions:\n")
      cat("    Labels:                    Nature:
       W: Worst or Wastebasket    S:  Starting
       N: Next to the worst       R:  Reflected
       B: Best                    E:  Expanded
                                  Cr: Contraction on the reflection side
       D: Disregarded             Cw: Contraction on the worst side

      Use print(..., conventions = FALSE) to disable conventions printing. \n")
    }
  }
}
