#' Prints given simplex (a \code{smplx} class object)
#'
#' Prints given simplex information.
#'
#' @param simplex     simplex object to be printed
#' @param extended    logical, if \code{TRUE}, the object is printed as a list containing all elements
#' @param conventions logical, if \code{TRUE} (default), the conventions used are printed
#'
#' @method print smplx
#' @export
# S3 method for smplx class object

print.smplx <- function(simplex, extended = FALSE, conventions = TRUE){
  if (extended) {
    class(simplex) <- "list"
    print(simplex)
  } else {
    if (is.null(simplex$coords)) {
      simplex$coords <- matrix()
    }

    NvertexSim <- simplex$dim + 1
    NvertexTot <- nrow(simplex$coords)
    if (simplex$P.eval) {
      FvertexAct <- NvertexTot - NvertexSim
    } else {
      FvertexAct <- NvertexTot - NvertexSim + 1
    }

    lab <- shape(x = simplex$vertex.label, simplex = simplex)
    QF  <- shape(x = simplex$qual.fun, simplex = simplex)
    nat <- shape(x = simplex$vertex.nat, simplex = simplex)
    row.names(simplex$coords) <- paste0(row.names(simplex$coords), ":")

    message("\nCurrent simplex:")
    print(data.frame(simplex$coords[NvertexTot:FvertexAct, ], . = "|",
                     Response = QF[NvertexTot:FvertexAct],
                     Label = lab[NvertexTot:FvertexAct],
                     Nature = nat[NvertexTot:FvertexAct]))

    if (FvertexAct > 1) {
      message("\nHistorical vertexes:")
      M <- simplex$coords[(FvertexAct - 1):1, ]
      if (FvertexAct == 2) {
        M <- t(M)
        rownames(M) <- rownames(simplex$coords)[1]
      }
      print(data.frame(M, . = "|",
                       Response = QF[(FvertexAct - 1):1],
                       Label = lab[(FvertexAct - 1):1],
                       Nature = nat[(FvertexAct - 1):1]))
    }

    message("\nConventions:")
    cat("    Labels:                    Nature:
      W: Worst or Wastebasket    S:  Starting
      N: Next to the worst       R:  Reflected
      B: Best                    E:  Expanded
                                 Cr: Contraction on the reflection side
      D: Disregarded             Cw: Contraction on the worst side

  Use print(..., conventions = FALSE) to disable conventions printing. \n")

    }
}
