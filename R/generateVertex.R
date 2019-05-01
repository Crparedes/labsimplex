#' Generates the new vertex of a simplex
#'
#' Gives the coordinates for the new vertex that must be experimented
#' based on the quality function values for vertices on the current simplex
#' and considering the optimization criteria.
#'
#' When minimization is the criteria, the algorithm will tend to approach zero.
#' If negative responses are possible and the most negative value is desired,
#' response values sign must be changed manually before entering the information.
#'
#' @param  simplex   \code{'chsmplx'} type object containig most information
#'                   of simplex.
#' @param  qflv      value of the quality function for the last vertex
#'                   (or vertices if it is the first simplex).
#' @param  crit      optimization criteria indicating if the goal is maximize
#'                   (\code{"max"}) or minimize (\code{"min"}) the quality
#'                   function. It can also be a numeric value to which the
#'                   quality function is supposed to approach.
#' @param  algor     algorithm to be followed in the vertex generation.
#'                   \code{"fixed"} for a fixed size simplex following
#'                   Spendley (1962) algorithm or \code{"variable"} for a
#'                   variable size simplex following Nelder and Mead (1965)
#'                   algorithm
#' @param  overwrite logical argument indicating if the simplex must be
#'                   automatically overwrited
#' @return A 'chsmplx' type object with the information of the simplex
#'        including the conditions for the new experiment to be permormed.
#' @examples
#' simplex3D <- labsimplex(N = 3)
#' simplex3D <- generateVertex(simplex = simplex3D, qflv = rnorm(4))
#' \dontrun{
#' ## Optional form:
#' ## After obtaining quality function for the last vertex generated:
#' NV <- rnorm(1)
#' generateVertex(simplex = simplex3D, qflv = NV, overwrite = TRUE)
#' @export

generateVertex <- function(simplex, qflv = NULL, crit = "max", algor = "fixed",
                      overwrite = FALSE){

  name <- deparse(substitute(simplex))

  # Error handling
  checkMain(simplex)
  if (!missing(qflv)){
    simplex <- AssignQF(simplex = simplex, qflv = qflv)
  }
  if (algor != "fixed" && algor != "variable") {
    stop("Algorithm must be set to 'fixed' or 'variable'")
  }

  #--------------------------------
  # Ordering the vertices that compose current simplex
  TNV  <- nrow(simplex$coords) #Total number of vertices
  rang <- (TNV - simplex$dim):TNV

  #First simplex
  if (max(simplex$tim.ret) == 1) {
    simplex <- LabelVertex(simplex = simplex, crit = crit, algor = algor)
    simplex$tim.ret[rang[-1]] <- simplex$tim.ret[rang[-1]] + 1
    NewVert <- reflect(rf = simplex$coords[rang[-1], ], wv = simplex$coords[rang[1], ])
    simplex$vertex.nat <- c(simplex$vertex.nat, "R")
    form <- 0 # Normal ordering of vertex coordinates
  } else {
    if (algor == 'fixed') {
      simplex <- LabelVertex(simplex = simplex, crit = crit, algor = algor)
      NewVert <- reflect(rf = simplex$coords[rang[-1], ], wv = simplex$coords[(rang[1]), ])
      simplex$vertex.nat <- c(simplex$vertex.nat, "R")
      form <- 0 # Normal ordering of vertex coordinates
    } else {
      # Variable size algorithm
      # The next transformation allows to minimization and other optimization criteria in variable size decisions
      qft <- checkCrit(crit = crit, lastQF = simplex$qual.fun, transf = TRUE)

      # Is there a pending evaluation of a expansion vertex?
      if (simplex$P.eval) {
        # This trick allows moving the disregarded vertex to the bottom of the matrix including its name.
        # depending on the choice, the redundant vertices must be deleted
        simplex$coords  <- rbind(simplex$coords[(TNV - 1):TNV, ], simplex$coords[1:TNV, ])

        if (qft[TNV] >= qft[rang[(simplex$dim - 1)]]) { # Expansion is accepted
          simplex$coords  <- simplex$coords[-c(2, (TNV + 1)), ]

          simplex$qual.fun     <- c(simplex$qual.fun[(TNV - 1)], simplex$qual.fun[- (TNV - 1)])
          simplex$vertex.label <- c(simplex$vertex.label[(TNV - 1)], simplex$vertex.label[- (TNV - 1)])
          simplex$tim.ret      <- c(simplex$tim.ret[(TNV - 1)], simplex$tim.ret[- (TNV - 1)])
          simplex$vertex.nat   <- c(simplex$vertex.nat[(TNV - 1)], simplex$vertex.nat[- (TNV - 1)])
        } else { #Expansion is declined
          simplex$coords  <- simplex$coords[-c(1, (TNV + 2)), ]

          simplex$qual.fun     <- c(simplex$qual.fun[TNV], simplex$qual.fun[- TNV])
          simplex$vertex.label <- c(simplex$vertex.label[TNV], simplex$vertex.label[- TNV])
          simplex$tim.ret      <- c(simplex$tim.ret[TNV], simplex$tim.ret[- TNV])
          simplex$vertex.nat   <- c(simplex$vertex.nat[TNV], simplex$vertex.nat[- TNV])
        }
        simplex$vertex.label[1] <- "D"
        simplex$P.eval <- FALSE
        qft <- checkCrit(crit = crit, lastQF = simplex$qual.fun, transf = TRUE)

        simplex <- LabelVertex(simplex = simplex, crit = crit, algor = algor)
        NewVert <- reflect(rf = simplex$coords[rang[-1], ], wv = simplex$coords[(rang[1]), ])
        simplex$vertex.nat <- c(simplex$vertex.nat, "R")
        form <- 0 #Normal ordering of vertex coordinates
      } else {
        if (simplex$vertex.nat[TNV] %in% c('Cr', 'Cw')) {# Avoid expansions/contractions after a contraction is made
          simplex <- LabelVertex(simplex = simplex, crit = crit, algor = algor)
          NewVert <- reflect(rf = simplex$coords[rang[-1], ], wv = simplex$coords[(rang[1]), ])
          simplex$vertex.nat <- c(simplex$vertex.nat, "R")
          form <- 0 #Normal ordering of vertex coordinates
          } else {
          # N <= R <= B, Case 1
          if (qft[TNV] >= qft[rang[1]] && qft[TNV] <= qft[rang[(simplex$dim)]]) {
            simplex <- LabelVertex(simplex = simplex, crit = crit, algor = algor)
            NewVert <- reflect(rf = simplex$coords[rang[-1], ], wv = simplex$coords[(rang[1]), ])
            simplex$vertex.nat <- c(simplex$vertex.nat, "R")
            form <- 0 #Normal ordering of vertex coordinates
          }
          # R > B, Case 2
          if (qft[TNV] > qft[rang[simplex$dim]]) {
            NewVert <- expandV(rf = simplex$coords[rang[-(simplex$dim + 1)], ], wv = simplex$coords[(rang[1] - 1), ])
            simplex$vertex.nat <- c(simplex$vertex.nat, "E")
            simplex$vertex.label[(TNV - 1):TNV] <- "pending"
            simplex$P.eval <- TRUE
            form <- 0 #Normal ordering of vertex coordinates
          }
          # R < N, Case 3
          if (qft[TNV] < qft[rang[1]]) {
            form <- 1 #Anormal ordering of vertex coordinates
            if (qft[TNV] >= qft[(rang[1] - 1)]) {
              NewVert <- contrRS(rf = simplex$coords[rang[-(simplex$dim + 1)], ], wv = simplex$coords[(rang[1] - 1), ])
              simplex$vertex.nat <- c(simplex$vertex.nat, "Cr")
            }
            if (qft[TNV] < qft[(rang[1] - 1)]) {
              NewVert <- contrWS(rf = simplex$coords[rang[-(simplex$dim + 1)], ], wv = simplex$coords[(rang[1] - 1), ])
              simplex$vertex.nat <- c(simplex$vertex.nat, "Cw")
            }
          }
        }
      }
    }
    simplex$tim.ret[rang[-1]] <- simplex$tim.ret[rang[-1]] + 1
  }

  simplex$tim.ret <- c(simplex$tim.ret, 1)

  if (form == 0) {
    simplex$coords  <- rbind(simplex$coords, NewVert)
    row.names(simplex$coords)[nrow(simplex$coords)] <- paste0("Vertex.", nrow(simplex$coords))
  }
  if (form == 1) {
    #This trick allows moving the disregarded vertex to the bottom of the matrix including its name.
    simplex$coords  <- rbind(simplex$coords[(TNV - 1):TNV, ], simplex$coords[1:(TNV - 1), ], NewVert)
    simplex$coords  <- simplex$coords[-1, ]

    simplex$qual.fun     <- c(simplex$qual.fun[TNV], simplex$qual.fun[- TNV])
    simplex$vertex.label <- c(simplex$vertex.label[TNV], simplex$vertex.label[- TNV])
    simplex$vertex.label[1] <- "D"
    simplex$tim.ret      <- c(simplex$tim.ret[TNV], simplex$tim.ret[- TNV])
    simplex$vertex.nat   <- c(simplex$vertex.nat[TNV], simplex$vertex.nat[- TNV])
    row.names(simplex$coords)[nrow(simplex$coords)] <- paste0("Vertex.", nrow(simplex$coords))
  }

  message("New vertex to be evaluated: ")
  print(simplex$coords[nrow(simplex$coords), ])
  if (overwrite) {
    assign(name, simplex, envir = parent.frame())
  } else {
    cat("\n\n")
    return(simplex)
  }
}
