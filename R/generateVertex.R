#' Generates the new vertex of a simplex optimization
#'
#' Gives the coordinates for the new vertex that must be performed
#' based on the responses for the vertexes on the current simplex
#' and considering the optimization criteria.
#'
#' When minimization is the criteria, the algorithm will tend to approach zero.
#' If negative responses are possible and the most negative value is desired,
#' a very large negative number must be provided in \code{crit} parameter.
#'
#' @param  simplex   object of class \code{smplx} with the simplex
#'                   information. See \code{\link{labsimplex}}
#' @param  qflv      response for the vertex (or vertexes) without responses
#' @param  crit      optimization criteria indicating if the goal is maximize
#'                   (\code{'max'}) or minimize (\code{'min'}) the response.
#'                   It can also be a numeric value to which the
#'                   response is supposed to approach
#' @param  algor     algorithm to be followed in the vertex generation.
#'                   \code{'fixed'} for a fixed step-size simplex
#'                   or \code{'variable'} for a variable step-size simplex
#' @param  overwrite logical argument. If \code{TRUE}, the output simplex will
#'                   replace the one provided in the \code{simplex} parameter.
#'                   Default \code{overwrite = FALSE}
#' @return An object of class \code{smplx} with the new simplex information
#'         including the conditions for the new experiment to be permormed.
#' @examples
#'   simplex <- labsimplex(n = 3, centroid = c(320, 7, 0.4),
#'                         stepsize = c(35, 2, 0.3))
#'   ## The experiments must be performed and the responses passed to qflv.
#'   ## Here we get the responses by using an example response surface
#'   ## included in the package:
#'   ##
#'   ## Initially, the response must be provided for all the vertexes
#'   response <- exampleSurfaceR3(x1 = simplex$coords[, 1],
#'                                x2 = simplex$coords[, 2],
#'                                x3 = simplex$coords[, 3])
#'   simplex <- generateVertex(simplex = simplex, qflv = response)
#'
#'   ## After this, the last vertex is the only one that must be evaluated
#'   response <- exampleSurfaceR3(x1 = simplex$coords[nrow(simplex$coords), 1],
#'                                x2 = simplex$coords[nrow(simplex$coords), 2],
#'                                x3 = simplex$coords[nrow(simplex$coords), 3])
#'   simplex <- generateVertex(simplex = simplex, qflv = response)
#'
#'   ## Alternatively the simplex object can overwrite the older one:
#'   generateVertex(simplex = simplex, qflv = response, overwrite = TRUE)
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Jesús Ágreda, \email{jagreda@@unal.edu.co}
#' @export

generateVertex <- function(simplex, qflv = NULL, crit = "max", algor = "fixed",
                           overwrite = FALSE){
  name <- deparse(substitute(simplex))
  # Error handling
  checkMain(simplex)
  if (!missing(qflv)) simplex <- AssignQF(simplex = simplex, qflv = as.numeric(qflv))
  if (algor != "fixed" && algor != "variable") {
    stop("Algorithm must be set to 'fixed' or 'variable'")
  }

  #--------------------------------
  # Ordering the vertices that compose current simplex
  TNV  <- nrow(simplex$coords) #Total number of vertices
  rang <- (TNV - simplex$dim):TNV
  wVertex <- FALSE #Families anomalities

  P.evalInv <- TRUE
  #First simplex
  if (max(simplex$tim.ret) == 1) {
    simplex <- LabelVertex(simplex = simplex, crit = crit, algor = algor)
    simplex$tim.ret[rang[-1]] <- simplex$tim.ret[rang[-1]] + 1
    NewVert <- reflect(rf = simplex$coords[rang[-1], ],
                       wv = simplex$coords[rang[1], ])
    simplex$vertex.nat <- c(simplex$vertex.nat, "R")
    form <- 0 # Normal ordering of vertex coordinates
  } else {
    if (algor == 'fixed') {
      simplex <- LabelVertex(simplex = simplex, crit = crit, algor = algor)
      NewVert <- reflect(rf = simplex$coords[rang[-1], ],
                         wv = simplex$coords[(rang[1]), ])
      simplex$vertex.nat <- c(simplex$vertex.nat, "R")
      form <- 0 # Normal ordering of vertex coordinates
    } else {
      # Variable size algorithm
      # The next transformation allows to minimization and other optimization
      #   criteria in variable size decisions
      qft <- checkCrit(crit = crit, lastQF = simplex$qual.fun, transf = TRUE)

      # Is there a pending evaluation of a expansion vertex?
      if (simplex$P.eval) {
        P.evalInv <- FALSE
        # This trick allows moving the disregarded vertex to the bottom of
        #  the matrix including its name.
        simplex$coords  <- rbind(simplex$coords[(TNV - 1):TNV, ],
                                 simplex$coords[1:TNV, ])

        if (qft[TNV] >= qft[rang[(simplex$dim - 1)]]) {
          # Expansion is accepted
          simplex$coords  <- simplex$coords[-c(2, (TNV + 1)), ]

          simplex$qual.fun     <- c(simplex$qual.fun[(TNV - 1)],
                                    simplex$qual.fun[- (TNV - 1)])
          simplex$vertex.label <- c(simplex$vertex.label[(TNV - 1)],
                                    simplex$vertex.label[- (TNV - 1)])
          simplex$tim.ret      <- c(simplex$tim.ret[(TNV - 1)],
                                    simplex$tim.ret[- (TNV - 1)])
          simplex$vertex.nat   <- c(simplex$vertex.nat[(TNV - 1)],
                                    simplex$vertex.nat[- (TNV - 1)])
        } else { #Expansion is declined
          simplex$coords  <- simplex$coords[-c(1, (TNV + 2)), ]

          simplex$qual.fun     <- c(simplex$qual.fun[TNV],
                                    simplex$qual.fun[- TNV])
          simplex$vertex.label <- c(simplex$vertex.label[TNV],
                                    simplex$vertex.label[- TNV])
          simplex$tim.ret      <- c(simplex$tim.ret[TNV],
                                    simplex$tim.ret[- TNV])
          simplex$vertex.nat   <- c(simplex$vertex.nat[TNV],
                                    simplex$vertex.nat[- TNV])
        }
        simplex$vertex.label[1] <- "D"
        simplex$P.eval <- FALSE
        qft <- checkCrit(crit = crit, lastQF = simplex$qual.fun, transf = TRUE)

        simplex <- LabelVertex(simplex = simplex, crit = crit, algor = algor)
        NewVert <- reflect(rf = simplex$coords[rang[-1], ],
                           wv = simplex$coords[(rang[1]), ])
        simplex$vertex.nat <- c(simplex$vertex.nat, "R")
        form <- 0 #Normal ordering of vertex coordinates
      } else {
        if (simplex$vertex.nat[TNV] %in% c('Cr', 'Cw')) {
          # Avoid expansions/contractions after a contraction is made
          simplex <- LabelVertex(simplex = simplex, crit = crit, algor = algor)
          NewVert <- reflect(rf = simplex$coords[rang[-1], ],
                             wv = simplex$coords[(rang[1]), ])
          simplex$vertex.nat <- c(simplex$vertex.nat, "R")
          form <- 0 #Normal ordering of vertex coordinates
          } else {
          # N <= R <= B, Case 1
          if (qft[TNV] >= qft[rang[1]] && qft[TNV] <=
                qft[rang[(simplex$dim)]]) {
            simplex <- LabelVertex(simplex = simplex, crit = crit,
                                   algor = algor)
            NewVert <- reflect(rf = simplex$coords[rang[-1], ],
                               wv = simplex$coords[(rang[1]), ])
            simplex$vertex.nat <- c(simplex$vertex.nat, "R")
            form <- 0 #Normal ordering of vertex coordinates
          }
          # R > B, Case 2
          if (qft[TNV] > qft[rang[simplex$dim]]) {
            NewVert <- expandV(rf = simplex$coords[rang[-(simplex$dim + 1)], ],
                               wv = simplex$coords[(rang[1] - 1), ])
            simplex$vertex.nat <- c(simplex$vertex.nat, "E")
            simplex$vertex.label[(TNV - 1):TNV] <- "pending"
            simplex$P.eval <- TRUE
            form <- 0 #Normal ordering of vertex coordinates
            wVertex <- TRUE
          }
          # R < N, Case 3
          if (qft[TNV] < qft[rang[1]]) {
            wVertex <- TRUE
            form <- 1 #Anormal ordering of vertex coordinates
            if (qft[TNV] >= qft[(rang[1] - 1)]) {
              NewVert <- contrRS(rf = simplex$coords[rang[-(simplex$dim + 1)], ],
                                 wv = simplex$coords[(rang[1] - 1), ])
              simplex$vertex.nat <- c(simplex$vertex.nat, "Cr")
            }
            if (qft[TNV] < qft[(rang[1] - 1)]) {
              NewVert <- contrWS(rf = simplex$coords[rang[-(simplex$dim + 1)], ],
                                 wv = simplex$coords[(rang[1] - 1), ])
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
    row.names(simplex$coords)[nrow(simplex$coords)] <-
      paste0("Vertex.", nrow(simplex$coords))
  }
  if (form == 1) {
    # This trick allows moving the disregarded vertex to the bottom of
    #   the matrix including its name.
    simplex$coords  <- rbind(simplex$coords[(TNV - 1):TNV, ],
                             simplex$coords[1:(TNV - 1), ], NewVert)
    simplex$coords  <- simplex$coords[-1, ]

    simplex$qual.fun     <- c(simplex$qual.fun[TNV], simplex$qual.fun[- TNV])
    simplex$vertex.label <- c(simplex$vertex.label[TNV],
                              simplex$vertex.label[- TNV])
    simplex$vertex.label[1] <- "D"
    simplex$tim.ret      <- c(simplex$tim.ret[TNV], simplex$tim.ret[- TNV])
    simplex$vertex.nat   <- c(simplex$vertex.nat[TNV],
                              simplex$vertex.nat[- TNV])
    row.names(simplex$coords)[nrow(simplex$coords)] <-
      paste0("Vertex.", nrow(simplex$coords))
  }

  if (simplex$P.eval) {
    rang[(length(rang) - 1)] <- rang[(length(rang) - simplex$dim)] - 1
  }

  AcVertexes <- as.numeric(gsub("Vertex.", "",
                                row.names(simplex$coords)))[(rang + 1)]

  simplex$families[[(length(simplex$families) + 1)]] <- AcVertexes

  message("New vertex to be evaluated: \n")
  print(simplex$coords[nrow(simplex$coords), ])
  if (overwrite) {
    assign(name, simplex, envir = parent.frame())
  } else {
    message("\n")
    return(simplex)
  }
}
