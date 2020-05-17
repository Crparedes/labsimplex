# No need for documentation since the function is not exported and will
#   not be aviliable to the user

LabelVertex <- function(simplex, crit, algor) {
  # Labels and orders the vertices of the simplex object based on the
  # quality function for vertices on the current simplex and the optimization
  # criteria

  if (length(simplex$qual.fun) != nrow(simplex$coords)) {
    stop("There must be a quality function value for each vertex:
        Number of vertices: ", nrow(simplex$coords),
        ", Number of quality function values: ", length(simplex$qual.fun))
  }

  #-------------------------------
  # Original (first) simplex:
  if (max(simplex$tim.ret) == 1) {
    rang   <- 1:nrow(simplex$coords)
    lastQF <- simplex$qual.fun[rang]
    #-----------------
    pos <- checkCrit(crit = crit, lastQF = lastQF)
    #------------------

    ver.lab <- rep(0, (simplex$dim + 1))
    # Convention: W -> -2, N -> -1, B -> 1, all others -> 0
    ver.lab[pos[1]] <- -2
    ver.lab[pos[2]] <- -1
    ver.lab[pos[length(pos)]] <- 1

    ver.lab2 <- rep('', (simplex$dim + 1))
    ver.lab2[pos[1]] <- 'W'
    ver.lab2[pos[2]] <- 'N'
    ver.lab2[pos[length(pos)]] <- 'B'

    simplex$coords       <- simplex$coords[order(ver.lab), ]
    simplex$vertex.label <- ver.lab2[order(ver.lab)]
    simplex$qual.fun     <- simplex$qual.fun[order(ver.lab)]
  } else {
    rang   <- (nrow(simplex$coords) - simplex$dim + 1):nrow(simplex$coords)
    lastQF <- simplex$qual.fun[rang]
    #-------------------
    pos <- checkCrit(crit = crit, lastQF = lastQF)
    #-------------------
    ver.lab <- rep(0, simplex$dim)
    # Convention: WasteB -> -2, N -> -1, B -> 1, all others -> 0
    ver.lab[pos[1]] <- -1
    ver.lab[pos[length(pos)]] <- 1
    coords <- rbind(simplex$coords[1:(nrow(simplex$coords) - simplex$dim), ],
                    simplex$coords[(nrow(simplex$coords) - simplex$dim + 1):
                                   nrow(simplex$coords), ][order(ver.lab), ])
    simplex$coords <- coords

    # Trimming the vector to make room for the new labels
    length(simplex$vertex.label) <- length(simplex$vertex.label) -
      simplex$dim + 1
    # Moving the previous 'N' to waste
    simplex$vertex.label[length(simplex$vertex.label)] <- 'W'

    ver.lab2 <- rep('', simplex$dim)
    ver.lab2[pos[1]] <- 'N'
    ver.lab2[pos[length(pos)]] <- 'B'
    simplex$vertex.label <- c(simplex$vertex.label, ver.lab2[order(ver.lab)])

    lsqf <- length(simplex$qual.fun)
    simplex$qual.fun[(lsqf -  simplex$dim + 1):lsqf] <-
      simplex$qual.fun[(lsqf -  simplex$dim + 1):lsqf][order(ver.lab)]
    simplex$vertex.nat[(lsqf -  simplex$dim + 1):lsqf] <-
      simplex$vertex.nat[(lsqf -  simplex$dim + 1):lsqf][order(ver.lab)]
  }
  return(simplex)
}
