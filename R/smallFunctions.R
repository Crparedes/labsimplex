# No need for documentations since these options are not exported to users
#----FUNCTIONS USED IN GENERATING NEW VERTICES---------------------------------
  #rf for remanent face and wv for wastebracket vertex
  reflect <- function(rf, wv){
    return((colSums(rf) / nrow(rf)) + ((colSums(rf) / nrow(rf)) - wv))
  }

  expandV <- function(rf, wv){
    return((colSums(rf) / nrow(rf)) + 2 * ((colSums(rf) / nrow(rf)) - wv))
  }

  contrRS <- function(rf, wv){
    return((colSums(rf) / nrow(rf)) + 0.5 * ((colSums(rf) / nrow(rf)) - wv))
  }

  contrWS <- function(rf, wv){
    return((colSums(rf) / nrow(rf)) - 0.5 * ((colSums(rf) / nrow(rf)) - wv))
  }

#----FUNCTIONS USED IN ERROR MANAGING------------------------------------------
  checkMain <- function(simplex) {
    if (class(simplex) != 'smplx') {
      stop("Argument simplex is expected to be 'smplx' class. Provided: ", class(simplex), ". ",
           "Use labsimplex() to generate a 'smplx' class object")
    }
  }

  checkCrit <- function(crit, lastQF, transf = FALSE) {
    if (class(crit) == "character") {
      if (crit == "max") {
        pos <- order(lastQF)  # Better at the last position
        qft <- lastQF
      } else {
        if (crit == "min") {
          pos <- order(lastQF ^ 2, decreasing = TRUE)  # Better at the last position
          qft <- 1 / (lastQF ^ 2)
        } else {
          stop("If criteria is not numeric, only 'max' or min' are accepted")
        }
      }
    }
    if (class(crit) == "numeric") {
      pos <- order((lastQF - crit) ^ 2, decreasing = TRUE)
      qft <- 1 / ((lastQF - crit) ^ 2)
    }
    if (transf) {
      return(qft)
    } else {
      return(pos)
    }
  }

#----FUNCTION USED IN SIMPLEX PRINTIG------------------------------------------
  shape <- function(x, simplex){
    if (is.null(x)) {
      x <- NA
    }
    if (length(x) < nrow(simplex$coords)){
      x <- c(x, rep(NA, nrow(simplex$coords) - length(x)))
    }
    return(x)
  }

#----FUNCTION FOR ASSIGNING CUALITY FUNCTION TO VERTICES-----------------------
  AssignQF <- function(simplex, qflv){
    if (class(qflv) != "numeric") {
      stop("Argument qf must be numeric")
    }
    simplex$qual.fun <- c(simplex$qual.fun, qflv)
    if (length(simplex$qual.fun) > nrow(simplex$coords)) {
      stop("The amount of vertices can not be smaller than the size of response vector.")
    }
    return(simplex)
  }






  redundant <- function(simplex, NV, counter) {

  }
