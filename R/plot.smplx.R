#' Makes a two dimentional plot of a simplex object.
#'
#' The function generates a 2D plot for a \code{smplx} class object having
#' dimensionality of at least 2. When dimensionality is higher than 2, the
#' function plots a 2D proyection of selected dimensions.
#'
#' For 3D representations of simplex with dimensionality higher than 2 use
#' \code{\link{plotSimplex3D}}
#' @param  x \code{smplx} class object containig the coordinates of
#'                 the vertexes.
#' @param  sel.dim numeric or char vector for variables to be considered when
#'                 simplex dimensionality is higher than 2. If \code{numeric}
#'                 form it must contain dimensions ordinal number. If
#'                 \code{char}, it must contain dimensions names.
#' @param  all.ver logical. Should all vertex be plotted? If \code{FALSE}
#'                 draws only vertexes corresponding to current simplex.
#' @param  all.lin logical. Should all lines be drawn? If \code{FALSE} draws
#'                 only last simplex.
#' @param  expand  logical. Should the plot scales be expanded?
#' @param  exp.fac expansion factor used when \code{expand = TRUE}.
#' @param  ...     other graphical parameters used in plot()
#' @return 2D proyection of the simplex coordinates
#' @examples
#'   plot(x = labsimplex(N = 2))
#'   plot(x = labsimplex(N = 2), expand = FALSE)
#'
#'   plot(x = labsimplex(N = 8))
#'   plot(x = labsimplex(N = 8), sel.dim = c(3, 4))
#'
#'   ## Simulation of the real proccess where a simplex is made and QF measured,
#'   set.seed(12)
#'   simplex2D <- labsimplex(N = 2)
#'   plot(x = simplex2D)
#'   generateVertex(simplex = simplex2D, qflv = rnorm(3), overwrite = TRUE)
#'   plot(x = simplex2D)
#'   generateVertex(simplex = simplex2D, qflv = rnorm(1), overwrite = TRUE)
#'   plot(x = simplex2D)
#'   generateVertex(simplex = simplex2D, qflv = rnorm(1), overwrite = TRUE)
#'   plot(x = simplex2D)
#' @importFrom graphics lines par plot segments title
#' @importFrom grDevices dev.off pdf
#' @method plot smplx
#' @export
# S3 method for smplx class object

plot.smplx <- function(x, sel.dim = NULL, all.ver = TRUE,
                          all.lin = TRUE, expand = TRUE,
                          exp.fac = 1.5, ...){

  # Error handling
  checkMain(simplex = x)
  if (x$dim < 2){
    stop("Simplex dimension must be at least 2")
  }
  if (!missing(sel.dim)) {
    if (length(sel.dim) != 2) {
      stop("Only 2 coordinates can be plotted. Length of sdim vector differs from 2: ",
             length(sel.dim))
      }
  } else {
    var.plt <- 1:2
  }

  if (x$dim > 2) {
    if (missing(sel.dim)) {
      message("No selected dimensions for ploting. Default is the first two ones.")
    } else {
      if (is.numeric(sel.dim)) {
        var.plt <- sel.dim
        if (any(var.plt > x$dim)){
          stop("At least one ingresed variable is not available in data")
        }
      } else {
        var.plt <- (1:ncol(x$coords))[!is.na(match(colnames(x$coords), sel.dim))]
      }
      if (length(var.plt) != 2) {
        stop("At least one ingresed variable is not available in data")
      }
    }
  }

  #opar <- par(oma = par()$oma, mar = par()$mar)
  #par(oma = c(5, 4, 0, 0) + 0.1, mar = c(0, 0, 1, 1) + 0.1)
  xlab <- dimnames(x$coords)[[2]][var.plt[1]]
  ylab <- dimnames(x$coords)[[2]][var.plt[2]]
  if (all.ver) {
    pdf(file = NULL)
    plot(x$coord[, var.plt[1]], x$coord[, var.plt[2]], pch = 16, ...)
    if (expand) {
      xexp <- c(par("usr")[1] - 0.5 * diff(par("usr")[1:2]) * exp.fac,
                par("usr")[2] + 0.5 * diff(par("usr")[1:2]) * exp.fac)
      yexp <- c(par("usr")[3] - 0.5 * diff(par("usr")[3:4]) * exp.fac,
                par("usr")[4] + 0.5 * diff(par("usr")[3:4]) * exp.fac)
      dev.off()
      plot(x$coord[, var.plt[1]], x$coord[, var.plt[2]], xlim = xexp, ylim = yexp, pch = 16,
           xlab = xlab, ylab = ylab, ...)
    } else {
      dev.off()
      plot(x$coord[, var.plt[1]], x$coord[, var.plt[2]], pch = 16,
           xlab = xlab, ylab = ylab, ...)
    }
    #title(xlab = dimnames(x$coords)[[2]][var.plt[1]],
    #      ylab = dimnames(x$coords)[[2]][var.plt[2]],
    #      outer = TRUE, line = 3)
  } else {
    plot(x$coord[(nrow(x$coords) - x$dim):nrow(x$coords), var.plt[1]],
         x$coord[(nrow(x$coords) - x$dim):nrow(x$coords), var.plt[2]], pch = 16,
         xlab = xlab, ylab = ylab, ...)
    if (expand) {
      xexp <- c(par("usr")[1] - 0.5 * diff(par("usr")[1:2]) * exp.fac,
                par("usr")[2] + 0.5 * diff(par("usr")[1:2]) * exp.fac)
      yexp <- c(par("usr")[3] - 0.5 * diff(par("usr")[3:4]) * exp.fac,
                par("usr")[4] + 0.5 * diff(par("usr")[3:4]) * exp.fac)
      plot(x$coord[(nrow(x$coords) - x$dim):nrow(x$coords), var.plt[1]],
           x$coord[(nrow(x$coords) - x$dim):nrow(x$coords), var.plt[2]],
           xlim = xexp, ylim = yexp, pch = 16,
           xlab = xlab, ylab = ylab, ...)
    }
    #title(xlab = dimnames(x$coords)[[2]][var.plt[1]],
    #      ylab = dimnames(x$coords)[[2]][var.plt[2]], outer = TRUE, line = 3)
  }

  if (all.lin) {
    for (ii in 1:(nrow(x$coords) - x$dim)){
      for (jj in (ii+1):(ii + x$dim)){
        lines(x$coords[c(ii, jj),  var.plt[1]],
              x$coords[c(ii, jj),  var.plt[2]], col = "grey")
      }
    }
  }
  for (ii in (nrow(x$coords) - x$dim):nrow(x$coords)){
    for (jj in ii:nrow(x$coords)){
      lines(x$coords[c(ii, jj),  var.plt[1]],
            x$coords[c(ii, jj),  var.plt[2]], col = "blue")
    }
  }

  #par(oma = opar$oma, mar = opar$mar)
}

