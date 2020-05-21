#' @name exampleSurfacesVisualization
#' @aliases cntr
#' @title Graphical representations of the example response surfaces
#'
#' @param length value
#' @param noise value
#' @param surface value
#' @param xlim value
#' @param ylim value
#' @param par  list with graphical parameters (\code{\link[graphics]{par}}).
#' @param ...  parameters to function \code{\link[graphics]{persp}}}
#' @inheritParams graphics::persp
#'
#' @note \code{funs} is a generic name for the functions documented.
#' \cr
#' If called, \code{funs} returns its own arguments.
#' @examples
#' prspctv(length = 45, noise = 0, surface = exampleSurfaceR2.2pks,
#'         ltheta = -120, expand = 0.85, xlab = 'Temperature (K)', ylab = 'pH', zlab = 'Yield (%)')
#' print(cntr(length = 200, noise = 0, surface = exampleSurfaceR2))
#' @rdname surfacesVisualization
#' @importFrom graphics persp
#' @importFrom grDevices colorRampPalette
#' @importFrom stats rnorm
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Jesús Ágreda, \email{jagreda@@unal.edu.co}
#' @export
prspctv <- function (surface, length = 45, noise = 0, x1lim = c(278, 365), x2lim = c(0, 14), par = NULL,
                     theta = 22, phi = 15, shade = 0.2, ticktype = "detailed", ...) {
  x1 <- seq(x1lim[1], x1lim[2], length = length)
  x2 <- seq(x2lim[1], x2lim[2], length = length)
  z <- outer(x1, x2, surface, noise = noise)

  colors  <- colorRampPalette(c("grey30", "white"))(100)
  z.facet.center <- (z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1] + z[-nrow(z), -ncol(z)])/4
  z.facet.range  <- cut(z.facet.center, 200)
  old.par <- par(no.readonly = T)
  if(!missing(par)) par(par)
  persp(x1, x2, z, col = colors[z.facet.range],
        theta = theta, phi = phi, ticktype = ticktype, lwd = 0.3, shade = shade, ...)
  par(old.par)
}

#' @rdname surfacesVisualization
#' @return \code{sum1(x,y)} returns x+y
#' @examples
#' x <- 5
#' @import ggplot2
#' @importFrom grDevices colorRampPalette
#' @importFrom stats rnorm
#' @export
cntr <- function (surface, length = 150, noise = 0, x1lim = c(278, 365), x2lim = c(0, 14)) {
  x1 <- seq(278, 365, length = length)
  x2 <- seq(0, 14, length = length)
  gg <- expand.grid(x1 = x1, x2 = x2)
  gg$z <- with(gg, surface(x1, x2, noise = noise))

  #brks <- cut(gg$z, breaks = seq(0, 100, len = 10))
  brks <- cut(gg$z, breaks = c(-20, seq(10, 90, 10)))
  brks <- gsub(",", " - ", brks, fixed = TRUE)
  gg$brks <- gsub("\\(|\\]", "", brks)  # reformat guide labels
  p <- ggplot(gg, aes(x1, x2)) + theme_bw() +
    geom_tile(aes(fill = brks)) + scale_fill_manual("Z", values = colorRampPalette(c("grey30", "white"))(11)) +
    scale_x_continuous(expand = c(0, 0), limits = c(278, 365)) + scale_y_continuous(expand = c(0, 0), limits = c(0, 14)) +
    theme(legend.position = 'none', axis.text = element_text(size = 12, color = 'black'),
          axis.title = element_text(size = 12, color = 'black'), plot.margin = unit(c(0.5, 0.5, 0.1, 0.1), "cm"),
          panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    labs(x = 'Temperature (K)', y = 'pH')
  return(p)
}

#' Plots a 3D perspective of two variables response surfaces
#'
#' The function plots a perspective 3D plot for the example response surfaces included in the package
#' @param centroid    value
#' @param stepsize    value
#' @param algor       value
#' @param experiments value
#' @param length      value
#' @param noise       value
#' @param surface     value
#' @import ggplot2
#' @export

exampleOptimization <- function (surface, simplex = NULL, N = 2, centroid = c(7, 3.40),
                                 stepsize = c(0.6, 10), algor = 'fixed',
                                 experiments = 17, length = 200, noise = 0,
                                 var.name = c('pH', 'Temperature')) {
  ii1 <- FALSE
  n = 0
  if (missing(simplex)) {
    ii1 <- TRUE
    simplex <- labsimplex(N = N, centroid = centroid, stepsize = stepsize,
                          var.name = var.name)
    n = N + 1
  }
  for (ii in 1:(experiments - n)){
    if (ii == 1 && ii1) {
      invisible(capture.output(
        simplex <- generateVertex(simplex = simplex, algor = algor,
                                  qflv = surface(x1 = simplex$coords[, 2],
                                                 x2 = simplex$coords[, 1],
                                                 noise = noise))))
    } else {
      invisible(capture.output(
        simplex <- generateVertex(simplex = simplex, algor = algor,
                                  qflv = surface(x1 = simplex$coords[nrow(simplex$coords), 2],
                                                 x2 = simplex$coords[nrow(simplex$coords), 1],
                                                 noise = noise))))
    }
  }
  return(simplex)
}

#' Adds simplex movements to a response surface contour
#'
#' The function complements the \code{\link[ggplot2]{ggplot} produced by \code{\link{cntr}} by
#' adding the movements made for a simplex object (generally produced by \code{\link{exampleOptimization}})
#' @param p a \code{\link[ggplot2]{ggplot} object
#' @param simplex simplex
#' @seealso \code{\link{cntr}} \code{\link{exampleOptimization}}
#' @example
#' simplex <- exampleOptimization(surface = exampleSurfaceR2, centroid = c(7, 340), stepsize = c(1.2, 15))
#' (p <- cntr(surface = exampleSurfaceR2))
#' p <- addSimplex2Surface(p = p, simplex = simplex)
#' print(p)
#' @importFrom  ggplot2 geom_segment aes
#' @export
addSimplex2Surface <- function (p, simplex) {
  V.pos <- as.numeric(gsub("Vertex.", "", row.names(simplex$coords)))
  x <- xend <- y <- yend <- vector()
  for (ii in 1:length(simplex$families)) {
    for (jj in 1:(length(simplex$families[[ii]]) - 1)) {
      for (kk in (jj + 1):length(simplex$families[[ii]])) {
        jj. <- which(simplex$families[[ii]][jj] == V.pos)
        kk. <- which(simplex$families[[ii]][kk] == V.pos)
        x    <- c(x, simplex$coords[jj., 2])
        xend <- c(xend, simplex$coords[kk., 2])
        y    <- c(y, simplex$coords[jj., 1])
        yend <- c(yend, simplex$coords[kk., 1])
      }
    }
  }
  p <- p + geom_segment(data = data.frame(x = x, xend = xend, y = y, yend = yend),
                          aes(x = x, xend = xend, y = y, yend = yend)) +
          geom_point(data = data.frame(x = simplex$coords[, 2], y = simplex$coords[, 1]),
                    aes(x = x, y = y), shape = 21, size = 3, fill = 'white')
  return(p)
}
