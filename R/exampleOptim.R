#' 3D perspective plot of example response surfaces
#'
#' @description Plots a \code{\link[graphics]{persp}} plot of the bivariated
#'   example response surfaces included in the package.
#' @param surface example response surface to be used. See
#'   \code{\link{exampleSurfaceR2}} and \code{\link{exampleSurfaceR2.2pks}}.
#' @param length  number of levels to use in each explanatory variables
#' @param noise   absolute noise to be included in the results
#' @param par     list with graphical parameters (\code{\link[graphics]{par}}).
#' @param x1lim   limits for the first variable (temperature in
#'   \code{\link{exampleSurfaceR2}} and \code{\link{exampleSurfaceR2.2pks}})
#' @param x2lim   limits for the second variable (pH in
#'   \code{\link{exampleSurfaceR2}} and \code{\link{exampleSurfaceR2.2pks}})
#' @inheritParams graphics::persp
#' @examples
#' prspctv(surface = exampleSurfaceR2.2pks)
#' prspctv(surface = exampleSurfaceR2.2pks, theta = 35, phi = 25,
#'         expand = 0.75, xlab = 'Temperature (K)', ylab = 'pH',
#'         zlab = 'Yield (%)')
#' @importFrom graphics persp
#' @importFrom grDevices colorRampPalette
#' @importFrom stats rnorm
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Jesús Ágreda, \email{jagreda@@unal.edu.co}
#' @export
prspctv <- function (surface, length = 45, noise = 0, x1lim = c(278, 365),
                     x2lim = c(0, 14), par = NULL, theta = 22, phi = 15,
                     shade = 0.2, ticktype = "detailed", ...) {
  x1 <- seq(x1lim[1], x1lim[2], length = length)
  x2 <- seq(x2lim[1], x2lim[2], length = length)
  z <- outer(x1, x2, surface, noise = noise)

  colors  <- colorRampPalette(c("grey30", "white"))(100)
  z.facet.center <- (z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1] +
                       z[-nrow(z), -ncol(z)]) / 4
  z.facet.range  <- cut(z.facet.center, 200)
  old.par <- par(no.readonly = T)
  if(!missing(par)) par(par)
  persp(x1, x2, z, col = colors[z.facet.range], ticktype = ticktype,
        theta = theta, phi = phi, lwd = 0.3, shade = shade, ...)
  par(old.par)
}

#' Contour plot of example response surfaces
#'
#' @description Plots a \code{\link[ggplot2]{ggplot}} with the contour of the
#'   bivariated example response surfaces included in the package.
#' @inheritParams prspctv
#' @examples
#' p <- cntr(surface = exampleSurfaceR2, length = 200)
#' print(p)
#' @import ggplot2
#' @importFrom grDevices colorRampPalette
#' @importFrom stats rnorm
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Jesús Ágreda, \email{jagreda@@unal.edu.co}
#' @export
cntr <- function (surface, length = 150, noise = 0, x1lim = c(278, 365),
                  x2lim = c(0, 14)) {
  x1 <- seq(278, 365, length = length)
  x2 <- seq(0, 14, length = length)
  gg <- expand.grid(x1 = x1, x2 = x2)
  gg$z <- with(gg, surface(x1, x2, noise = noise))

  #brks <- cut(gg$z, breaks = seq(0, 100, len = 10))
  brks <- cut(gg$z, breaks = c(-20, seq(10, 90, 10)))
  brks <- gsub(",", " - ", brks, fixed = TRUE)
  gg$brks <- gsub("\\(|\\]", "", brks)  # reformat guide labels
  p <- ggplot(gg, aes(x1, x2)) + theme_bw() + geom_tile(aes(fill = brks)) +
    scale_fill_manual("Z",
                      values = colorRampPalette(c("grey30", "white"))(11)) +
    scale_x_continuous(expand = c(0, 0), limits = x1lim) +
    scale_y_continuous(expand = c(0, 0), limits = x2lim) +
    theme(legend.position = 'none',
          axis.text = element_text(size = 12, color = 'black'),
          axis.title = element_text(size = 12, color = 'black'),
          plot.margin = unit(c(0.5, 0.5, 0.1, 0.1), "cm"),
          panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black")) +
    labs(x = 'Temperature (K)', y = 'pH')
  return(p)
}

#' Performs a complete simplex optimization
#'
#' @description The function plots a perspective 3D plot for the example response surfaces included in the package
#' @inheritParams prspctv
#' @inheritParams labsimplex
#' @inheritParams generateVertex
#' @param experiments number of vertexes to evaluate
#' @import ggplot2
#' @importFrom utils capture.output
#' @export

exampleOptimization <- function (surface, simplex = NULL, centroid = c(7, 3.40),
                                 stepsize = c(0.6, 10), algor = 'fixed',
                                 experiments = 17, noise = 0) {
  ii1 <- FALSE
  n = 0
  if (missing(simplex)) {
    ii1 <- TRUE
    simplex <- labsimplex(N = 2, centroid = centroid, stepsize = stepsize)
    n = 2 + 1
  }
  invisible(capture.output(
    for (ii in 1:(experiments - n)){
      if (ii == 1 && ii1) {
        simplex <- generateVertex(simplex = simplex, algor = algor,
                                  qflv = surface(x1 = simplex$coords[, 2],
                                                 x2 = simplex$coords[, 1],
                                                 noise = noise))
      } else {
        simplex <- generateVertex(simplex = simplex, algor = algor,
          qflv = surface(x1 = simplex$coords[nrow(simplex$coords), 2],
                         x2 = simplex$coords[nrow(simplex$coords), 1],
                         noise = noise))
      }
    }
  ))
  return(simplex)
}

#' Adds simplex movements to a response surface contour
#'
#' @description The function complements the
#' @param p       asdsdf
#' @param simplex simplex
#' @seealso \code{\link{cntr}} \code{\link{exampleOptimization}}
#' @examples
#' simplex <- exampleOptimization(surface = exampleSurfaceR2,
#'                                centroid = c(7, 340),
#'                                stepsize = c(1.2, 15))
#' (p <- cntr(surface = exampleSurfaceR2))
#' p <- addSimplex2Surface(p = p, simplex = simplex)
#' print(p)
#' @importFrom  ggplot2 geom_segment aes
#' @export
addSimplex2Surface <- function (p, simplex) {
  # \code{\link[ggplot2]{ggplot} produced by \code{\link{cntr}} by
  # adding the movements made for a simplex object (generally produced by \code{\link{exampleOptimization}})
  # a \code{\link[ggplot2]{ggplot} object produced by \code{\link{cntr}}
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
