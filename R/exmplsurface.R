#' Evaluates two variables in a hypothetical response surface
#'
#' The response surface modelates the yield of a hypotetical chemical reaction
#' affected by the temperature and the pH of the media.
#'
#' @param  temp  Numeric value (may be a vector) for temperature.
#' @param  pH    Numeric value (may be a vector) for pH.
#' @param  noise Noise used in the response surface as absolute percentaje.
#' @param  seed  Seed for the noise generated
#'
#' @return The yield of the reaction
#' @examples
#' exmplsurface(temp = 290, pH = 4)
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Jesús Ágreda, \email{jagreda@@unal.edu.co}
#' @export

exmplsurface <- function (temp, pH, noise = 0, seed = NULL) {
  if (length(temp) != length(pH)) stop('Vector parameters x and y must have same length')
  # good range for Matlab's peaks function is -2, 2 for both x and y. We want 10, 90 for x and 1, 13 for y.
  x <- temp * 0.05 - 16
  y <- pH / 3 - 2.33
  if (!missing(seed)) set.seed(seed)
  return (3.8 * (8 + 10 * (1 - x)^2 * exp((- x^2 - (y + 1)^2)) -
                   10 * (x/5 - x^3 - y^5) * exp(- x^2 - y^2) -
                   1/3 * exp(- (x + 1)^2 - y^2)) + rnorm(n = length(x), mean = 0, sd = noise))
}

if (F) {



  ## Finding the *nearest* local optima
  ```{r LocalOptima, echo = FALSE, results = 'hide', message = FALSE}
  simplex1 <- labsimplex(N = 2, centroid = c(7.5, 290), stepsize = c(0.6, 10), var.name = c('pH', 'Temperature'))
  simplex2 <- labsimplex(N = 2, centroid = c(10.5, 290), stepsize = c(0.6, 10), var.name = c('pH', 'Temperature'))
  for (ii in 1:20){

    if (ii == 1) {
      simplex1 <- generateVertex(simplex = simplex1, algor = 'fixed',
                                 qflv = exmplsurface(temp = simplex1$coords[, 2], pH = simplex1$coords[, 1]))
      simplex2 <- generateVertex(simplex = simplex2, algor = 'fixed',
                                 qflv = exmplsurface(temp = simplex2$coords[, 2], pH = simplex2$coords[, 1]))
    } else {
      simplex1 <- generateVertex(simplex = simplex1, algor = 'fixed',
                                 qflv = exmplsurface(temp = simplex1$coords[nrow(simplex1$coords), 2],
                                                     pH = simplex1$coords[nrow(simplex1$coords), 1]))
      simplex2 <- generateVertex(simplex = simplex2, algor = 'fixed',
                                 qflv = exmplsurface(temp = simplex2$coords[nrow(simplex2$coords), 2],
                                                     pH = simplex2$coords[nrow(simplex2$coords), 1]))
    }
  }
  #plot(0)
  par(new = FALSE, mar = c(3.52, 3.4, 2.4, 4.85))
  cntr(length = 300, noise = 0)
  par(new = TRUE, mar = c(3.52, 3.4, 2.4, 4.85))
  plot(x = simplex1$coords[, 2], y = simplex1$coords[, 1], axes = FALSE, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n',
       ann = FALSE, bty = "n", yaxs = "i", xaxs = "i", xlim = c(280, 360), ylim = c(1, 13), pch = 16)

  cntr(length = 300, noise = 0)
  par(new = TRUE, mar = c(3.52, 3.4, 2.4, 4.85))
  plot(x = simplex2$coords[, 2], y = simplex2$coords[, 1], axes = FALSE, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n',
       ann = FALSE, bty = "n", yaxs = "i", xaxs = "i", xlim = c(280, 360), ylim = c(1, 13), pch = 16)
  ```

  ```{r plotsW2noise, echo = FALSE}
  par(mar = c(1, 1, 0.1, 0.1))
  plot1 <- prspctv(length = 45, noise = 2)
  plot2 <- cntr(length = 45, noise = 2)
  print(plot2)
  ```
  ```{r plotsW4noise, echo = FALSE}
  par(mar = c(1, 1, 0.1, 0.1))
  plot1 <- prspctv(length = 45, noise = 4)
  plot2 <- cntr(length = 45, noise = 4)
  print(plot2)
  ```
  ```{r plotsW7noise, echo = FALSE}
  par(mar = c(1, 1, 0.1, 0.1))
  plot1 <- prspctv(length = 45, noise = 7)
  plot2 <- cntr(length = 45, noise = 7)
  print(plot2)
  ```

  ```{r plotsWOnoiseprint}
  2
  ```


library(labsimplex)
library(lattice)

cntr <- function (length, noise) {
  x <- seq(280, 360, length = length)
  y <- seq(1, 13, length = length)
  simplex$coordsurfc(x = expand.grid(x, y)[, 1], y = expand.grid(x, y)[, 2], noise = noise)
  colnames(df) <- c('x', 'y', 'z')
  plot2 <- contourplot(z ~ x * y, df, lwd = 0.7, cuts = 10, labels = FALSE, region = TRUE,
                       col.regions = colorRampPalette(c("grey10", "white")),
                       xlab = 'Temperature (K)', ylab = 'pH', zlab = 'Yield (%)')
  return(plot2)
}


simplex <- labsimplex(N = 2, centroid = c(8, 290), stepsize = c(0.6, 10), var.name = c('pH', 'Temperature'))
cntr(length = 300, noise = 0)
for (ii in 1:20){
  par(new = TRUE, mar = c(3.52, 3.4, 2.4, 4.85))
  plot(x = simplex$coords[, 2], y = simplex$coords[, 1], axes = FALSE, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n',
       ann = FALSE, bty = "n", yaxs = "i", xaxs = "i", xlim = c(280, 360), ylim = c(1, 13), pch = 16)

#  for (i in 1:(nrow(simplex$coords) - 1)) {
#    for (j in (i + 1):nrow(simplex$coords)) {
#      segments(x0 = simplex$coords[i, 2], y0 = simplex$coords[i, 1], x1 = simplex$coords[j, 2], y1 = simplex$coords[j, 1])
#    }
#  }

  if (ii == 1) {
    generateVertex(simplex = simplex, qflv = exmplsurfaceresult(temp = simplex$coords[, 2], pH = simplex$coords[, 1]),
                   algor = 'fixed', overwrite = TRUE)
  } else {
    generateVertex(simplex = simplex, qflv = exmplsurfaceresult(temp = simplex$coords[nrow(simplex$coords), 2],
                                                                pH = simplex$coords[nrow(simplex$coords), 1]),
                   algor = 'fixed', overwrite = TRUE)
  }
  invisible(readline(prompt="Press [enter] to continue"))
}

simplex <- labsimplex(N = 2, centroid = c(10, 290), stepsize = c(0.6, 10), var.name = c('pH', 'Temperature'))
cntr(length = 300, noise = 0)
for (ii in 1:20){
  par(new = TRUE, mar = c(3.52, 3.4, 2.4, 4.85))
  plot(x = simplex$coords[, 2], y = simplex$coords[, 1], axes = FALSE, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n',
       ann = FALSE, bty = "n", yaxs = "i", xaxs = "i", xlim = c(280, 360), ylim = c(1, 13), pch = 16)

  #  for (i in 1:(nrow(simplex$coords) - 1)) {
  #    for (j in (i + 1):nrow(simplex$coords)) {
  #      segments(x0 = simplex$coords[i, 2], y0 = simplex$coords[i, 1], x1 = simplex$coords[j, 2], y1 = simplex$coords[j, 1])
  #    }
  #  }

  if (ii == 1) {
    generateVertex(simplex = simplex, qflv = exmplsurfaceresult(temp = simplex$coords[, 2], pH = simplex$coords[, 1]),
                   algor = 'fixed', overwrite = TRUE)
  } else {
    generateVertex(simplex = simplex, qflv = exmplsurfaceresult(temp = simplex$coords[nrow(simplex$coords), 2],
                                                                pH = simplex$coords[nrow(simplex$coords), 1]),
                   algor = 'fixed', overwrite = TRUE)
  }
  invisible(readline(prompt="Press [enter] to continue"))
}


plot2; par(new = TRUE, mar = c(3.52, 3.4, 2.4, 4.85))
plot(x = simplex$coords[, 2], y = simplex$coords[, 1], axes = FALSE, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n',
     ann = FALSE, bty = "n", yaxs = "i", xaxs = "i", xlim = c(280, 360), ylim = c(1, 13), pch = 16)
for (i in 1:(nrow(simplex$coords) - 1)) {
  for (j in (i + 1):nrow(simplex$coords)) {
    segments(x0 = simplex$coords[i, 2], y0 = simplex$coords[i, 1], x1 = simplex$coords[j, 2], y1 = simplex$coords[j, 1])
  }
}
}
