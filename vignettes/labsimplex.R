## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "48%"
)
# Render html vignetes by using devtools::document(roclets = "vignette")
# Render also pdf vignetes by using rmarkdown::render("vignettes/labsimplex.Rmd", "all")
require(labsimplex)
require(ggplot2)

## ----functions, echo = FALSE---------------------------------------------
prspctv <- function (length, noise) {
  temp <- x <- seq(278, 365, length = length)
  pH <- y <- seq(0, 14, length = length)
  z <- outer(x, y, exampleSurfaceR2.2pks, noise = noise)
  
  colors  <- colorRampPalette(c("grey30", "white"))(100)
  z.facet.center <- (z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1] + z[-nrow(z), -ncol(z)])/4
  z.facet.range  <- cut(z.facet.center, 200)
  par(mar = c(1.2, 1, 0, 0))
  persp(x, y, z, theta = 22, phi = 15, ticktype = "detailed", col = colors[z.facet.range], lwd = 0.3,
        ltheta = -120, shade = 0.2, expand = 0.6, xlab = 'Temperature (K)', ylab = 'pH', zlab = 'Yield (%)')
}
cntr <- function (length, noise) {
  x <- seq(278, 365, length = length)
  y <- seq(0, 14, length = length)
  gg <- expand.grid(x = x, y = y)
  gg$z <- with(gg, exampleSurfaceR2.2pks(x, y, noise = noise))
    
  #brks <- cut(gg$z, breaks = seq(0, 100, len = 10))
  brks <- cut(gg$z, breaks = c(-20, seq(10, 90, 10)))
  brks <- gsub(",", " - ", brks, fixed = TRUE)
  gg$brks <- gsub("\\(|\\]", "", brks)  # reformat guide labels
  p <- ggplot(gg, aes(x, y)) + theme_bw() +
         geom_tile(aes(fill = brks)) + scale_fill_manual("Z", values = colorRampPalette(c("grey30", "white"))(11)) +
         scale_x_continuous(expand = c(0, 0), limits = c(278, 365)) + scale_y_continuous(expand = c(0, 0), limits = c(0, 14)) +
         theme(legend.position = 'none', axis.text = element_text(size = 12, color = 'black'),
               axis.title = element_text(size = 12, color = 'black'), plot.margin = unit(c(0.5, 0.5, 0.1, 0.1), "cm"),
               panel.border = element_blank(), panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
         labs(x = 'Temperature (K)', y = 'pH')
  return(p)
}
completeOptimization <- function (centroid, stepsize = c(0.6, 10), algor = 'fixed', experiments = 17,
                                  length = 200, noise = 0) {
  simplex <- labsimplex(N = 2, centroid = centroid, stepsize = stepsize, var.name = c('pH', 'Temperature'))
  for (ii in 1:experiments){
    if (ii == 1) {
      simplex <- generateVertex(simplex = simplex, algor = algor,
                                qflv = exampleSurfaceR2.2pks(temp = simplex$coords[, 2],
                                                             pH = simplex$coords[, 1], noise = noise))
    } else {
      simplex <- generateVertex(simplex = simplex, algor = algor,
                                qflv = exampleSurfaceR2.2pks(temp = simplex$coords[nrow(simplex$coords), 2], 
                                                     pH = simplex$coords[nrow(simplex$coords), 1], noise = noise))
    }
  }
  p <- cntr(length = length, noise = noise) + geom_point(data = data.frame(x = simplex$coords[, 2], y = simplex$coords[, 1]))
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
                        aes(x = x, xend = xend, y = y, yend = yend))
  print(p)
  return(simplex)
}

## ----Nprov---------------------------------------------------------------
ExpSet <- labsimplex(N = 3)
print(ExpSet)

## ----Startprov-----------------------------------------------------------
ExpSet <- labsimplex(N = 3, start = c(7, 25, 0.15), stepsize = c(0.2, 5, 0.02), 
                     var.name = c('pH', 'Temp', 'Conc'))
ExpSet <- labsimplex(N = 3, centroid = c(6.85, 25, 0.15), stepsize = c(0.2, 5, 0.02), 
                     var.name = c('pH', 'Temp', 'Conc'))
print(ExpSet)

## ----usrdef--------------------------------------------------------------
ExpMtrx <- rbind(c(7.1, 25, 0.15), c(6.9, 28, 0.15), c(6.9, 23, 0.16), c(6.9, 23, 0.14))
ManSet <- labsimplex(N = 3, usrdef = ExpMtrx, var.name = c('pH', 'Temp', 'Conc'))
print(ManSet)

## ----frf2----------------------------------------------------------------
suppressMessages(library(FrF2))
set.seed(1)
screening <- FrF2(resolution = 3, nfactors = 3, 
                  factor.names = list(pH = c(6.9, 7.1), 
                                      Temp = c(23, 28), 
                                      Conc = c(0.14, 0.16)))
print(screening)

## ----frf2-2--------------------------------------------------------------
FrF2Set <- labsimplex(N = 3, usrdef = matrix(as.numeric(as.matrix(screening)), ncol = 3),
                      var.name = dimnames(screening)[[2]])
print(FrF2Set)

## ----changing------------------------------------------------------------
#adjustVertex(simplex = ManSet, newcoords = list(Vertex.1 = c(7.15, NA, NA), 
#                                                Vertex.2 = c(NA, 29, NA)),
#             overwrite = TRUE)
#print(ManSet)

## ----plot3D, dpi = 300, fig.width = 7, fig.height = 7, fig.align = 'center', fig.cap = 'Initial simplex representation in a 3D space'----
#plotSimplex3D(ExpSet)

## ----plot2D, dpi = 300, fig.width = 12, fig.height = 5, out.width = '110%', fig.align = 'center', fig.cap = ' Two-dimensional proyections of simplex shown in Figure 1'----
#par(mfrow = c(1, 3))
#plot(ExpSet, sel.dim = c('pH', 'Temp'))
#plot(ExpSet, sel.dim = c('pH', 'Conc'))
#plot(ExpSet, sel.dim = c('Temp', 'Conc'))

## ----genV1, dpi = 300, fig.width = 7, fig.height = 7, fig.align = 'center', fig.cap = 'Movement of the simplex after first reflection'----
#generateVertex(simplex = ExpSet, qflv = c(65, 72, 54, 78), overwrite = TRUE)
#plotSimplex3D(ExpSet)

## ----plotsWOnoise, echo = FALSE------------------------------------------
#  prspctv(length = 45, noise = 0)
#  print(cntr(length = 350, noise = 0))

## ----LocalOptima, echo = FALSE, results = 'hide', message = FALSE, warning = FALSE----
#completeOptimization(centroid = c(5.5, 315), stepsize = c(-1.5, 15), experiments = 12)
#completeOptimization(centroid = c(1.5, 310), stepsize = c(1.5, 15), experiments = 12)
#completeOptimization(centroid = c(3, 290), stepsize = c(1.5, 15), experiments = 15)
#completeOptimization(centroid = c(12, 335), stepsize = c(1.5, 15), experiments = 12)

#completeOptimization(centroid = c(5.5, 315), stepsize = c(-1.5, 15), experiments = 15, algor = 'variable')
#completeOptimization(centroid = c(1.5, 310), stepsize = c(1.5, 15), experiments = 15, algor = 'variable')
#completeOptimization(centroid = c(3, 290), stepsize = c(1.5, 15), experiments = 18, algor = 'variable')
#completeOptimization(centroid = c(12, 335), stepsize = c(1.5, 15), experiments = 18, algor = 'variable')

## ----plotsW2noise, echo = FALSE, results = 'hide', message = FALSE, warning = FALSE----
#set.seed(10)
#  prspctv(length = 45, noise = 2)
#  completeOptimization(centroid = c(5.5, 315), stepsize = c(-1.5, 15), experiments = 12,
#                       length = 100, noise = 2)
#  prspctv(length = 45, noise = 8)
#  completeOptimization(centroid = c(5.5, 315), stepsize = c(-1.5, 15), experiments = 12,
#                       length = 100, noise = 8)
#  prspctv(length = 45, noise = 14)
#  completeOptimization(centroid = c(5.5, 315), stepsize = c(-1.5, 15), experiments = 12,
#                       length = 100, noise = 14)

## ----export--------------------------------------------------------------
simplexExport(ExpSet)

## ----import--------------------------------------------------------------
simplexImport('ExpSet.smplx', name = 'importedSimplex')
print(importedSimplex)

