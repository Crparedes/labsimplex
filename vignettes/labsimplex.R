## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "75%"
)
# Render html vignetes by using devtools::document(roclets = "vignette")
require(labsimplex)

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
adjustVertex(simplex = ManSet, newcoords = list(Vertex.1 = c(7.15, NA, NA), 
                                                Vertex.2 = c(NA, 29, NA)),
             overwrite = TRUE)
print(ManSet)

## ----plot3D, dpi = 300, fig.width = 7, fig.height = 7, fig.align = 'center', fig.cap = '**Figure 1.** Initial simplex representation in a 3D space'----
plotSimplex3D(ExpSet)

## ----plot2D, dpi = 300, fig.width = 12, fig.height = 5, out.width = '110%', fig.align = 'center', fig.cap = '**Figure 2.** Two-dimensional proyections of simplex shown in Figure 1'----
par(mfrow = c(1, 3))
plot(ExpSet, sel.dim = c('pH', 'Temp'))
plot(ExpSet, sel.dim = c('pH', 'Conc'))
plot(ExpSet, sel.dim = c('Temp', 'Conc'))

## ----genV1, dpi = 300, fig.width = 7, fig.height = 7, fig.align = 'center', fig.cap = '**Figure 3.** Movement of the simplex after first reflection'----
generateVertex(simplex = ExpSet, qflv = c(65, 72, 54, 78), overwrite = TRUE)
plotSimplex3D(ExpSet)

## ----export--------------------------------------------------------------
simplexExport(ExpSet)

## ----import--------------------------------------------------------------
simplexImport('ExpSet.smplx', name = 'importedSimplex')
print(importedSimplex)

