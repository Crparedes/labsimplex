## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "48%"
)
# Render html vignetes by using devtools::document(roclets = "vignette")
# Render also pdf vignetes by using rmarkdown::render("vignettes/labsimplex.Rmd", "all")
library(labsimplex)
library(ggplot2)

## ----  eval = FALSE------------------------------------------------------
#  install.packages("labsimplex")

## ----  eval = FALSE------------------------------------------------------
#  devtools::install_github("Crparedes/labsimplex", build_vignettes = TRUE)

## ----surfaces1, echo = TRUE, fig.cap = 'Response surface `exampleSurfaceR2()` in 3D perspective (left) and contour plot (right).', fig.show = "hold"----
prspctv(surface = exampleSurfaceR2, par = list(mar = c(0.5, 0.6, 0, 0)), phi = 30, theta = 30,
        ltheta = -120, expand = 0.6, xlab = 'Temperature (K)', ylab = 'pH', zlab = 'Yield (%)')
(cont.surf <- cntr(surface = exampleSurfaceR2, length = 200))

## ----Nprov---------------------------------------------------------------
simplexR2 <- labsimplex(n = 2, centroid = c(7, 340), stepsize = c(1.2, 10),
                        var.name = c('pH', 'Temperature'))
print(simplexR2)

## ----usrdef--------------------------------------------------------------
coords <- rbind(c(7.1, 325), c(6.5, 350), c(6.5, 300))
simplexR2Manual <- labsimplex(n = 2, usrdef = coords, var.name = c('pH', 'Temperature'))
print(simplexR2Manual, conventions = FALSE)

## ----frf2, message = FALSE-----------------------------------------------
if (!require(FrF2)) message('Please install FrF2 package')
set.seed(1)
(screening <- FrF2(resolution = 3, 
                   factor.names = list(pH = c(6.8, 7.2), Temp = c(330, 350), Conc = c(0.4, 0.6))))


## ----frf2-2--------------------------------------------------------------
simplexR3 <- labsimplex(n = 3, usrdef = matrix(as.numeric(as.matrix(screening)), ncol = 3),
                        var.name = dimnames(screening)[[2]])
print(simplexR3, conventions = FALSE)

## ----changing------------------------------------------------------------
adjustVertex(simplex = simplexR2, newcoords = list(Vertex.1 = c(7.95, NA), Vertex.2 = c(NA, 342)),
             overwrite = TRUE)
print(simplexR2, conventions = FALSE)

## ----plot1SimplexR2, fig.cap = 'Initial two-variables simplex isolated in the space (left) and over the response surface that describes the system (right).', fig.show = "hold"----
plot(simplexR2)
(addSimplex2Surface(p = cont.surf, simplex = simplexR2))

## ----responses, message = FALSE------------------------------------------
(responses <- exampleSurfaceR2(x1 = simplexR2$coords[, 2], x2 = simplexR2$coords[, 1]))
generateVertex(simplex = simplexR2, qflv = responses, crit = 'max', 
               algor = 'fixed', overwrite = TRUE)
print(simplexR2, conventions = FALSE)

## ----plot2SimplexR2, message = FALSE, fig.cap = 'First movement (left) and complete path (right) of a fixed step-size simplex optimazation over the response surface `exampleSurfaceR2()`.', fig.show = "hold"----
(addSimplex2Surface(p = cont.surf, simplex = simplexR2))
simplexR2 <- exampleOptimization(surface = exampleSurfaceR2, simplex = simplexR2)
(addSimplex2Surface(p = cont.surf, simplex = simplexR2))

## ----plot3SimplexR2, message = FALSE, fig.cap = 'Complete path of a variable step-size simplex optimazation over the response surface `exampleSurfaceR2()`.', fig.show = "hold"----
simplexR2Var <- exampleOptimization(surface = exampleSurfaceR2, algor = 'variable', 
                                    centroid = c(7, 340), stepsize = c(1.2, 10))
(addSimplex2Surface(p = cont.surf, simplex = simplexR2Var))

## ----plot4SimplexR2, message = FALSE, fig.cap = 'Responses vs. vertex number for fixed (right) and a variable (left) step-size simplex optimizationover the response surface `exampleSurfaceR2()`', fig.show = "hold"----
plotSimplexResponse(simplexR2)
plotSimplexResponse(simplexR2Var)

## ----export--------------------------------------------------------------
simplexExport(simplex = simplexR3)

## ----import--------------------------------------------------------------
print(simplexR3, conventions = FALSE)
rm(simplexR3)
exists('simplexR3')
# We have exported and removed the 'simplexR3' object. Now it will be imported
simplexImport(filename = 'simplexR3')
print(simplexR3, conventions = FALSE)

## ----plotsW2noise, echo = TRUE, results = 'hide', message = FALSE, warning = FALSE, fig.cap = '3D perspective (left), simplex path over the contour plot (center) and response against vertex number (right) for fixed step-size simplex optimization over the response surface `exampleSurfaceR2()` at low noise (top), medium noise (middle) and high noise (bottom).', fig.show = "hold", out.width = "31%"----
noises <- c(3, 8, 18)
seeds <- c(0, 13, 13)
for (ii in 1:3) {
  prspctv(length = 45, noise = noises[ii], surface = exampleSurfaceR2, 
          par = list(mar = c(1.2, 1, 0, 0)), ltheta = -120, shade = 0.2, expand = 0.6, 
          xlab = 'Temperature (K)', ylab = 'pH', zlab = 'Yield (%)', ticktype = "detailed")
  set.seed(seeds[ii])
  simplexNoisy <- exampleOptimization(surface = exampleSurfaceR2, noise = noises[ii],
                                      centroid = c(7, 340), stepsize = c(1.2, 10))
  cntr.ns <- cntr(surface = exampleSurfaceR2, length = 200, noise = noises[ii])
  print(addSimplex2Surface(p = cntr(surface = exampleSurfaceR2, length = 200, noise = noises[ii]), 
                           simplex = simplexNoisy))
  plotSimplexResponse(simplexNoisy)
}

## ----plotsW3noise, echo = TRUE, results = 'hide', message = FALSE, warning = FALSE, fig.cap = '3D perspective (left), simplex path over the contour plot (center) and response against vertex number (right) for variable step-size simplex optimization over the response surface `exampleSurfaceR2()` at low noise (top), medium noise (middle) and high noise (bottom).', fig.show = "hold", out.width = "31%"----
noises <- c(3, 8, 18)
seeds <- c(0, 65, 13)
for (ii in 1:3) {
  prspctv(length = 45, noise = noises[ii], surface = exampleSurfaceR2, 
          par = list(mar = c(1.2, 1, 0, 0)), ltheta = -120, shade = 0.2, expand = 0.6, 
          xlab = 'Temperature (K)', ylab = 'pH', zlab = 'Yield (%)', ticktype = "detailed")
  set.seed(seeds[ii])
  simplexNoisy <- exampleOptimization(surface = exampleSurfaceR2, noise = noises[ii],
                                      centroid = c(7, 340), stepsize = c(1.2, 10), 
                                      algor = 'variable')
  cntr.ns <- cntr(surface = exampleSurfaceR2, length = 200, noise = noises[ii])
  print(addSimplex2Surface(p = cntr(surface = exampleSurfaceR2, length = 200, noise = noises[ii]), 
                           simplex = simplexNoisy))
  plotSimplexResponse(simplexNoisy)
}

## ----surfaces2, echo = TRUE, fig.cap = 'Response surface `exampleSurfaceR2.2pks()` in 3D perspective (left) and contour plot (right).', fig.show = "hold"----
prspctv(surface = exampleSurfaceR2.2pks, par = list(mar = c(0.5, 0.6, 0, 0)), phi = 30, theta = 30,
        ltheta = -120, expand = 0.6, xlab = 'Temperature (K)', ylab = 'pH', zlab = 'Yield (%)')
(cont.surf2 <- cntr(surface = exampleSurfaceR2.2pks, length = 200))

## ----LocalOptima, echo = TRUE, results = 'hide', message = FALSE, warning = FALSE, fig.cap = 'Complete paths of fixed (up) and variable (down) step-size simplex optimization using centroids that make the simplex to move towards the global maximun (left) and towarsd a local maximum (rigth).', fig.show = "hold"----
addSimplex2Surface(p = cont.surf2, 
                   simplex = exampleOptimization(surface = exampleSurfaceR2.2pks, 
                                                 centroid = c(5.5, 315), 
                                                 stepsize = c(-1.5, 15), 
                                                 experiments = 13))
addSimplex2Surface(p = cont.surf2, 
                   simplex = exampleOptimization(surface = exampleSurfaceR2.2pks, 
                                                 centroid = c(1.5, 310), 
                                                 stepsize = c(-1.5, 15), 
                                                 experiments = 13))
addSimplex2Surface(p = cont.surf2, 
                   simplex = exampleOptimization(surface = exampleSurfaceR2.2pks, 
                                                 centroid = c(5.5, 315), 
                                                 stepsize = c(-1.5, 15), 
                                                 experiments = 17, algor = 'variable'))
addSimplex2Surface(p = cont.surf2, 
                   simplex = exampleOptimization(surface = exampleSurfaceR2.2pks, 
                                                 centroid = c(1.5, 310), 
                                                 stepsize = c(-1.5, 15), 
                                                 experiments = 17, algor = 'variable'))

