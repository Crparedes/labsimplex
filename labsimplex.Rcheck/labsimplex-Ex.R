pkgname <- "labsimplex"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "labsimplex-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('labsimplex')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("adjustVertex")
### * adjustVertex

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: adjustVertex
### Title: Modify given coordinates of given vertices of a simplex
### Aliases: adjustVertex

### ** Examples

simplex <- labsimplex(N = 3, start = c(7, 25, 0.15),
                      stepsize = c(0.2, 5, 0.02))
adjustVertex(simplex = simplex, newcoords = list(Vertex.1 = c(7, NA, NA),
                                                 Vertex.3 = c(7.2, NA, NA)),
             overwrite = TRUE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("adjustVertex", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("generateVertex")
### * generateVertex

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: generateVertex
### Title: Generates the new vertex of a simplex
### Aliases: generateVertex

### ** Examples

simplex3D <- labsimplex(N = 3)
simplex3D <- generateVertex(simplex = simplex3D, qflv = rnorm(4))
## Not run: 
##D ## Optional form:
##D ## After obtaining the response for the last vertex generated:
##D NV <- rnorm(1)
##D generateVertex(simplex = simplex3D, qflv = NV, overwrite = TRUE)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("generateVertex", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("labsimplex")
### * labsimplex

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: labsimplex
### Title: Generates simplex object with coordinates of starting vertices.
### Aliases: labsimplex

### ** Examples

  labsimplex(N = 3)
  labsimplex(N = 3, centroid = c(66, 1, 12), stepsize = c(10, 0.1, 2),
             var.name = c('potential', 'pH', 'T'))

  labsimplex(N = 3, usrdef = rbind(c(2, 0, 0), c(-0.5, 1, 0),
                                   c(-0.5, -0.7, 1), c(-0.5, -0.4, -0.6)))
## Not run: 
##D ## A user defined coordinates may define faces that rely on same hyperplane:
##D   labsimplex(N = 3, usrdef = rbind(c(2, 0, 0), c(-0.5, 0, 0), c(0, 0, 0),
##D                                    c(-0.5, -0.4, -0.6)))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("labsimplex", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.smplx")
### * plot.smplx

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.smplx
### Title: Makes a two dimentional plot of a simplex object.
### Aliases: plot.smplx

### ** Examples

  plot(x = labsimplex(N = 2))
  plot(x = labsimplex(N = 2), expand = FALSE)

  plot(x = labsimplex(N = 8))
  plot(x = labsimplex(N = 8), sel.dim = c(3, 4))

  ## Simulation of the real proccess where a simplex is made and evaluated
  set.seed(12)
  simplex2D <- labsimplex(N = 2)
  plot(x = simplex2D)
  generateVertex(simplex = simplex2D, qflv = rnorm(3), overwrite = TRUE)
  plot(x = simplex2D)
  generateVertex(simplex = simplex2D, qflv = rnorm(1), overwrite = TRUE)
  plot(x = simplex2D)
  generateVertex(simplex = simplex2D, qflv = rnorm(1), overwrite = TRUE)
  plot(x = simplex2D)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.smplx", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plotSimplex3D")
### * plotSimplex3D

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plotSimplex3D
### Title: Makes a three dimentional plot of a simplex object.
### Aliases: plotSimplex3D

### ** Examples

  plotSimplex3D(simplex = labsimplex(N = 3))

  plotSimplex3D(simplex = labsimplex(N = 8))
  plotSimplex3D(simplex = labsimplex(N = 8), sel.dim = c(4, 6, 8))

  ## Simulation of the real proccess where a simplex is made and measured,
  ## plotted nad the new vertex is measuresd after it is generated.
  set.seed(12)
  simplex3D <- labsimplex(N = 3)
  plotSimplex3D(simplex = simplex3D)
  generateVertex(simplex = simplex3D, qflv = rnorm(4), overwrite = TRUE)
  plotSimplex3D(simplex = simplex3D)
  generateVertex(simplex = simplex3D, qflv = rnorm(1), overwrite = TRUE)
  plotSimplex3D(simplex = simplex3D)
  generateVertex(simplex = simplex3D, qflv = rnorm(1), overwrite = TRUE)
  plotSimplex3D(simplex = simplex3D)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plotSimplex3D", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plotSimplexResponse")
### * plotSimplexResponse

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plotSimplexResponse
### Title: Plots the response versus the vertex number of a simplex
###   optimization.
### Aliases: plotSimplexResponse

### ** Examples

set.seed(1)
# Generate a simplex and add some responses to the vertices
simplex <- labsimplex(N = 3)
generateVertex(simplex, qflv = rnorm(4), algor = 'variable',
  overwrite = TRUE)
generateVertex(simplex, qflv = 2.75, algor = 'variable', overwrite = TRUE)
generateVertex(simplex, qflv = 0.8,  algor = 'variable', overwrite = TRUE)
generateVertex(simplex, qflv = 2.94, algor = 'variable', overwrite = TRUE)
generateVertex(simplex, qflv = 3.14, algor = 'variable', overwrite = TRUE)

# Plot the response versus the vertex function
plotSimplexResponse(simplex)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plotSimplexResponse", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("simplexExport")
### * simplexExport

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: simplexExport
### Title: Exports the information contained on a 'smplx' class object.
### Aliases: simplexExport

### ** Examples

## Not run: 
##D   simplex <- labsimplex(N = 5, qual.fun = rnorm(6, 2, 1))
##D   simplexExport(simplex = simplex)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("simplexExport", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("simplexImport")
### * simplexImport

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: simplexImport
### Title: Imports the information contained in a '.smplx' file.
### Aliases: simplexImport

### ** Examples

## Not run: 
##D   simplexExport(labsimplex(N = 4), name = "simplex4D")
##D   simplexImport("simplex4D.smplx")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("simplexImport", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
