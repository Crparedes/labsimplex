#' \code{labsimplex}: Simplex Optimization Algorithms for Laboratory and
#' Manufacturing Processes
#'
#' The \code{labsimplex} package implements the simplex
#' optimization algorithms firstly proposed by Spendley et al. (1962)
#' <doi:10.1080/00401706.1962.10490033> and later modified by Nelder
#' and Mead (1965) <doi:10.1093/comjnl/7.4.308> for laboratory and
#' manufacturing processes. The package also provides tools for
#' graphical representation of the simplexes and some example response
#' surfaces that are useful for illustrating the optimization process.
#'
#' A simplex is a geometric element defined as the simpler polytope possible
#' in an \emph{n}-dimensional space. If the space has \emph{n} dimensions,
#' the simplexes there will have \emph{n+1} corners called vertexes.
#' The simplexes in two and three-dimensional spaces are the well-known
#' triangle and tetrahedron, respectively.\cr
#' In the simplex optimization algorithm, the experimental variables are
#' represented by the dimensions in the abstract space. Each vertex in the
#' simplex represents an experiment, then the coordinates of the vertex
#' represent the values for the variables in that experimental setting. The
#' experiments must be performed and a response must be assigned to each
#' vertex. In the optimization process, one of the vertexes is discarded in
#' favor of a new one that must be evaluated. In the first simplex, the vertex
#' with the worst response is discarded. The second worst vertex in this
#' simplex is discarded in the following simplex and the procedure is repeated
#' until the optimum is reached or a response good enough is obtained. The
#' process of discarding a vertex and generating a new one is known as a
#' movement of the simplex.\cr
#' In this document, the words vertex and experiment are used
#' interchangeably. The same applies to dimensions and experimental
#' variables. \cr
#'
#' @section \code{labsimplex} functions:
#' This package uses list objects of class \code{'smplx'} to store the
#' simplex information, including all the coordinates of the
#' vertexes and their responses.\cr
#' The \code{labsimplex} functions can generate a new \code{'smplx'} class
#' object, assing responses to the vertices to generate the next one and to
#' visualize different spatial representations of the \emph{n}-dimensional
#' simplex in 2D or 3D projections. Detailed information can be found by
#' typing \code{vignette('labsimplex')}.
#'
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Jesús Ágreda, \email{jagreda@@unal.edu.co}
#' @references
#' Nelder, J. A., and R. Mead. 1965. “A Simplex Method for Function
#' Minimization.” The Computer Journal 7 (4): 308–13.
#'
#' Spendley, W., G. R. Hext, and F. R0. Himsworth. 1962. “Sequential
#' Application of Simplex Designs in Optimization and Evolutionary Operation.”
#' Technometrics 4 (4): 441–61.
#' @docType package
#' @name labsimplex-package
NULL
