#' labsimplex: Implementation of simplex optimization algorithms for
#' laboratory applications.
#'
#' The labsimplex package provides tools to optimize a process by using the
#' simplex algorithms fixed-size reported by Spendley et al. (1962)
#' and variable-size reported by Nelder and Mead (1965).
#'
#' The package uses a list-like \code{'smplx'} class object to store the
#' simplex information including all the vertices coordinates and experimental
#' responses.
#' @section labsimplex functions:
#' The labsimplex functions allow generate a new \code{'smplx'} class object,
#' assing responses to the vertices to generate the next one and to visualize
#' different spatial representation of the \emph{n}-dimensional simplex in 2D
#' or 3D proyections. Detailed information can be found by typing
#' \code{vignette('labsimplex')}.
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
#'
#' @docType package
#' @name labsimplex-package
NULL
