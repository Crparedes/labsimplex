
<!-- README.md is generated from README.Rmd. Please edit that file -->

# labsimplex

The goal of labsimplex is to implement in R the simplex algorithms
(fixed size and variable size) for laboratory and proccess
optimizations.

## Installation

You can install the last version of labsimplex by downloading the
[source
package](https://github.com/Crparedes/labsimplex/blob/master/labsimplex.tar.gz)
and installing it with:

``` r
install.packages(path_to_file, repos = NULL, type="source")
```

Where `path_to_file` would represent the full path and file name:

\-On Windows it will look something like this:
`"C:\Users\user\Downloads\labsimplex.tar.gz"`.

\-On UNIX it will look like this: `"/home/Downloads/labsimplex.tar.gz`"

A better alternative is use `install_github()` function from `devtools`
package:

``` r
devtools:install_github(url = "git://github.com/Crparedes/labsimplex")
```

Coming soon the released version of labsimplex will be avilable from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("labsimplex")
```

## Example

This is a basic example which shows you how to start an optimization
procces:

Suppose there is a reaction taking place in water at determinate pH and
temperature. Both variables have shown to have effect on the yiel of the
reaction and the purposse is to maximize it. Ussually the reaction is
made at 350 °C and pH 5.5. This will be the starting point. Rational
changes can be made in steps of 10°C and 0.5 units of pH for each
variable.

``` r
library(labsimplex)
yield <- labsimplex(N = 2, start = c(350, 5.5), var.name = c('Temp', 'pH'),
                    stepsize = c(10, 0.5))
print(yield)
#> 
#> Current simplex:
#>           Temp   pH . Response Label Nature
#> Vertex.3:  340 5.25 |       NA    NA      S
#> Vertex.2:  340 5.75 |       NA    NA      S
#> Vertex.1:  350 5.50 |       NA    NA      S
#> 
#> Conventions:    Labels:                    Nature:
#>       W: Worst or Wastebasket    S:  Starting
#>       N: Next to the worst       R:  Reflected
#>       B: Best                    E:  Expanded
#>                                  Cr: Contraction on the reflection side
#>       D: Disregarded             Cw: Contraction on the worst side
#> 
#>   Use print(..., conventions = FALSE) to disable conventions printing.
```

Is possible to plot the simplex object to visualize the coordinates of
the vertexes:

``` r
plot(yield)
```

<img src="man/figures/README-example2-1.png" width="100%" />

The experiments corresponding to the Temp. and pH values indicated for
each vertex must be performed and the response must be recorded. Suppose
the responses for vertexes 1 to 3 were 57%, 65% and 54% respectively.
The new vertex can be generated and the simplex movement can be
visualized again ploting the simplex.

``` r
generateVertex(simplex = yield, qflv = c(57, 65, 54), overwrite = TRUE)
#> New vertex to be evaluated:
#> Temp   pH 
#>  350    6
plot(yield)
```

<img src="man/figures/README-example3-1.png" width="100%" />

For more information read the package manuals and vignette.
