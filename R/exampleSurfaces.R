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
#' exampleSurfaceR2(temp = 290, pH = 4)
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Jesús Ágreda, \email{jagreda@@unal.edu.co}
#' @export

exampleSurfaceR2 <- function (temp, pH, noise = 0) {
  if (length(temp) != length(pH)) stop('Vector parameters x and y must have same length')

  if (any(any(temp > 365), any(temp < 278), any(pH > 14), any(pH < 0))) return(-1)
  return(94 * (exp(-(0.035*(pH - 10)^2 + 0.002 * (temp - 300)^2))))
}

#' Evaluates two variables in a hypothetical response surface
#'
#' The response surface modelates the yield of a hypotetical chemical reaction
#' affected by the temperature and the pH of the media.
#'
#' @inheritParams exampleSurfaceR2
#'
#' @return The yield of the reaction
#' @examples
#' exampleSurfaceR2(temp = 290, pH = 4)
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Jesús Ágreda, \email{jagreda@@unal.edu.co}
#' @export

exampleSurfaceR2.2pks <- function (temp, pH, noise = 0) {
  if (length(temp) != length(pH)) stop('Vector parameters x and y must have same length')

  if (any(any(temp > 365), any(temp < 278), any(pH > 14), any(pH < 0))) return(-1)
  return(80 * exp(-(0.05*(pH - 4.5)^2 + 0.0025 * (temp - 340)^2)) +
         94 * (exp(-(0.035*(pH - 10)^2 + 0.002 * (temp - 300)^2))) +
         rnorm(length(pH), 0, noise))
}

#' Evaluates two variables in a hypothetical response surface
#'
#' The response surface modelates the yield of a hypotetical chemical reaction
#' affected by the temperature and the pH of the media.
#'
#' @param  pH    Numeric value (may be a vector) for pH.
#' @param  noise Noise used in the response surface as absolute percentaje.
#' @param  seed  Seed for the noise generated
#' @inheritParams exampleSurfaceR2
#'
#' @return The yield of the reaction
#' @examples
#' exampleSurfaceR2.2pks(temp = 290, pH = 4)
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Jesús Ágreda, \email{jagreda@@unal.edu.co}
#' @export

exampleSurfaceR3 <- function (temp, pH, Conc, noise = 0) {
  if (length(temp) != length(pH) || length(Conc) != length(pH)) {
    stop('Vector parameters temp, pH and Conc must have same length')
  }

  if (any(any(temp > 365), any(temp < 278), any(pH > 14), any(pH < 0),
          any(Conc < 0), any(Conc > 1))) return(-1)
  return(94 * (exp(-(0.035*(pH - 10)^2 + 0.002 * (temp - 300)^2 +
                         0.3*(Conc - 0.5)^2))))
}


#temp <- x <- seq(278, 365, length = 10)
#pH <- y <- seq(0, 14, length = 10)
#z <- outer(x, y, exampleSurfaceR2.2pks, noise = noise)
#colors  <- colorRampPalette(c("grey30", "white"))(100)
#z.facet.center <- (z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1] + z[-nrow(z), -ncol(z)])/4
#z.facet.range  <- cut(z.facet.center, 200)
#persp(x, y, z, theta = 22, phi = 15, ticktype = "detailed", col = colors[z.facet.range], lwd = 0.3,
#      ltheta = -120, shade = 0.2, expand = 0.6, xlab = 'Temperature (K)', ylab = 'pH', zlab = 'Yield (%)')

#library(ggplot2)
#x <- seq(278, 365, length = 45)
#y <- seq(0, 14, length = 45)
#gg <- expand.grid(x = x, y = y)
#gg$z <- with(gg, exampleSurfaceR2.2pks(x, y, noise = noise))

#brks <- cut(gg$z, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90))
#brks <- gsub(",", " - ", brks, fixed = TRUE)
#gg$brks <- gsub("\\(|\\]", "", brks)  # reformat guide labels
#ggplot(gg, aes(x, y)) + theme_bw() +
#  geom_tile(aes(fill = brks)) + scale_fill_manual("Z", values = colorRampPalette(c("grey20", "white"))(11)) +
#  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
#  theme(legend.position = 'none', axis.text = element_text(size = 12, color = 'black'),
#        axis.title = element_text(size = 12, color = 'black'), plot.margin = unit(c(0.5, 0.5, 0.1, 0.1), "cm"),
#        panel.border = element_blank(), panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
#  labs(x = 'Temperature (K)', y = 'pH')
