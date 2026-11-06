#' Comparer plusieurs solutions
#'
#' Résout la même équation pour plusieurs conditions initiales et
#' superpose les courbes.
#'
#' @param equation Objet créé par [riccati()].
#' @param y0 Vecteur d'au moins deux valeurs initiales.
#' @param interval,n,ymax Voir [solve_riccati()].
#' @param ylim Limites verticales du graphique (facultatif, utile en cas d'explosion).
#' @param plot Si `FALSE`, renvoie le tableau de données au lieu du graphique.
#' @return Un objet `ggplot` (ou un `data.frame` si `plot = FALSE`).
#' @examples
#' eq <- riccati(a = 1, b = -2, c = 1)
#' compare_riccati(eq, y0 = c(-1, 0, 0.5, 2), interval = c(0, 3), ylim = c(-2, 3))
#' @importFrom ggplot2 .data
#' @export
compare_riccati <- function(equation, y0, interval = c(0, 5), n = 500,
                            ymax = 1e3, ylim = NULL, plot = TRUE) {
  if (length(y0) < 2) {
    stop("Fournir au moins deux conditions initiales.", call. = FALSE)
  }
  sols <- lapply(y0, function(v) solve_riccati(equation, v, interval, n = n, ymax = ymax))
  d <- do.call(rbind, lapply(seq_along(sols), function(i) {
    di <- sols[[i]]$data
    di$condition <- sprintf("y0 = %g", y0[i])
    di
  }))
  d$condition <- factor(d$condition, levels = sprintf("y0 = %g", y0))
  if (!plot) return(d)

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$y, colour = .data$condition)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(title = "Comparaison de solutions", x = "x", y = "y(x)",
                  colour = "Condition initiale") +
    ggplot2::theme_minimal()
  if (!is.null(ylim)) p <- p + ggplot2::coord_cartesian(ylim = ylim)
  p
}
