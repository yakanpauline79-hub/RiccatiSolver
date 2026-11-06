#' Tracer la solution d'une équation de Riccati
#'
#' @param solution Objet créé par [solve_riccati()].
#' @param title Titre du graphique (facultatif).
#' @return Un objet `ggplot`. Une ligne verticale pointillée marque
#'   l'explosion éventuelle.
#' @examples
#' eq <- riccati(a = 1, b = -2, c = 1)
#' plot_riccati(solve_riccati(eq, y0 = 0, interval = c(0, 5)))
#' @importFrom ggplot2 .data
#' @export
plot_riccati <- function(solution, title = NULL) {
  if (!inherits(solution, "riccati_solution")) {
    stop("`solution` doit \u00eatre cr\u00e9\u00e9e avec solve_riccati().", call. = FALSE)
  }
  if (is.null(title)) title <- "Solution de l'\u00e9quation de Riccati"

  p <- ggplot2::ggplot(solution$data, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_line(linewidth = 1, colour = "#2A9D8F") +
    ggplot2::labs(title = title, x = "x", y = "y(x)") +
    ggplot2::theme_minimal()

  if (isTRUE(solution$blowup)) {
    p <- p +
      ggplot2::geom_vline(xintercept = solution$x_blowup, linetype = "dashed",
                          colour = "#E76F51") +
      ggplot2::labs(subtitle = sprintf("Explosion estim\u00e9e en x \u2248 %.3f",
                                       solution$x_blowup))
  }
  p
}

#' Champ de directions d'une équation de Riccati
#'
#' Chaque petit segment indique la pente \eqn{y' = a y^2 + b y + c} au point
#' \eqn{(x, y)}. On peut superposer une solution.
#'
#' @param equation Objet créé par [riccati()].
#' @param xlim,ylim Fenêtre d'affichage.
#' @param n Nombre de segments par direction.
#' @param solution Solution à superposer (facultatif).
#' @return Un objet `ggplot`.
#' @examples
#' eq <- riccati(a = 1, b = -2, c = 1)
#' riccati_field(eq, xlim = c(0, 5), ylim = c(-1, 3))
#' @importFrom ggplot2 .data
#' @export
riccati_field <- function(equation, xlim = c(0, 5), ylim = c(-3, 3), n = 20,
                          solution = NULL) {
  if (!inherits(equation, "riccati")) {
    stop("`equation` doit \u00eatre cr\u00e9\u00e9e avec riccati().", call. = FALSE)
  }
  g <- expand.grid(x = seq(xlim[1], xlim[2], length.out = n),
                   y = seq(ylim[1], ylim[2], length.out = n))
  pente <- equation$a(g$x) * g$y^2 + equation$b(g$x) * g$y + equation$c(g$x)

  # segments de longueur identique a l'ecran (echelles x et y rapportees a leur etendue)
  sx <- diff(xlim)
  sy <- diff(ylim)
  m <- pente * sx / sy
  ux <- 1 / sqrt(1 + m^2)
  long <- 0.7 / (n - 1)
  g$xend <- g$x + sx * long * ux
  g$yend <- g$y + sy * long * m * ux

  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = g,
                          ggplot2::aes(x = .data$x, y = .data$y,
                                       xend = .data$xend, yend = .data$yend),
                          colour = "grey55") +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) +
    ggplot2::labs(title = "Champ de directions", x = "x", y = "y(x)") +
    ggplot2::theme_minimal()

  if (!is.null(solution)) {
    p <- p + ggplot2::geom_line(data = solution$data,
                                ggplot2::aes(x = .data$x, y = .data$y),
                                colour = "#2A9D8F", linewidth = 1)
  }
  p
}
