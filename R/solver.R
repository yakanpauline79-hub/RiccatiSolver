#' Résoudre numériquement une équation de Riccati
#'
#' Résout \eqn{y' = a(x) y^2 + b(x) y + c(x)} avec la condition initiale
#' \eqn{y(x_0) = y_0} grâce à `deSolve`. Le calcul s'arrête si la solution
#' dépasse `ymax` en valeur absolue : on parle alors d'explosion.
#'
#' @param equation Objet créé par [riccati()].
#' @param y0 Valeur initiale (un nombre).
#' @param interval Vecteur `c(x0, x1)`.
#' @param n Nombre de points de la grille de sortie.
#' @param ymax Seuil au-delà duquel on considère que la solution explose.
#' @param method Méthode de `deSolve::ode` (par défaut `"lsoda"`).
#' @return Un objet de classe `riccati_solution` (liste contenant `data`,
#'   `blowup`, `x_blowup`, ...). L'abscisse d'explosion est estimée à partir
#'   du dernier point calculé.
#' @examples
#' eq <- riccati(a = 1, b = -2, c = 1)
#' sol <- solve_riccati(eq, y0 = 0, interval = c(0, 5))
#' sol
#' @export
solve_riccati <- function(equation, y0, interval = c(0, 5), n = 500,
                          ymax = 1e3, method = "lsoda") {
  if (!inherits(equation, "riccati")) {
    stop("`equation` doit \u00eatre cr\u00e9\u00e9e avec riccati().", call. = FALSE)
  }
  if (!is.numeric(y0) || length(y0) != 1) {
    stop("y0 doit \u00eatre un nombre.", call. = FALSE)
  }
  if (!is.numeric(interval) || length(interval) != 2 || interval[1] >= interval[2]) {
    stop("interval doit \u00eatre de la forme c(x0, x1) avec x0 < x1.", call. = FALSE)
  }
  if (abs(y0) >= ymax) {
    stop("|y0| doit \u00eatre inf\u00e9rieur \u00e0 ymax.", call. = FALSE)
  }

  times <- seq(interval[1], interval[2], length.out = n)
  rhs <- function(t, y, parms) {
    list(equation$a(t) * y^2 + equation$b(t) * y + equation$c(t))
  }

  erreur <- NULL
  out <- tryCatch(
    suppressWarnings(
      deSolve::ode(y = c(y = y0), times = times, func = rhs, parms = NULL,
                   method = method, atol = 1e-10, rtol = 1e-10)
    ),
    error = function(e) {
      erreur <<- conditionMessage(e)
      NULL
    }
  )

  d <- data.frame(x = out[, 1], y = out[, 2])
  hors <- !is.finite(d$y) | abs(d$y) > ymax
  # si deSolve s'arrete avant la fin, c'est que la solution explose
  blowup <- any(hors) || nrow(d) < n
  if (any(hors)) {
    d <- d[seq_len(which(hors)[1] - 1), , drop = FALSE]
  }

  # estimation de l'abscisse d'explosion : x_blow ~ x_dernier + 1 / |a * y|
  x_blowup <- NA_real_
  if (blowup) {
    xl <- d$x[nrow(d)]
    yl <- d$y[nrow(d)]
    al <- equation$a(xl)
    x_blowup <- if (abs(al * yl) > 0) xl + 1 / abs(al * yl) else xl
  }

  structure(
    list(data = d, equation = equation, y0 = y0, interval = interval,
         ymax = ymax, blowup = blowup, x_blowup = x_blowup),
    class = "riccati_solution"
  )
}

#' @export
print.riccati_solution <- function(x, ...) {
  cat("Solution num\u00e9rique de l'\u00e9quation de Riccati\n")
  cat(sprintf("Condition initiale : y(%g) = %g\n", x$interval[1], x$y0))
  cat(sprintf("Intervalle : [%g, %g], %d points\n", x$interval[1], x$interval[2], nrow(x$data)))
  if (x$blowup) {
    cat(sprintf("Explosion estim\u00e9e en x = %.4f\n", x$x_blowup))
  } else {
    cat("Aucune explosion d\u00e9tect\u00e9e sur l'intervalle\n")
  }
  invisible(x)
}
