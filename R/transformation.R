#' Transformer une équation de Riccati en équation linéaire
#'
#' Avec le changement de variable \eqn{y = -u'/(a u)}, l'équation de Riccati
#' devient l'équation linéaire du second ordre
#' \eqn{u'' + p(x) u' + q(x) u = 0}, avec \eqn{p = -(b + a'/a)} et \eqn{q = a c}.
#' Cela suppose \eqn{a(x) \neq 0} et `a` dérivable. La dérivée de `a` est
#' calculée numériquement.
#'
#' @param equation Objet créé par [riccati()].
#' @param y0 Valeur initiale (facultatif). Si elle est donnée, l'équation
#'   linéaire est résolue puis on revient à `y`.
#' @param interval Intervalle de résolution (utilisé si `y0` est donné).
#' @param n Nombre de points.
#' @param ymax Seuil d'explosion pour le retour à `y`.
#' @return Un objet `riccati_linear` : fonctions `p` et `q`, et
#'   `solution` (de classe `riccati_solution`) si `y0` est fourni.
#' @examples
#' eq <- riccati(a = 1, b = -2, c = 1)
#' tr <- riccati_transform(eq, y0 = 0, interval = c(0, 5))
#' tr
#' @export
riccati_transform <- function(equation, y0 = NULL, interval = c(0, 5), n = 500,
                              ymax = 1e3) {
  if (!inherits(equation, "riccati")) {
    stop("`equation` doit \u00eatre cr\u00e9\u00e9e avec riccati().", call. = FALSE)
  }
  h <- 1e-6
  da <- function(x) (equation$a(x + h) - equation$a(x - h)) / (2 * h)
  p <- function(x) -(equation$b(x) + da(x) / equation$a(x))
  q <- function(x) equation$a(x) * equation$c(x)

  resultat <- list(p = p, q = q, equation = equation, solution = NULL)

  if (!is.null(y0)) {
    x0 <- interval[1]
    a0 <- equation$a(x0)
    if (a0 == 0) {
      stop("a(x0) = 0 : la transformation n'est pas possible.", call. = FALSE)
    }
    rhs <- function(t, s, parms) {
      list(c(s[2], -p(t) * s[2] - q(t) * s[1]))
    }
    times <- seq(interval[1], interval[2], length.out = n)
    out <- suppressWarnings(
      deSolve::ode(y = c(u = 1, v = -a0 * y0), times = times, func = rhs,
                   method = "lsoda", atol = 1e-10, rtol = 1e-10)
    )
    x <- out[, 1]
    yv <- -out[, 3] / (equation$a(x) * out[, 2])
    ok <- is.finite(yv) & abs(yv) <= ymax
    # on garde le debut valide (avant la premiere explosion)
    if (all(ok)) {
      garde <- ok
      blowup <- FALSE
      x_blowup <- NA_real_
    } else {
      premier <- which(!ok)[1]
      garde <- seq_along(x) < premier
      blowup <- TRUE
      x_blowup <- x[premier]
    }
    resultat$solution <- structure(
      list(data = data.frame(x = x[garde], y = yv[garde]), equation = equation,
           y0 = y0, interval = interval, ymax = ymax, blowup = blowup,
           x_blowup = x_blowup),
      class = "riccati_solution"
    )
  }
  structure(resultat, class = "riccati_linear")
}

#' @export
print.riccati_linear <- function(x, ...) {
  cat("\u00c9quation lin\u00e9aire associ\u00e9e : u'' + p(x) u' + q(x) u = 0\n")
  cat("avec p = -(b + a'/a), q = a c, et y = -u'/(a u)\n")
  if (!is.null(x$solution)) cat("Une solution y(x) a \u00e9t\u00e9 reconstruite (champ $solution).\n")
  invisible(x)
}
