#' Points d'équilibre (coefficients constants)
#'
#' Pour \eqn{y' = a y^2 + b y + c} avec `a`, `b`, `c` constants, les solutions
#' constantes sont les racines de \eqn{a y^2 + b y + c = 0}. Leur stabilité est
#' donnée par le signe de \eqn{2 a y^* + b}.
#'
#' @param equation Objet créé par [riccati()] avec des coefficients constants.
#' @return Un `data.frame` avec `y_eq` et `stabilite`
#'   (`"stable"`, `"instable"` ou `"semi-stable"`).
#' @examples
#' riccati_equilibria(riccati(a = 1, b = -3, c = 2))
#' @export
riccati_equilibria <- function(equation) {
  k <- equation$const
  if (is.null(k)) {
    stop("Les points d'\u00e9quilibre supposent des coefficients constants.", call. = FALSE)
  }
  a <- k[["a"]]
  b <- k[["b"]]
  cc <- k[["c"]]
  vide <- data.frame(y_eq = numeric(0), stabilite = character(0))

  if (abs(a) < 1e-12) {
    if (abs(b) < 1e-12) return(vide)
    r <- -cc / b
  } else {
    disc <- b^2 - 4 * a * cc
    if (disc < -1e-12) return(vide)
    r <- if (abs(disc) <= 1e-12) -b / (2 * a) else (-b + c(-1, 1) * sqrt(disc)) / (2 * a)
  }
  pente <- 2 * a * r + b
  stab <- ifelse(abs(pente) < 1e-8, "semi-stable", ifelse(pente < 0, "stable", "instable"))
  data.frame(y_eq = r, stabilite = stab)
}

#' Étudier le comportement d'une solution
#'
#' Résume une solution : valeurs extrêmes, monotonie, explosion éventuelle et,
#' pour des coefficients constants, points d'équilibre et stabilité.
#'
#' @param solution Objet créé par [solve_riccati()].
#' @return Un objet de classe `riccati_study`.
#' @examples
#' eq <- riccati(a = 1, b = -2, c = 1)
#' riccati_study(solve_riccati(eq, y0 = 0, interval = c(0, 5)))
#' @export
riccati_study <- function(solution) {
  if (!inherits(solution, "riccati_solution")) {
    stop("`solution` doit \u00eatre cr\u00e9\u00e9e avec solve_riccati().", call. = FALSE)
  }
  d <- solution$data
  dy <- diff(d$y)
  monotonie <- if (all(dy >= 0)) {
    "croissante"
  } else if (all(dy <= 0)) {
    "d\u00e9croissante"
  } else {
    "non monotone"
  }
  equilibres <- if (!is.null(solution$equation$const)) riccati_equilibria(solution$equation) else NULL

  structure(
    list(y_min = min(d$y), y_max = max(d$y), y_final = d$y[nrow(d)],
         monotonie = monotonie, blowup = solution$blowup,
         x_blowup = solution$x_blowup, equilibres = equilibres),
    class = "riccati_study"
  )
}

#' @export
print.riccati_study <- function(x, ...) {
  cat("\u00c9tude de la solution\n")
  cat(sprintf("Valeurs : min = %g, max = %g, finale = %g\n", x$y_min, x$y_max, x$y_final))
  cat(sprintf("Monotonie : %s\n", x$monotonie))
  if (x$blowup) {
    cat(sprintf("Explosion estim\u00e9e en x = %.4f\n", x$x_blowup))
  } else {
    cat("Aucune explosion sur l'intervalle\n")
  }
  if (!is.null(x$equilibres)) {
    cat("Points d'\u00e9quilibre :\n")
    if (nrow(x$equilibres) == 0) cat("  aucun\n") else print(x$equilibres, row.names = FALSE)
  }
  invisible(x)
}
