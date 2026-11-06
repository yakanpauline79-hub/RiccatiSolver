#' Définir une équation de Riccati
#'
#' Décrit l'équation \eqn{y' = a(x) y^2 + b(x) y + c(x)}.
#' Chaque coefficient peut être un nombre ou une fonction de `x`.
#'
#' @param a,b,c Nombres ou fonctions de `x`.
#' @return Un objet de classe `riccati`.
#' @examples
#' eq <- riccati(a = 1, b = -2, c = 1)
#' eq
#' @export
riccati <- function(a = 1, b = 0, c = 0) {
  coefs <- list(a = a, b = b, c = c)
  valide <- vapply(coefs, function(z) is.function(z) || (is.numeric(z) && length(z) == 1),
                   logical(1))
  if (!all(valide)) {
    stop("a, b et c doivent \u00eatre des nombres ou des fonctions de x.", call. = FALSE)
  }
  constants <- vapply(coefs, is.numeric, logical(1))

  en_fonction <- function(z) {
    if (is.function(z)) return(z)
    force(z)
    function(x) rep(z, length(x))
  }

  structure(
    list(a = en_fonction(a), b = en_fonction(b), c = en_fonction(c),
         const = if (all(constants)) unlist(coefs) else NULL),
    class = "riccati"
  )
}

#' @export
print.riccati <- function(x, ...) {
  cat("\u00c9quation de Riccati : y' = a(x) y^2 + b(x) y + c(x)\n")
  if (!is.null(x$const)) {
    cat(sprintf("Coefficients constants : a = %g, b = %g, c = %g\n",
                x$const[["a"]], x$const[["b"]], x$const[["c"]]))
  } else {
    cat("Coefficients variables (fonctions de x)\n")
  }
  invisible(x)
}
