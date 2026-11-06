# RiccatiSolver

Package R pour définir, résoudre numériquement, représenter, comparer et analyser
les équations de Riccati :

    y' = a(x) y^2 + b(x) y + c(x)

## Installation (depuis le dossier du projet)

```r
devtools::document()
devtools::install()
```

## Exemple

```r
library(RiccatiSolver)
eq  <- riccati(a = 1, b = -2, c = 1)
sol <- solve_riccati(eq, y0 = 0, interval = c(0, 5))
plot_riccati(sol)
riccati_study(sol)
```

## Fonctions

| Fonction | Rôle |
|---|---|
| `riccati()` | Définir l'équation |
| `solve_riccati()` | Résoudre numériquement |
| `plot_riccati()` | Tracer la solution |
| `compare_riccati()` | Comparer plusieurs conditions initiales |
| `riccati_field()` | Champ de directions |
| `riccati_transform()` | Passer à une équation linéaire d'ordre 2 |
| `riccati_study()` | Étudier une solution (monotonie, explosion, équilibres) |
| `riccati_equilibria()` | Points d'équilibre et stabilité |
# RiccatiSolver
# RiccatiSolver
