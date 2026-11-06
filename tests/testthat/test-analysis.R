test_that("points d'equilibre et stabilite de y' = (y-1)(y-2)", {
  eq <- riccati_equilibria(riccati(a = 1, b = -3, c = 2))
  expect_equal(eq$y_eq, c(1, 2))
  expect_equal(eq$stabilite, c("stable", "instable"))
})

test_that("riccati_study resume une solution", {
  eq <- riccati(a = 1, b = -2, c = 1)
  etude <- riccati_study(solve_riccati(eq, y0 = 0, interval = c(0, 5)))
  expect_s3_class(etude, "riccati_study")
  expect_equal(etude$monotonie, "croissante")
  expect_false(etude$blowup)
})

test_that("les graphiques sont des objets ggplot", {
  eq <- riccati(a = 1, b = -2, c = 1)
  sol <- solve_riccati(eq, y0 = 0, interval = c(0, 5))
  expect_s3_class(plot_riccati(sol), "ggplot")
  expect_s3_class(riccati_field(eq, ylim = c(-1, 3)), "ggplot")
  expect_s3_class(compare_riccati(eq, y0 = c(0, 2), interval = c(0, 3)), "ggplot")
})
