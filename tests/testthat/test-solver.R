test_that("riccati() refuse des coefficients invalides", {
  expect_error(riccati(a = "x"))
  expect_s3_class(riccati(a = 1, b = -2, c = 1), "riccati")
})

test_that("solve_riccati retrouve la solution exacte x/(x+1)", {
  eq <- riccati(a = 1, b = -2, c = 1)
  sol <- solve_riccati(eq, y0 = 0, interval = c(0, 5))
  exact <- sol$data$x / (sol$data$x + 1)
  expect_lt(max(abs(sol$data$y - exact)), 1e-5)
  expect_false(sol$blowup)
})

test_that("l'explosion de y' = y^2, y(0) = 1 est detectee en x = 1", {
  eq <- riccati(a = 1, b = 0, c = 0)
  sol <- solve_riccati(eq, y0 = 1, interval = c(0, 3), ymax = 1e3)
  expect_true(sol$blowup)
  expect_equal(sol$x_blowup, 1, tolerance = 1e-2)
})
