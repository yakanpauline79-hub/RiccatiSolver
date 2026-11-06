test_that("la transformation lineaire redonne la meme solution", {
  eq <- riccati(a = 1, b = -2, c = 1)
  tr <- riccati_transform(eq, y0 = 0, interval = c(0, 5))
  d <- tr$solution$data
  expect_lt(max(abs(d$y - d$x / (d$x + 1))), 1e-5)
})
