test_that("f06 works", {
  x <- c(3, 4, 3, 1, 2)
  expect_equal(f06a(x, 80), 5934)
  expect_equal(f06b(x, 80), 5934)
})
