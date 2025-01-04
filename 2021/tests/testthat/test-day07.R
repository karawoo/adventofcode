test_that("f07", {
  x <- c(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)
  expect_equal(f07a(x), 37)
  expect_equal(f07b(x), 168)
})
