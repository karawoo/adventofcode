test_that("f01a works for simple input", {
  expect_equal(f01a(c(1, 2, 3)), 2)
  expect_equal(f01a(c(1, 0, 3)), 1)
})

test_that("f01b works for simple input", {
  expect_equal(f01b(c(1, 2, 3, 4, 3, 2, 1, 5)), 3)
})
