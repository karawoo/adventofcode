test_that("f02a works", {
  x <- c("forward 5", "down 5", "up 1")
  expect_equal(f02a(x), 20)
})

test_that("f02b works", {
  x <- c("forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")
  expect_equal(f02b(x), 900)
})
