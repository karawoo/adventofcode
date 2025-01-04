test_that("f03a works", {
  x <- data.frame(
    V1 = c(0L, 1L, 1L, 1L, 1L, 0L, 0L, 1L, 1L, 1L, 0L, 0L),
    V2 = c(0L, 1L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L),
    V3 = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L),
    V4 = c(0L, 1L, 1L, 1L, 0L, 1L, 1L, 0L, 0L, 0L, 1L, 1L),
    V5 = c(0L, 0L, 0L, 1L, 1L, 1L, 1L, 0L, 0L, 1L, 0L, 0L)
  )
  expect_equal(f03a(x), 198)
})

test_that("f03b works", {
  x <- data.frame(
    V1 = c(0L, 1L, 1L, 1L, 1L, 0L, 0L, 1L, 1L, 1L, 0L, 0L),
    V2 = c(0L, 1L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L),
    V3 = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L),
    V4 = c(0L, 1L, 1L, 1L, 0L, 1L, 1L, 0L, 0L, 0L, 1L, 1L),
    V5 = c(0L, 0L, 0L, 1L, 1L, 1L, 1L, 0L, 0L, 1L, 0L, 0L)
  )
  expect_equal(f03b(x), 230)
})
