test_that("f04a & b", {
  numbers <- c(
    7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25,
    12, 22, 18, 20, 8, 19, 3, 26, 1
  )
  cards <- list(`1` = structure(c(
    22L, 8L, 21L, 6L, 1L, 13L, 2L, 9L, 10L,
    12L, 17L, 23L, 14L, 3L, 20L, 11L, 4L, 16L, 18L, 15L, 0L, 24L,
    7L, 5L, 19L
  ), .Dim = c(5L, 5L), .Dimnames = list(c(
    "1", "2",
    "3", "4", "5"
  ), c("V1", "V2", "V3", "V4", "V5"))), `2` = structure(c(
    3L,
    9L, 19L, 20L, 14L, 15L, 18L, 8L, 11L, 21L, 0L, 13L, 7L, 10L,
    16L, 2L, 17L, 25L, 24L, 12L, 22L, 5L, 23L, 4L, 6L
  ), .Dim = c(
    5L,
    5L
  ), .Dimnames = list(c("6", "7", "8", "9", "10"), c(
    "V1", "V2",
    "V3", "V4", "V5"
  ))), `3` = structure(c(
    14L, 10L, 18L, 22L, 2L,
    21L, 16L, 8L, 11L, 0L, 17L, 15L, 23L, 13L, 12L, 24L, 9L, 26L,
    6L, 3L, 4L, 19L, 20L, 5L, 7L
  ), .Dim = c(5L, 5L), .Dimnames = list(
    c("11", "12", "13", "14", "15"), c(
      "V1", "V2", "V3", "V4",
      "V5"
    )
  )))
  expect_equal(f04a(numbers, cards), 4512)
  expect_equal(f04b(numbers, cards), 1924)
})
