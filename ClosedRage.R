setClass("ClosedRange", slots = list(start = "numeric", end = "numeric"))

testthat::test_that("Can create ClosedRange object", {
  closed_range <- new("ClosedRange", start = 1, end = 2)
  testthat::expect_true(!is.null(closed_range))
})
