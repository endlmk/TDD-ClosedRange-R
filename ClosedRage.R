setClass("ClosedRange", slots = list(start = "integer", end = "integer"))

testthat::test_that("Can create ClosedRange object", {
  closed_range <- new("ClosedRange", start = 1L, end = 2L)
  testthat::expect_true(!is.null(closed_range))
})
