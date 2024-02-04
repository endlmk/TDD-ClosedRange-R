setClass("ClosedRange", slots = list(start = "integer", end = "integer"))

is_valid_closed_range <- function(object) {
  if (object@start <= object@end) {
    TRUE
  } else {
    "Start must be less than or equal to end."
  }
}
setValidity("ClosedRange", is_valid_closed_range)

setMethod("as.character", "ClosedRange", function(x, ...) {
  paste("[", as.character(x@start), ", ", as.character(x@end), "]", sep = "")
})

setGeneric("contains", function(r, x) standardGeneric("contains"))

setMethod("contains", "ClosedRange", function(r, x) {
  r@start <= x && x <= r@end
})

testthat::test_that("Can create ClosedRange when start is less than end", {
  closed_range <- new("ClosedRange", start = 1L, end = 2L)
  testthat::expect_true(!is.null(closed_range))
})

testthat::test_that("Can create ClosedRange when start is equal to end", {
  closed_range <- new("ClosedRange", start = 2L, end = 2L)
  testthat::expect_true(!is.null(closed_range))
})

testthat::test_that("Error occurs when start is larger than end", {
  testthat::expect_error(new("ClosedRange", start = 2L, end = 1L))
})

testthat::test_that("Can show range", {
  cr <- new("ClosedRange", start = 1L, end = 2L)
  testthat::expect_equal(as.character(cr), "[1, 2]")
})

testthat::test_that("Value is in range", {
  cr <- new("ClosedRange", start = 1L, end = 2L)
  testthat::expect_true(contains(cr, 1))
})

testthat::test_that("Value is out of range", {
  cr <- new("ClosedRange", start = 1L, end = 2L)
  testthat::expect_false(contains(cr, 3))
})
