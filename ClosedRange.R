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

setMethod(
  "contains",
  signature(r = "ClosedRange", x = "integer"),
  function(r, x) {
    r@start <= x && x <= r@end
  }
)

setMethod(
  "Compare",
  signature(e1 = "ClosedRange", e2 = "ClosedRange"),
  function(e1, e2) {
    e1@start == e2@start && e1@end == e2@end
  }
)

setGeneric("includes", function(r1, r2) standardGeneric("includes"))

setMethod(
  "includes",
  signature(r1 = "ClosedRange", r2 = "ClosedRange"),
  function(r1, r2) {
    r1@start <= r2@start && r2@end <= r1@end
  }
)

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
  testthat::expect_true(contains(cr, 1L))
})

testthat::test_that("Value is out of range", {
  cr <- new("ClosedRange", start = 1L, end = 2L)
  testthat::expect_false(contains(cr, 3L))
})

testthat::test_that("Equal range", {
  cr1 <- new("ClosedRange", start = 1L, end = 2L)
  cr2 <- new("ClosedRange", start = 1L, end = 2L)
  testthat::expect_true(cr1 == cr2)
})

testthat::test_that("Not equal range", {
  cr1 <- new("ClosedRange", start = 1L, end = 2L)
  cr2 <- new("ClosedRange", start = 1L, end = 3L)
  testthat::expect_false(cr1 == cr2)
})

testthat::test_that("Include range", {
  cr1 <- new("ClosedRange", start = 1L, end = 2L)
  cr2 <- new("ClosedRange", start = 1L, end = 2L)
  testthat::expect_true(includes(cr1, cr2))
})

testthat::test_that("Not include range1", {
  cr1 <- new("ClosedRange", start = 1L, end = 2L)
  cr2 <- new("ClosedRange", start = 1L, end = 3L)
  testthat::expect_false(includes(cr1, cr2))
})

testthat::test_that("Not include range2", {
  cr1 <- new("ClosedRange", start = 1L, end = 2L)
  cr2 <- new("ClosedRange", start = 0L, end = 1L)
  testthat::expect_false(includes(cr1, cr2))
})
