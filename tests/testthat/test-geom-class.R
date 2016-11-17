context("geom")

test_that("geom class is coherent", {
  g <- geom(2:4, 5:7)
  expect_is(g, "geom")
  expect_is(g[1], "geom")
  expect_true(is.geom(g))
  expect_false(is.geom(list(x=1, y=2)))
  expect_false(is.geom(NULL))
  expect_error(geom(1, 2:3))
  expect_error(geom(1, "a"))
  expect_equal(length(g), 3)
  expect_equal(g[1], geom(2, 5))
  expect_equal(g[2], geom(3, 6))
  expect_equal(g[3], geom(4, 7))
  expect_error(geom(1:5, 1:5, 1:5), "not support 3D")
})

# [ -----------------------------------------------------------------------

test_that("[ is coherent", {
  g <- geom(2:4, 5:7)
  expect_silent(g[2])
  expect_error(g[5], "i <= length(x) is not TRUE", fixed=TRUE)
  expect_error(g[1:5], "i <= length(x) are not all TRUE", fixed=TRUE)
  expect_error(g[1, 2], "unused argument")
  expect_equal(g[1:2], geom(2:3, 5:6))
  expect_equal(g[c(1, 3)], g[-2])
})

test_that("[<- is coherent", {
  g <- geom(2:4, 5:7)
  expect_error(g[1] <- 1, "is.geom(value) is not TRUE", fixed=TRUE)
  expect_error(g[1] <- 1:2, "is.geom(value) is not TRUE", fixed=TRUE)
  expect_error(g[1] <- list(x=1, y=2), "is.geom(value) is not TRUE", fixed=TRUE)
  expect_silent(g[1] <- geom(9, 9))
  expect_error(g[1] <- geom(8:9, 8:9), "length(i) == length(value) is not TRUE", fixed=TRUE)
  expect_silent(g[1:2] <- geom(8:9, 8:9))
  expect_error(g[1:2] <- geom(9, 9), "length(i) == length(value) is not TRUE", fixed=TRUE)
})

test_that("$ is coherent", {
  g <- geom(2:4, 5:7)
  expect_error(g$`1`)
  expect_error(g$k)
  #expect_equal(g$x, c(2:4))
  #expect_equal(g$y, c(5:7))
})

# c ---------------------------------------------

test_that("c is coherent", {
  expect_equal(c(geom(1:3, 5:7), geom(4:5, 8:9)),
               geom(1:5, 5:9))
})

# dim
test_that("dim and length", {
  g <- geom(2:4, 5:7)
  expect_equal(length(g), 3)
  expect_null(dim(g))
})
