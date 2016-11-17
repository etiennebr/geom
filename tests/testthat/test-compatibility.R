context("tibble compatibility")
stopifnot(require(tidyverse))
stopifnot(require(sp))

test_that("can add geom column", {
  data(meuse)
  m <- as_tibble(meuse)

  expect_silent(m %>% mutate(geom = geom(x, y)))
})
