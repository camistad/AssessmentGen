test_that("autoGen dataset generated", {
  df <- auto_generation()
  expect_true(is.data.frame(df))
})

test_that("autoGen seed - same dataset", {
  df1 <- auto_generation(n = 1000, seed = 1)
  df2 <- auto_generation(n = 1000, seed = 1)
  expect_true(identical(df1, df2))
})

test_that("autoGen seed - different dataset", {
  df1 <- auto_generation(n = 1000, seed = 1)
  df2 <- auto_generation(n = 1000, seed = 2)
  expect_false(identical(df1, df2))
})