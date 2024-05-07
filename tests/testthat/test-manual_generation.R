Omega <- as.data.frame(matrix(c(1, 0.8, 0.3, 0.8, 1, 0.8, 0.3, 0.8, 1), byrow = TRUE, ncol = 3))
name <- c("GMA", "Personality", "Performance")
colnames(Omega) <- name
means <- c(5, 5, 5)
sd <- c(1, 1, 1)
effect_size <- c(1.5, 0.3, 0)

test_that("manual_generation creates dataframe", {
  df <- manual_generation(Omega, n = 1000, means = means, sd = sd, effect_size)
  expect_true(is.data.frame(df))
})

test_that("seeding - same dataset", {
  df1 <- manual_generation(Omega, n = 1000, means = means, sd = sd, effect_size, seed = 1)
  df2 <- manual_generation(Omega, n = 1000, means = means, sd = sd, effect_size, seed = 1)
  expect_true(identical(df1, df2))
})

test_that("seeding - differing datasets", {
  df1 <- manual_generation(Omega, n = 1000, means = means, sd = sd, effect_size, seed = 1)
  df2 <- manual_generation(Omega, n = 1000, means = means, sd = sd, effect_size, seed = 2)
  expect_false(identical(df1, df2))
})
