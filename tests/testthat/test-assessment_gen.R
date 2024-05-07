Omega <- as.data.frame(matrix(c(1, 0.8, 0.3, 0.8, 1, 0.8, 0.3, 0.8, 1), byrow = TRUE, ncol = 3))
name <- c("GMA", "Personality", "Performance")
colnames(Omega) <- name
means <- c(5, 5, 5)
sd <- c(1, 1, 1)
effect_size <- c(1.5, 0.3, 0)

test_that("assessment_gen creates dataframe", {
  df <- assessment_gen(Omega, n = 1000, means = means, sd = sd, effect_size)
  expect_true(is.data.frame(df))
})

