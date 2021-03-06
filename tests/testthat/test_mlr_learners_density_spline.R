context("dens.spline")

test_that("autotest", {
  set.seed(1)
  learner = lrn("dens.spline")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
