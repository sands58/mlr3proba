context("dens.penLP")

test_that("autotest", {
  set.seed(50)
  learner = lrn("dens.penLP")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
