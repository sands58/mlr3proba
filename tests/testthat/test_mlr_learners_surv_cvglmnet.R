context("surv.cvglmnet")

test_that("autotest", {
  learner = mlr_learners$get("surv.cvglmnet")
  expect_learner(learner)
  result = run_autotest(learner, exclude = "feat_single")
  expect_true(result, info = result$error)
})