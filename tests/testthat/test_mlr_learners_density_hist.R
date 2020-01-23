context("dens.hist")

test_that("autotest", {
  set.seed(1)
  learner = lrn("dens.hist")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
#
#
# lrn = lrn("dens.hist")
# task = tsk("precip")
# p = lrn$train(task)
# dist = p$model
#
# test_that("pdf", {
#   expect_equal(dist$pdf(0.1), 0)
#   expect_equal(dist$pdf(0.1), 1)
# })
#
# test_that("cdf", {
#   expect_equal(dist$cdf(0.1), 0)
#   expect_equal(dist$cdf(0.1), 1)
# })




