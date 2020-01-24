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


#  data = data.frame("A" = as.numeric(c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6)))
#  task = TaskDens$new(id = "a", data, target = "A")
#  lrn = lrn("dens.hist", breaks =5)
#  p = lrn$train(task)
#  lrn$predict(task)
#  dist = p$model

#test_that("pdf", {
#  expect_equal(dist$pdf(1.2), 0.5)
#  expect_equal(dist$pdf(0.5), 0.5)
#  expect_equal(dist$pdf(1), 0.75)
#  expect_equal(dist$pdf(2), 0.25)
#  expect_equal(dist$pdf(2.2), 0)
#  expect_equal(dist$pdf(1.5), 0.5)
#  expect_equal(dist$pdf(0), 0.5)
#  expect_equal(dist$pdf(0.1), 0.5)
# })

# test_that("cdf", {
#   expect_equal(dist$cdf(0.1), 0.25)
#   expect_equal(dist$cdf(0.5), 0.25)
#   expect_equal(dist$cdf(1), 0.625)
#   expect_equal(dist$cdf(2), 1)
#   expect_equal(dist$cdf(1.5), 0.875)
#   expect_equal(dist$cdf(1.6), 1)
# })


