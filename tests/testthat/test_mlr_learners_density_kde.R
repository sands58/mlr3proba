context("dens.KDE")

test_that("autotest", {
  learner = lrn("dens.KDE")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})


data = data.frame("A" = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6))
a <- ks::kde(x = data$A, h = 0.1, eval.points = c(0, 0.5, 1))

task = TaskDens$new(id = "a", data, target = "A")
lrn = lrn("dens.KDE", bandwidth = 0.1, kernel = "Norm")
p = lrn$train(task)
dist = p$model$distr

test_that("pdf", {
  expect_equal(dist$pdf(0),  0.06764712)
  expect_equal(dist$pdf(0.5), 0.6161)
  expect_equal(dist$pdf(1.0), 0.6337)
})
