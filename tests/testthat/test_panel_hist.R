test_that("panel.hist is available as a function", {
  expect_true(is.function(panel.hist))
  # Not invoking it directly since it expects a plotting context (used inside pairs)
})
