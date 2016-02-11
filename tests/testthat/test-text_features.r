context("Text Features")

test_that("Text Features Detection API returns proper answer", {
  features <- text_features("Queen of England")
  expect_is(features, "list")
  expect_equal(length(features), 300)
})

test_that("Text Features API returns proper answer", {

  text_arr <- list()
  text_arr[[1]] = "Queen of England"

  features <- text_features(text_arr)

  expect_is(features, "list")
  expect_is(features[[1]], "list")
  expect_equal(length(features), length(text_arr))
  expect_equal(length(features[[1]]), 300)
})
