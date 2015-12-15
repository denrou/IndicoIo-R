context("Relevance")

test_that("Relevance API returns list of floats", {
  test_string <- "president"
  test_queries <- "president"
  result <- relevance(test_string, test_queries)

  expect_is(result, "numeric")
})

test_that("Batch people API returns list of list of scores for the different personas axis", {
  test_string_arr <- c("president", "president")
  test_queries_arr <- c("president", "president")

  result <- relevance(test_string_arr, test_queries_arr)

  expect_is(result, "list")
  expect_is(result[[1]], "numeric")
  expect_equal(length(result), 2)
})
