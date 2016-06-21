context("Places")

test_that("places API returns dict with correct keys", {
  test_string <- "I want to travel to New York"
  result <- places(test_string)

  expect_is(result, "list")
  expect_equal(length(result[[1]]), 3)

  result_v1 <- organizations(test_string, version=1)
  expect_equal(length(result_v1[[1]]), 3)
  expect_true(abs(result_v1[[1]]$confidence - result[[1]]$confidence) > .001)
})

test_that("Batch places API returns list of list of scores for the different personas axis", {
  test_string_arr <- c(
    "I want to travel to New York",
    "I want to travel to New York"
  )
  result <- places(test_string_arr)

  expect_is(result, "list")
  expect_is(result[[1]], "list")
  expect_is(result[[1]][[1]], "list")
  expect_equal(length(result[[1]][[1]]), 3)

  result_v1 <- places(test_string_arr, version=1)
  expect_true(abs(result_v1[[1]][[1]]$confidence - result[[1]][[1]]$confidence) > .001)
})
