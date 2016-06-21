context("Organizations")

test_that("organizations API returns dict with correct keys", {
  test_string <- "A year ago, the New York Times published confidential comments about ISIS' ideology by Major General Michael K. Nagata"
  result <- organizations(test_string)

  expect_is(result, "list")
  expect_equal(length(result[[1]]), 3)

  result_v1 <- organizations(test_string, version=1)
  expect_true(abs(result_v1[[1]]$confidence - result[[1]]$confidence) > .001)
})

test_that("Batch organizations API returns list of list of scores for the different personas axis", {
  test_string_arr <- c(
    "A year ago, the New York Times published confidential comments about ISIS' ideology by Major General Michael K. Nagata",
    "A year ago, the New York Times published confidential comments about ISIS' ideology by Major General Michael K. Nagata"
  )
  result <- organizations(test_string_arr)

  expect_is(result, "list")
  expect_is(result[[1]], "list")
  expect_is(result[[1]][[1]], "list")
  expect_equal(length(result[[1]][[1]]), 3)

  result_v1 <- organizations(test_string_arr, version=1)
  expect_true(abs(result_v1[[1]][[1]]$confidence - result[[1]][[1]]$confidence) > .001)
})
