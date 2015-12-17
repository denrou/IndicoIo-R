context("Organizations")

test_that("organizations API returns dict with correct keys", {
  test_string <- "A year ago, the New York Times published confidential comments about ISIS' ideology by Major General Michael K. Nagata"
  result <- organizations(test_string)

  expect_is(result, "list")
  expect_equal(length(result[[1]]), 3)
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
})
