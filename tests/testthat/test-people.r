context("People")

test_that("people API returns dict with correct keys", {
  test_string <- "Barack Obama is scheduled to give a talk next Saturday at the White House."
  result <- people(test_string)

  expect_is(result, "list")
  expect_equal(length(result[[1]]), 3)

  result_v1 <- people(test_string, version=1)
  expect_true(abs(result_v1[[1]]$confidence - result[[1]]$confidence) > .001)
})

test_that("Batch people API returns list of list of scores for the different personas axis", {
  test_string_arr <- c(
    "Barack Obama is scheduled to give a talk next Saturday at the White House.",
    "Barack Obama is scheduled to give a talk next Saturday at the White House."
  )
  result <- people(test_string_arr)

  expect_is(result, "list")
  expect_is(result[[1]], "list")
  expect_is(result[[1]][[1]], "list")
  expect_equal(length(result[[1]][[1]]), 3)

  result_v1 <- people(test_string_arr, version=1)
  expect_true(abs(result_v1[[1]][[1]]$confidence - result[[1]][[1]]$confidence) > .001)
})
