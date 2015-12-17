context("Places")

test_that("places API returns dict with correct keys", {
  test_string <- "Barack Obama is scheduled to give a talk next Saturday at the White House."
  result <- places(test_string)

  expect_is(result, "list")
  expect_equal(length(result[[1]]), 3)
})

test_that("Batch places API returns list of list of scores for the different personas axis", {
  test_string_arr <- c(
    "Barack Obama is scheduled to give a talk next Saturday at the White House.",
    "Barack Obama is scheduled to give a talk next Saturday at the White House."
  )
  result <- places(test_string_arr)

  expect_is(result, "list")
  expect_is(result[[1]], "list")
  expect_is(result[[1]][[1]], "list")
  expect_equal(length(result[[1]][[1]]), 3)
})
