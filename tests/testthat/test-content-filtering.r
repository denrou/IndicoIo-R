context("Content Filtering")

test_that("Content Filtering API returns proper answer", {

  test_image <- matrix(runif(64*64, 0, 1), nrow = 64)
  probability <- content_filtering(test_image)

  expect_is(probability, "numeric")
  expect_more_than(.5, probability)
})

test_that("Throws error on empty or wrong image", {
  expect_error(content_filtering())
  expect_error(content_filtering(array(10)))
})

test_that("Batch Image Features Detection API returns proper answer", {

  test_image_arr <- list()
  test_image_arr[[1]] = matrix(runif(64*64, 0, 1), nrow = 64)

  probabilities <- batch_content_filtering(test_image_arr)

  expect_is(probabilities, "list")
  expect_is(probabilities[[1]], "numeric")
  expect_equal(length(probabilities), length(test_image_arr))
  expect_more_than(.5, probabilities[[1]])
})
