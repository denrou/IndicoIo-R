context("Image Features Detection")

test_that("Image Features Detection API returns proper answer", {

  test_image <- paste(readLines("image/base64.txt"), collapse=" ")
  features <- image_recognition(test_image, top_n = 3)

  expect_is(features, "list")
  expect_equal(length(features), 3)
})

test_that("Throws error on empty or wrong image", {
  expect_error(image_recognition())
  expect_error(image_recognition(array(10)))
})

test_that("Batch Image Features Detection API returns proper answer", {

  test_image_arr <- list()
  test_image_arr[[1]] = paste(readLines("image/base64.txt"), collapse=" ")

  features <- image_recognition(test_image_arr, top_n = 3)

  expect_is(features, "list")
  expect_is(features[[1]], "list")
  expect_equal(length(features), length(test_image_arr))
  expect_equal(length(features[[1]]), 3)
})
