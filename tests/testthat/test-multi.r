context("Multi API")

test_that("Predict Text returns appropriate response", {
    test_string <- "Clearly an English sentence."
    results <- analyze_text(test_string, apis=c("sentiment", "language"))

    expect_is(results, "list")
    expect_true("sentiment" %in% names(results))
    expect_true("language" %in% names(results))
    expect_true("Romanian" %in% names(results[["language"]]))
})

test_that("Throws error on empty text", {
    expect_error(analyze_text())
    expect_error(analyze_text(""))
    expect_error(analyze_text(" "))
})

test_that("Batch Predict Text returns appropriate response", {
    test_string_arr <- c("Clearly an English sentence.", "Clearly not an English sentence.")
    results <- analyze_text(test_string_arr, apis=c("sentiment", "language"))

    expect_is(results, "list")
    expect_true("sentiment" %in% names(results))
    expect_true("language" %in% names(results))
    expect_equal(length(results[["sentiment"]]), 2)
    expect_equal(length(results[["language"]]), 2)
})

test_that("Predict Image returns appropriate response", {
    fer_set <- c("Angry", "Sad", "Neutral", "Surprise", "Fear", "Happy")
    test_image <- paste(readLines("image/base64.txt"), collapse=" ")
    results <- analyze_image(test_image, apis=c("facial_features", "fer"))

    expect_is(results, "list")
    expect_true("facial_features" %in% names(results))
    expect_true("fer" %in% names(results))
    expect_true(all(names(results[["fer"]]) %in% fer_set))
})

test_that("Throws error on empty or wrong image", {
  expect_error(analyze_image())
  expect_error(analyze_image(array(10)))
})

test_that("Batch Predict Image returns appropriate response", {
    test_image_list = list()
    test_image_list[[1]] <- paste(readLines("image/base64.txt"), collapse=" ")
    test_image_list[[2]] <- paste(readLines("image/base64.txt"), collapse=" ")
    results <- analyze_image(test_image_list, apis=c("facial_features", "fer"))

    expect_is(results, "list")
    expect_true("facial_features" %in% names(results))
    expect_true("fer" %in% names(results))
    expect_equal(length(results[["facial_features"]]), 2)
    expect_equal(length(results[["fer"]]), 2)
})

test_that("Intersections API returns appropriate response (raw input)", {
    test_list = list()
    results <- intersections(c("text", "weather", "data"), apis=c("sentiment", "twitter_engagement"))
    expect_is(results, "list")
    expect_true("sentiment" %in% names(results))
})

test_that("Intersections API returns appropriate response", {
    test_list = list()
    test_list[["sentiment"]] <- c(0.1, 0.2, 0.3)
    test_list[["twitter_engagement"]] <- c(0.1, 0.2, 0.3)
    results <- intersections(test_list, c("sentiment", "twitter_engagement"))
    expect_is(results, "list")
    expect_true("sentiment" %in% names(results))
})

test_that("Intersections API needs more data", {
    test_data = c("text", "text")
    expect_error(intersections(test_data, apis = c("sentiment", "twitter_engagement")))
})

test_that("Intersections API needs fewer apis", {
    test_data = c("text", "text", "text")
    expect_error(intersections(test_data, apis = c("sentiment", "twitter_engagement", "text_tags")))
})

test_that("Intersections API needs apis of the same type", {
    test_data = c("text", "text", "text")
    expect_error(intersections(test_data, apis = c("sentiment", "fer")))
})