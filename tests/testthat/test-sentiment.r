context("Positive/Negative Sentiment Analysis")

test_that("Political API returns list for four parties", {
  
  test_string <- "Worst song ever."
  emotion <- sentiment(test_string)
  
  expect_is(emotion, "numeric")
})
