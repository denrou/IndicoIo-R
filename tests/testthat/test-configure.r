context("Utility function tests")

test_that("request_url functions as expected", {
  url <- request_url(cloud = FALSE, api = "sentiment", batch = FALSE)
  expect_equal(url, "http://apiv1.indico.io/sentiment")

  url <- request_url(cloud = FALSE, api = "sentiment", batch = TRUE)
  expect_equal(url, "http://apiv1.indico.io/sentiment/batch")

  url <- request_url(cloud = FALSE, api = "political", batch = TRUE)
  expect_equal(url, "http://apiv1.indico.io/political/batch")

  url <- request_url(cloud = "testing", api = "political", batch = TRUE)
  expect_equal(url, "http://testing.indico.domains/political/batch")

  .indicoio$private_cloud = "http://%s.indico.com/"
  url <- request_url(cloud = "testing", api = "political", batch = TRUE)
  expect_equal(url, "http://testing.indico.com/political/batch")
})

test_that("format_image functions as expected", {
  img <- matrix(runif(48*48, 0, 1), nrow = 48)
  result <- format_image(img)
  expect_is(result, "data.frame")
  expect_equal(length(result), 48)
});

test_that("format_images functions as expected", {
  img_list = list()
  img_list[[1]] = matrix(runif(48*48, 0, 1), nrow = 48)
  result <- format_images(img_list)
  expect_is(result, "list")
  expect_equal(length(result), 1)
  expect_is(result[[1]], "data.frame")
  expect_equal(length(result[[1]]), 48)
});

test_that("auth configuration is loaded from environment variables", {
  # store previous config
  prev_username <- Sys.getenv("INDICO_USERNAME", unset = FALSE)
  prev_password <- Sys.getenv("INDICO_PASSWORD", unset = FALSE)

  username <- "env-username"
  password <- "env-password"
  Sys.setenv('INDICO_USERNAME' = username, 'INDICO_PASSWORD' = password)
  loadEnvironmentVars()
  expect_equal(.indicoio$auth[[1]], username)
  expect_equal(.indicoio$auth[[2]], password)

  # restore after testing
  Sys.setenv('INDICO_USERNAME' = prev_username, 'INDICO_PASSWORD' = prev_password)
  loadEnvironmentVars()
})

test_that("cloud configuration is loaded from environment variables", {
  # store previous config 
  prev_cloud <- Sys.getenv("INDICO_CLOUD", unset = FALSE)

  cloud <- "env-cloud"
  Sys.setenv('INDICO_CLOUD' = cloud)
  loadEnvironmentVars()
  expect_equal(.indicoio$cloud, cloud)

  # restore after testing
  Sys.setenv('INDICO_CLOUD' = prev_cloud)
  loadEnvironmentVars()
})

test_that("auth configuration is loaded from the configuration files", {
  username <- "file-username"
  password <- "file-password" 
  content <- sprintf(
    "[auth]\nusername = %s\npassword = %s", 
    username, 
    password
  )
  loadConfigFile(content)
  expect_equal(.indicoio$auth[[1]], username)
  expect_equal(.indicoio$auth[[2]], password)

  # reset to defaults
  .indicoio$auth = FALSE
  .indicoio$cloud = FALSE
  loadConfiguration()
})

test_that("cloud configuration is loaded from the configuration files", {
  cloud <- "file-cloud"
  content <- sprintf("[private_cloud]\ncloud = %s", cloud)
  loadConfigFile(content)
  expect_equal(.indicoio$cloud, cloud)

  # reset to defaults
  .indicoio$auth = FALSE
  .indicoio$cloud = FALSE
  loadConfiguration()
})

test_that("env variables take precedence over config files", {
  prev_cloud <- Sys.getenv("INDICO_CLOUD", unset = FALSE)

  file_cloud <- "file-cloud"
  env_cloud <- "env-cloud"
  content <- sprintf("[private_cloud]\ncloud = %s", file_cloud)
  Sys.setenv('INDICO_CLOUD' = env_cloud)
  loadConfigFile(content)
  loadEnvironmentVars()
  expect_equal(.indicoio$cloud, env_cloud)

  # reset to defaults
  .indicoio$auth = FALSE
  .indicoio$cloud = FALSE
  Sys.setenv('INDICO_CLOUD' = prev_cloud)
  loadConfiguration()
})
