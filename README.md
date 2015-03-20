indicoio
========

R-based client for Machine Learning API

## Install

```r
library(devtools)
devtools::install_github("IndicoDataSolutions/IndicoIo-R")
```

Documentation
------------
Found [here](http://indico.readme.io/v1.0/docs)

## Usage

```r
library(indicoio)
```

### Positive/Negative Sentiment Analysis

```r
emotion <- sentiment("Thanks everyone for the birthday wishes!!
                      It was a crazy few days ><")
emotion
cat(sprintf("This text has %s tonality", 
             ifelse(emotion > 0.5, "positive", "negative")))
```

### Political Sentiment Analysis

```r
affilation <- political("I am so proud to stand here today 
                         as Prime Minister of four nations
                         in one United Kingdom.")
affilation
most.like <- names(affilation[which.max(unlist(affilation))])
least.like <- names(affilation[which.min(unlist(affilation))])
cat(sprintf("This text is most like %s and least like %s", 
            most.like, least.like))

```

### Language Detection

```r
languages <- language("Monday: Delightful with mostly sunny skies.
                            Highs in the low 70s.")
languages
most.possible <- sort(unlist(languages), decreasing = TRUE)[1:2]
cat(sprintf("Detected %s language with probability %0.4f.\n",
            names(most.possible)[1], most.possible[1]))
cat(sprintf("Next possible is %s with probability %0.4f.", 
            names(most.possible)[2], most.possible[2]))
```

### Text Tagging

```r
categories <- text_tags("Monday: Delightful with mostly sunny skies.
                   Highs in the low 70s.")
categories
most.possible <- sort(unlist(categories), decreasing = TRUE)[1:2]
cat(sprintf("Detected category \%s with probability \%0.4f.\\n",
            names(most.possible)[1], most.possible[1]))
cat(sprintf("Next possible is \%s with probability \%0.4f.",
            names(most.possible)[2], most.possible[2]))
```


### Face Emotion Detection

```r
## Example 1
img <- matrix(runif(48*48, 0, 1), nrow = 48)
emotion <- face_emotion(img)

most.possible <- sort(unlist(emotion), decreasing = TRUE)[1:2]
cat(sprintf("Detected '%s' emotion with probability %0.4f.\n",
            names(most.possible)[1], most.possible[1]))
cat(sprintf("Next possible is '%s' emotion with probability %0.4f.", 
            names(most.possible)[2], most.possible[2]))

## Example 2
# Reads PNG file
file.face <- system.file("extdata", "face1.png", package = "indicoio")
img <- readPNG(file.face)
# Converts to grayscale
img <- 0.2126 * img[, , 1] + 0.7152 * img[, , 2] + 0.0722 * img[, , 3]
# Plots image
plot(0:1, 0:1, xlab = "", ylab = "", axes = FALSE, col = "white")
rasterImage(img, xleft = 0, ybottom = 0, xright = 1, ytop = 1)
# Detects emotion
face_emotion(img)
```

### Face Features Detection

```r
img <- matrix(runif(48*48, 0, 1), nrow = 48)
features <- face_features(img)
length(features)
```

### Image Features Detection

```r
img <- matrix(runif(64*64, 0, 1), nrow = 64)
features <- image_features(img)

length(features)
min(unlist(features))
max(unlist(features))
sum(unlist(features))
```
### Batch API Access
If you have the proper credentials, you can use any of indico's batch API methods to request that multiple datapoints be processed with a single API call:
```
batch_sentiment
batch_political
batch_language
batch_text_tags
batch_face_emotion
batch_face_features
batch_image_features
```
Authentication credentials can be provided in an `auth` argument to the API function.

Example:
```
sentiment_scores = batch_sentiment(c('Sample text', 'More sample text'), auth=c('username', 'password'))
```

For details on indico's batch API, please visit [our website](https://indico.io/pricing).

### Private Cloud Access
For high throughput applications, indico provides custom infrastructure to ensure that you can meet demand.

Authentication credentials must be provided, alongside a `cloud` argument that corresponds to the subdomain of your private cloud.

Example:
```
sentiment_scores = batch_sentiment(c('Sample text', 'More sample text'), auth=c('username', 'password'), cloud='subdomain')
```


For details on indico's private cloud access, please visit [our website](https://indico.io/pricing).

### Configuration
Environment variables and config files can be used in place of the `auth` and `cloud` arguments to indico API functions.

### Environment Variables
The following environment variables are supported:
```
$INDICO_USERNAME
$INDICO_PASSWORD
$INDICO_CLOUD
```
By setting these environment variables, you can use indico's batch API or access an indico private cloud without worrying about passing in the proper credentials with each call.  The `$INDICO_USERNAME` and `$INDICO_PASSWORD` variables are your indico authentication credentials, while the `$INDICO_CLOUD` variable is the name of the subdomain of an indico private cloud.

### Configuration files
Configuration via a `~/.indicorc` or `$PWD/.indicorc` file is also supported.  Settings supplied in a global configuration file (`~/.indicorc`) will always be overwritten by those provided in a local one (`$PWD/.indicorc`), which will in turn be overwritten by any environment variables that have been set or any arguments that have been explicitly passed into an indico API call.

The `.indicorc` files follow the `.ini` format.
A sample configuration file is provided below:
```
[auth]
username = username
passwrod = password

[private_cloud]
cloud = subdomain
```
