
# TornAPIRequester

<!-- badges: start -->
  [![Build Status](https://app.travis-ci.com/NPO-Stats/TornAPIRequester.svg?token=ySZu5YtBszXRx8KxzXJY&branch=master)](https://app.travis-ci.com/NPO-Stats/TornAPIRequester)
  [![Codecov test coverage](https://codecov.io/gh/NPO-Stats/TornAPIRequester/branch/master/graph/badge.svg)](https://app.codecov.io/gh/NPO-Stats/TornAPIRequester?branch=master)
  <!-- badges: end -->

Sends requests to the Torn API, with extra safety checks and error handling. Also includes built-in support for pooling of API keys, in a way that respects the rate limit.

## Installation

You can install the development version of TornAPIRequester from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("NPO-Stats/TornAPIRequester")
```

## How to use it

If you want to make requests that do not depend on using a specific API key, that is, any request that
can be done with a public key, the library automatically handles pooling of API keys for you, once
initialized:

``` r
library(TornAPIRequester)
initializeAPIrequester(appName = "Name of your app", apiKeys = c("key one", "key two"))
bogie <- apiRequest("user", "basic", 148747)
bogie$name
# [1] "bogie"
```

