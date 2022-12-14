
# TornAPIRequester

<!-- badges: start -->
[![check-standard](https://github.com/NPO-Stats/TornAPIRequester/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/NPO-Stats/TornAPIRequester/actions/workflows/check-standard.yaml)
[![codecov](https://codecov.io/gh/NPO-Stats/TornAPIRequester/branch/master/graph/badge.svg?token=EKTL0CCCXI)](https://codecov.io/gh/NPO-Stats/TornAPIRequester)
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

