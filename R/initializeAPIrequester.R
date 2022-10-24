#' Initialize the API requester
#'
#' Used to set the keys to be used for the API key pooling. This function also sets
#' the name of your app, which is passed into the API as a comment and visible to the
#' owners of the API keys used in their API settings page.
#'
#' @param appName The name of your app
#' @param apiKeys The API keys to be used for the pooling of API keys in apiRequest
#' @param apiKeyNames Names for the API keys, that will be displayed in error messages. Should normally be the name of the owner of the key. Optional, but very useful for error messages.
#' @param JSONinitData A string that can be parsed by the jsonlite-package JSON reader. Could be actual JSON in a string, or the path to a file or a URL from which to read it. Note that this format requires names for the API keys.
#'
#' @return TRUE, if initialization was successful.
#' @export

initializeAPIrequester <- function(appName = NULL, apiKeys = NULL, apiKeyNames = NULL, JSONinitData = NULL) {
  # We start by determining if we are getting our data from the JSON or from the parameters of our function.
  # If we get both, the JSON takes precedence.
  if (!missing(JSONinitData)) {
    # Try to read the JSON, and verify that it has the right structure:
    jsonData <- jsonlite::fromJSON(JSONinitData)
    if (!identical(names(jsonData), c("appName","keys")) || !is.list(jsonData$keys)) {
      stop("Invalid structure of supplied JSON data.")
    }

    # Having read the JSON, we move the data from it into the other arguments, and then proceed as if
    # those had been provided:
    appName <- jsonData$appName
    apiKeys <- unlist(jsonData$keys)
    apiKeyNames <- names(jsonData$keys)
  } else if (missing(appName) || missing(apiKeys)) {
    # If we did not get any JSON data, we need to check that the other data we need is present.
    stop("Called initializeAPIrequester with neither JSON data nor data in arguments.")
  }
  # Start by checking if the apiRequesterData environment already exists. If
  # it does, we will update its contents, if not, we start by creating it.
  if (!exists(".apiRequesterData")) {
    .apiRequesterData <<- rlang::env(
      appName = appName,
      apiKeys = apiKeys
    )
  } else {
    .apiRequesterData$appName <- appName
    .apiRequesterData$apiKeys <- apiKeys
  }

  .apiRequesterData$numAPIkeys <- length(apiKeys)
  .apiRequesterData$mostRecentKeyUsed <- 1
  .apiRequesterData$APIkeysMostRecentUse <- lapply(1:.apiRequesterData$numAPIkeys, function(n) Sys.time())
  return(TRUE)
}
