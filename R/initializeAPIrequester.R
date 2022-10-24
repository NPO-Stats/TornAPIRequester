#' Initialize the API requester
#'
#' Used to set the keys to be used for the API key pooling. This function also sets
#' the name of your app, which is passed into the API as a comment and visible to the
#' owners of the API keys used in their API settings page.
#'
#' There are three ways of passing the required information into the function: a) Through the
#' named arguments, b) by passing a JSON string or filepath/URL to a JSON file in JSONinitData, or
#' c) by having such a JSON string/path to a string in your TAPIR_INIT environment variable.
#'
#' @param appName The name of your app
#' @param apiKeys The API keys to be used for the pooling of API keys in apiRequest
#' @param apiKeyNames Names for the API keys, that will be displayed in error messages. Should normally be the name of the owner of the key. Optional, but very useful for error messages.
#' @param JSONinitData A string that can be parsed by the jsonlite-package JSON reader. Could be actual JSON in a string, or the path to a file or a URL from which to read it. Note that this format requires names for the API keys.
#'
#' @return TRUE, if initialization was successful.
#' @export

initializeAPIrequester <- function(appName = NULL, apiKeys = NULL, apiKeyNames = NULL, JSONinitData = NULL) {
  # First off, we check if the TAPIR_INIT environment variable has been set, in which case we will treat its
  # value as our JSONinitData parameter:
  if ("TAPIR_INIT" %in% names(Sys.getenv())) {
    JSONinitData <- Sys.getenv("TAPIR_INIT")
  }
  # Then, if we have JSON init data, we parse that and treat its contents as the values of our parameters:
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

  # Now, irrespective of how we got our data, it is in the appName/apiKeys/keyNames variables,
  # and we can proceed as if that was how we got the data.

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
