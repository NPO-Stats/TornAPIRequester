#' Initialize the API requester
#'
#' Used to set the keys to be used for the API key pooling. This function also sets
#' the name of your app, which is passed into the API as a comment and visible to the
#' owners of the API keys used in their API settings page.
#'
#' @param appName The name of your app
#' @param apiKeys The API keys to be used for the pooling of API keys in apiRequest
#'
#' @return TRUE, if initialization was successful.
#' @export

initializeAPIrequester <- function(appName, apiKeys) {
  # Start by checking if the apiRequesterData environment already exists. If
  # it does, we will update its contents, if not, we start by creating it.
  if (!exists(".apiRequesterData")) {
    .apiRequesterData <<- rlang::env(appName = appName,
                                     apiKeys = apiKeys)
  } else {
    .apiRequesterData$appName <- appName
    .apiRequesterData$apiKeys <- apiKeys
  }

  return(TRUE)
}
