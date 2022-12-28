#' Make a request from the Torn API
#'
#' This function sends requests to the Torn API, using the pool of API keys specified at
#' initialization. It is thus markedly faster than using a single key, and it implements
#' both checks that your parameters make sense before the request is sent, and checks the
#' response for errors that need to be handled, such as an API key being invalid or getting
#' rate limited. (While the pooled respects the rate limit for its own requests, if a key is
#' also seeing use from a different app, getting rate limited is still possible.)
#'
#' @param section The section component of your request.
#' @param selection The selection component of your request
#' @param IDtoSend The ID to send with the request. Optional, defaults to no ID.
#' @param comment Any additional comment to send with your request. Will always be prefixed with your
#' app name, which you set with initializeAPIrequester. Optional, if omitted, comment will only be your
#' app name.
#' @param timestampToSend If you are requesting past personal stats data, this is where you set the time
#' from which you want the data.
#' @param personalStatsToRequest If requesting past personal stats data, this is where you specify the stats
#' you want. Expected as a vector of stats. Note that the API does not permit more than ten stats at a time.
#'
#' @return The response of the API, as a list object
#' @export
#'
# TODO: Add examples to documentation:
## @examples
apiRequest <- function(section,
                       selection,
                       IDtoSend = "",
                       comment = "",
                       timestampToSend = "",
                       personalStatsToRequest = c()) {
  # We start by checking that the API requester has been initialized:
  if (!exists(".apiRequesterData", mode = "environment")) {
    namedStop("APIrequesterInitializationError", "API requester has not been initialized!")
  }

  # The algorithm we use is to rotate through the key, and store for each
  # key when it was most recently used. If that is more recently than 6/10s
  # of a second (the rate at which we are limited being 60 seconds per 100
  # requests), we wait a little bit before making the request.
  #
  # Note the somewhat cursed fact that we have to do some assignments in
  # the global environment, since global variables in R are weird.

  # Update which key to use:
  .apiRequesterData$mostRecentKeyUsed <- ifelse(.apiRequesterData$mostRecentKeyUsed == .apiRequesterData$numAPIkeys,
    1, .apiRequesterData$mostRecentKeyUsed + 1
  )
  # Record the current time:
  currentTime <- Sys.time()

  # When we are done, we will update the most recent time used of the key we used:
  on.exit(.apiRequesterData$APIkeysMostRecentUse[[.apiRequesterData$mostRecentKeyUsed]] <- Sys.time())

  # If the key was used too recently, we wait a bit first:
  timeSinceLastCall <- currentTime - .apiRequesterData$APIkeysMostRecentUse[[.apiRequesterData$mostRecentKeyUsed]]
  if (timeSinceLastCall < 6 / 10) {
    Sys.sleep(6 / 10 + 0.0005 - timeSinceLastCall)
  }

  # Now we are sure that the key is fresh enough to be reused, so we can
  # proceed to make the request:
  queryResult <- tryCatch(
    unpooledAPIrequest(
      .apiRequesterData$apiKeys[.apiRequesterData$mostRecentKeyUsed],
      section = section, selection = selection,
      IDtoSend = IDtoSend,
      comment = comment,
      timestampToSend = timestampToSend,
      personalStatsToRequest = personalStatsToRequest
    ),
    APIthrottledError = function(e) {
      # This error is caused by us breaching the 100 requests per minute limit. So, we wait
      # a minute and then try again.
      message("API limit of 100 requests per minute reached. We will wait one minute and try again.")
      Sys.sleep(60)
      unpooledAPIrequest(
        .apiRequesterData$apiKeys[.apiRequesterData$mostRecentKeyUsed],
        section = section, selection = selection,
        IDtoSend = IDtoSend,
        comment = comment,
        timestampToSend = timestampToSend,
        personalStatsToRequest = personalStatsToRequest
      )
    },
    APIbackendError = function(e) {
      # This error is caused by general API flakiness, and the recommended thing to do is
      # to wait and try again. We do so.
      message("API has suffered a backend error. We will wait ten seconds and try again.")
      Sys.sleep(10)
      unpooledAPIrequest(
        .apiRequesterData$apiKeys[.apiRequesterData$mostRecentKeyUsed],
        section = section, selection = selection,
        IDtoSend = IDtoSend,
        comment = comment,
        timestampToSend = timestampToSend,
        personalStatsToRequest = personalStatsToRequest
      )
    }
  )

  # and we return the result:
  return(queryResult)
}
