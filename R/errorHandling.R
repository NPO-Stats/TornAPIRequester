# This file contains some code to set up our error handling, but also contains a list of error names that may be thrown, for
# reference:

# Error types from APIRequester.r:
# "fromJSONerror": Thrown by doSingleRequest if it fails to get and parse the JSON object from the API. Either because of a networking
# issue or because of an issue in the JSON parser -- it does not attempt to distinguish sources of errors inside fromJSON.
#
# "APIkeyError": Thrown by doSingleRequest if a key it tried to use returned an error from the API relating to the key itself -- user
# inactivity, blocked for excessive use, etc. These must always be dealt with by a human, since ignoring them can get you banned from the API!
#
# "APIthrottledError": Thrown by doSingleRequest if its request returned an Error 5, caused by doing more than a hundred requests from a single
# user (not a single API key or single IP address doing requests). This error is caught by smartAPIRequester, which will wait out the throttle
# period but print a warning to the console.
#
# "errorFromAPI": Thrown by doSingleRequest if in error handling mode "stop", and the API request returned an error not related to the API key
# or to being throttled. Usually an error of the type "you tried requesting data the key does not have access to" or "the ID supplied is invalid".

# Constructor of conditions. Copypasted from: http://adv-r.had.co.nz/Exceptions-Debugging.html#condition-handling
# in the "Custom signal classes" section.
condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call),
    ...
  )
}
is.condition <- function(x) inherits(x, "condition")

# This function does the same thing as stop(), except it takes two parameters -- a name for the error type,
# and an error message. The errorName is just a string, but it can then be caught and handled in a tryCatch
# block, while a normal stop() just gives a generic error. So using namedStop can allow for handling
# particular to the error type, without needing to try to parse the error message. For example useful
# for dealing with HTTP errors in a different way than API key errors.
# See http://adv-r.had.co.nz/Exceptions-Debugging.html#condition-handling for where this code came from
# and how condition handling works (or should work, rather, it often doesn't for package code) in R.
#
# For example, consider these examples:
# tryCatch(stop("Testing of errors"), test = function(c) paste0("Test error: ",c$message), error = function(c) paste0("Real error: ",c$message))
# Outputs: "Test error: Testing of errors"
# tryCatch(stop("Testing of errors"), test = function(c) paste0("Test error: ",c$message), error = function(c) paste0("Real error: ",c$message))
# Outputs: "Real error: Testing of errors"
#
# So a plain stop(), which is what most package code uses, is caught by error = ..., while namedStops (which must be listed FIRST in the handlers,
# list from least specific to most specific) are caught by their name.
#
# If not caught in a tryCatch, a namedStop behaves precisely like a normal stop().
namedStop <- function(errorName, message, call = sys.call(-1),
                      ...) {
  c <- condition(c(errorName, "error"), message, call = call, ...)
  stop(c)
}
