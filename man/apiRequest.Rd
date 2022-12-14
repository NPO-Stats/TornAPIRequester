% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apiRequest.R
\name{apiRequest}
\alias{apiRequest}
\title{Make a request from the Torn API}
\usage{
apiRequest(
  section,
  selection,
  IDtoSend = "",
  comment = "",
  timestampToSend = "",
  personalStatsToRequest = c()
)
}
\arguments{
\item{section}{The section component of your request.}

\item{selection}{The selection component of your request}

\item{IDtoSend}{The ID to send with the request. Optional, defaults to no ID.}

\item{comment}{Any additional comment to send with your request. Will always be prefixed with your
app name, which you set with initializeAPIrequester. Optional, if omitted, comment will only be your
app name.}

\item{timestampToSend}{If you are requesting past personal stats data, this is where you set the time
from which you want the data.}

\item{personalStatsToRequest}{If requesting past personal stats data, this is where you specify the stats
you want. Expected as a vector of stats. Note that the API does not permit more than ten stats at a time.}
}
\value{
The response of the API, as a list object
}
\description{
This function sends requests to the Torn API, using the pool of API keys specified at
initialization. It is thus markedly faster than using a single key, and it implements
both checks that your parameters make sense before the request is sent, and checks the
response for errors that need to be handled, such as an API key being invalid or getting
rate limited. (While the pooled respects the rate limit for its own requests, if a key is
also seeing use from a different app, getting rate limited is still possible.)
}
