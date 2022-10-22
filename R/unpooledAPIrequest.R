#' Make a request from the Torn API with a specific key
#'
#' Sends a single request to the Torn API using a specified key, instead of the pooled
#' behaviour of the apiRequest function. This is intended for when you are requesting a
#' specific user's data, and so really need to be using their specific key - for a request
#' that can be made with a public access API key, you are better off using the apiRequest
#' function.
#'
#' This function implements all the safety checks the package provides - as far as possible
#' it checks that your input makes sense for the request you are making, and it also looks at
#' what the API returns and throws errors if you got an API error that needs handling, such
#' as your key being invalid or that you have been rate limited.
#'
#' @param keyToUse The API key to use for the request.
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
#' @param errorBehaviour Soon to be deprecated.
#'
#' @return The response of the API, as a list object
#' @export
#'
#
#TODO: Add examples later:
# #' @examples
unpooledAPIRequest <- function(keyToUse,
                               section,
                               selection,
                               IDtoSend="",
                               comment="",
                               timestampToSend="",
                               personalStatsToRequest=c(),
                               errorBehaviour="silent") {
  # We need to do some input validation:
  if (!((length(section) == 1) && (section %in% c("user","property","faction","company","market","torn")))) {
    stop("Invalid choice of section for API request!")
  }
  # We could theoretically list the exact possible choices of selection, but that
  # is liable to change, so we just check that we are getting a string without spaces:
  if (!(is.character(selection) && (length(selection) == 1) && !grepl("\\s",selection))) {
    stop("Invalid choice of selection for API request!")
  }
  # The supplied ID needs to be either an integer or a string containing an int or missing:
  if (!(missing(IDtoSend) || IDtoSend == "" || ((length(IDtoSend)==1) && (is.integer(IDtoSend) || !is.na(suppressWarnings(as.integer(IDtoSend))))))) {
    stop("Invalid choice of ID to send for API request!")
  }
  # The supplied comment needs to be either missing, or a single string of length less than 20 characters:
  if (!(missing(comment) || (length(comment) == 1 && nchar(comment) < 21))) {
    stop("Invalid choice of comment to send for API request!")
  }
  # The supplied timestamp needs to be either missing/the empty string, or a single numeric
  # value between the start of Torn and the current time:
  if (!(missing(timestampToSend) || timestampToSend=="")) {
    if (length(timestampToSend) == 1 && is.numeric(timestampToSend)) {
      if (timestampToSend != as.integer(timestampToSend)) {
        stop("timestamp needs to be an integer value")
      } else {
        if (timestampToSend > as.integer(Sys.time())) {
          stop("timestamp cannot be in the future!")
        } else {
          if (timestampToSend < 1065306877) {
            stop("timestamp cannot be before the start of Torn!")
          }
        }
      }
    } else {
      stop("timestamp for API request needs to be a single numeric value")
    }
  }
  # If we are requesting some personalstats, they all need to be valid choices, there needs to be at most
  # ten of them, and we need the other fields to be a request for them:
  if (!(missing(personalStatsToRequest) || length(personalStatsToRequest) == 0)) {
    # Must all be valid options:
    if (!all(personalStatsToRequest %in% c("useractivity","activestreak","bestactivestreak","itemsbought","pointsbought","itemsboughtabroad","weaponsbought","itemssent","auctionswon","auctionsells","attackswon","attackslost","attacksdraw","bestkillstreak","moneymugged","attacksstealthed","attackhits","attackmisses","attackdamage","attackcriticalhits","respectforfaction","onehitkills","defendswon","defendslost","defendsstalemated","bestdamage","roundsfired","yourunaway","theyrunaway","highestbeaten","peoplebusted","failedbusts","peoplebought","peopleboughtspent","virusescoded","cityfinds","traveltimes","bountiesplaced","bountiesreceived","bountiescollected","totalbountyreward","revives","revivesreceived","medicalitemsused","statenhancersused","trainsreceived","totalbountyspent","drugsused","overdosed","meritsbought","personalsplaced","classifiedadsplaced","mailssent","friendmailssent","factionmailssent","companymailssent","spousemailssent","largestmug","cantaken","exttaken","kettaken","lsdtaken","opitaken","shrtaken","spetaken","pcptaken","xantaken","victaken","chahits","heahits","axehits","grehits","machits","pishits","rifhits","shohits","smghits","piehits","slahits","argtravel","mextravel","dubtravel","hawtravel","japtravel","lontravel","soutravel","switravel","chitravel","cantravel","dumpfinds","dumpsearches","itemsdumped","daysbeendonator","caytravel","jailed","hospital","attacksassisted","bloodwithdrawn","networth","missionscompleted","contractscompleted","dukecontractscompleted","missioncreditsearned","consumablesused","candyused","alcoholused","energydrinkused","nerverefills","unarmoredwon","h2hhits","organisedcrimes","territorytime","territoryjoins","arrestsmade","tokenrefills","booksread","traveltime","boostersused","rehabs","rehabcost","awards","receivedbountyvalue","racingskill","raceswon","racesentered","racingpointsearned","specialammoused","cityitemsbought","hollowammoused","tracerammoused","piercingammoused","incendiaryammoused","attackswonabroad","defendslostabroad","rankedwarringwins","retals","elo","jobpointsused","reviveskill","itemslooted","refills"))) {
      stop("Invalid personal stat requested")
    }
    # Must be no more than ten of them:
    if (length(personalStatsToRequest) > 10) {
      stop("Can only request at most ten past personal stat entries at a time.")
    }
    # Other fields must be the right shape:
    if (!(section == "user" &&
          selection == "personalstats" &&
          !(missing(timestampToSend) || timestampToSend == ""))) {
      stop("Specified personal stats to request, but other parameters are not for such a request.")
    }
  }

  # Construct the URL for our API request.
  # We start with the comment, for which we need the appName. Since we want this function to work
  # even if initializeAPIrequester hasn't been called yet, a little bit of trickery is needed:
  appName <- tryCatch(.apiRequesterData$appName,
                      error = function(e) { return("An Uninitialized TornAPIRequester Project")})
  if (missing(comment) || comment == "") {
    comment <- appName
  } else {
    comment <- paste0(appName,": ",comment)
  }
  comment <- gsub(" ","%20",comment) # No unencoded whitespace in URLs!
  # and now we make the URL itself:
  requestString <- paste0("https://api.torn.com/",section,"/",IDtoSend,
                          "?selections=",selection,
                          "&key=",keyToUse,
                          "&comment=",comment)
  # if timestamp is not missing or the empty string, we send that as well:
  if (!(missing(timestampToSend) || timestampToSend=="")) {
    requestString <- paste0(requestString,"&timestamp=",timestampToSend)
  }

  # and if personal stats to request is set, we append that to our request string:
  if (!(missing(personalStatsToRequest) || length(personalStatsToRequest) == 0)) {
    requestString <- paste0(requestString,"&stat=",paste0(personalStatsToRequest,collapse=","))
  }
  # Pull it, and let fromJSON turn the JSON into a nested list structure. We need some error handling here, because if we get an error
  # in the JSON package we really want to know exactly which request caused it, since we'll often be calling this function tens of thousands
  # of times.
  requestResult <- tryCatch(jsonlite::fromJSON(requestString),
                            error = function(c) {
                              # We do some foul dark magic to c to pass it on while attaching some new information to it.
                              # First we cast it to a fromJSONerror class error... By just modifying its class attribute:
                              class(c) <- c("fromJSONerror", class(c))
                              # Then we attach the extra information we want to attach to the error message:
                              c$message <- paste0("Error in fromJSON when trying to fetch ",requestString,": ",c$message)
                              # and we throw the same error we were given again, now with some more information attached:
                              stop(c)
                              # Doing it like this instead of with namedStop lets us preserve all the information of the
                              # original error, including any extra that fromJSON attached, but works exactly the same
                              # as a namedStop for catching it upstream. It's just a lot spookier.
                            },
                            warning = function(c) {
                              warning(paste0("Warning in fromJSON when trying to fetch ",requestString,": ",c))
                            },
                            message = function(c) {
                              message(paste0("Message in fromJSON when trying to fetch ",requestString,": ",c))
                            })

  # Handle eventual errors, in the way determined by errorBehaviour:
  if (identical(names(requestResult),"error")) {
    errorMessage <- paste0("Error ",requestResult$error$code,": ",
                           requestResult$error$error,".")
    # Errors with the key will always throw, no matter what:
    if (requestResult$error$code %in% c(1,  # 1 => Key is empty : Private key is empty in current request
                                        2,  # 2 => Incorrect Key : Private key is wrong/incorrect format.
                                        10, # 10 => Key owner is in federal jail : Current key can't be used because owner is in federal jail.
                                        13  # 13 => The key is temporarily disabled due to owner inactivity : The key owner hasn't been online for more than 7 days.
    )) {
      namedStop(errorName = "APIkeyError",
                message = paste0("An error with the key being used (",keyToUse,") occurred. You must fix this before attempting any further requests. Specifically, the error that occurred was ",errorMessage))
    }
    # Errors of excessive use or abuse will also always throw:
    if (requestResult$error$code %in% c(8, # 8 => IP block : Current IP is banned for a small period of time because of abuse.
                                        14 # 14 => Daily read limit reached : Too many records have been pulled today by this user from our cloud services.
    )) {
      namedStop(errorName = "APIkeyError",
                message = errorMessage)
    }
    # Getting blocked for the 100 requests per minute limit gives an error of a different type than the APIkeyError ones (since those must always be dealt with by human
    # action): (This error is then caught by smartAPIRequester, which will wait out the "small period of time".)
    if (requestResult$error$code == 5) { # 5 => Too many requests : Requests are blocked for a small period of time because of too many requests per user (max 100 per minute).
      namedStop(errorName = "APIthrottledError",
                message = "API request returned Error 5, Too many requests. Slow down, buckaroo!")
    }

    # Having dealt with the error forms that must always throw or give a warning,
    # we proceed to deal with the other errors according to user preference:
    if (errorBehaviour == "silent") {
      return(requestResult)
    } else {
      if (errorBehaviour == "stop") {
        # Here, we return an error type signalling that the error is from the API, not from failing the request
        # or our code being wrong: (Possible errors this could be are e,g, that you requested data on a chain ID
        # that doesn't exist.)
        namedStop(errorName = "errorFromAPI",
                  message = errorMessage)
      } else if (errorBehaviour == "warn") {
        warning(errorMessage)
        return(requestResult)
      } else {
        stop(paste0("You specified an invalid errorBehaviour. Also, your API request returned ",errorMessage))
      }
    }
  } else {
    # If there was no error, we just return the API response:
    return(requestResult)
  }
}
