.onLoad <- function(libname, pkgname) {
  if ("TAPIR_INIT" %in% names(Sys.getenv())) {
    initializeAPIrequester()
    packageStartupMessage("Loaded TornAPIRequester and initialized using TAPIR_INIT environment variable.")
  } else {
    packageStartupMessage("Loaded TornAPIRequester - remember to initialize with initalizeAPIrequester() before trying to make requests")
  }
}
