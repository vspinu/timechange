##' Package `timechange`
##'
##' Utilities for efficient updating of date-times components while accounting
##' for time-zones and day-light saving times. When it makes sense functions
##' provide a refined control of what happens in ambiguous situations through
##' `roll_month` and `roll_dst` arguments.
##'
##' @author Vitalie Spinu (\email{spinuvit@gmail.com})
##' @useDynLib timechange, .registration=TRUE
"_PACKAGE"


.onLoad <- function(libname, pkgname) {
  ## CCTZ needs zoneinfo. On windows we set it to R's own zoneinfo. On unix-like
  ## it's in "/usr/share/zoneinfo" where CCTZ looks by default. On some systems
  ## (solaris, osx) it might be in a different location. So, help ourselves by
  ## setting the TZDIR env var, but only if it's not already set.
  if (Sys.getenv("TZDIR") == "") {
    ## adapted from OlsonNames function
    tzdir <-
      if (.Platform$OS.type == "windows") {
        file.path(R.home("share"), "zoneinfo")
      } else if (!file.exists("/usr/share/zoneinfo")) {
        tzdirs <- c(file.path(R.home("share"), "zoneinfo"),
                    "/usr/share/lib/zoneinfo", "/usr/lib/zoneinfo",
                    "/usr/local/etc/zoneinfo", "/etc/zoneinfo",
                    "/usr/etc/zoneinfo")
        tzdirs <- tzdirs[file.exists(tzdirs)]
        if (length(tzdirs)) tzdirs[[1]]
        else NULL
      }
    if (!is.null(tzdir)) {
      Sys.setenv(TZDIR = tzdir)
    }
  }
}
