#' Get components of a date-time object
#'
#' @name time_get
#' @param time a date-time object
#' @param components a character vector of components to return. Component is
#'   one of "year", "month", "yday", "day", "mday", "wday", "hour", "minute",
#'   "second" where "day" is the same as "mday".
#' @param week_start week starting day (Default is 1, Monday). Set `week_start`
#'   option to change this globally.
#' @return A data.frame of the requested components
#' @examples
#' x <- as.POSIXct("2019-02-03")
#' time_get(x)
#' @export
time_get <- function(time,
                     components = c("year", "month", "yday", "day", "wday", "hour", "minute", "second"),
                     week_start = getOption("week_start", 1)) {

  if (length(diffs <- setdiff(components, names(components_template))) > 0) {
    stop(sprintf("Invalid components: %s", paste(diffs, collapse = ", ")))
  }

  if (length(time) == 0L) {
    return(components_template[components])
  }

  if (length(components) == 0L) {
    return(components_empty_template(length(time)))
  }

  if (is.POSIXct(time)) {
    C_time_get(time, components, week_start)
  } else if (is.Date(time)) {
    time <- as.POSIXct(time, tz = "UTC")
    C_time_get(time, components, week_start)
  } else if (is.POSIXlt(time)) {
    components[components == "day"] <- "mday"
    components <- unique(components)
    all_components <- names(timechange2posixlt)
    components <- all_components[sort(match(components, all_components))]
    compslt <- timechange2posixlt[components]
    out <- unclass(time)[compslt]

    if (!is.null(out$year))
      out$year <- out$year + 1900L
    if (!is.null(out$yday))
      out$yday <- out$yday + 1L
    if (!is.null(out$month))
      out$month <- out$month + 1L
    if (!is.null(out$wday))
      out$wday <- 1L + (out$wday + (6L - week_start)) %% 7L

    names(out) <- names(compslt)
    as.data.frame(out)
  } else {
    unsupported_date_time(time)
  }
}

timechange2posixlt <- c("year" = "year",
                        "month" = "mon",
                        "yday" = "yday",
                        "mday" = "mday",
                        "wday" = "wday",
                        "hour" = "hour",
                        "minute" = "min",
                        "second" = "sec")

components_template <- data.frame(year = integer(),
                                  month = integer(),
                                  day = integer(),
                                  yday = integer(),
                                  mday = integer(),
                                  wday = integer(),
                                  hour = integer(),
                                  minute = integer(),
                                  second = double())

components_empty_template <- function(n) {
  structure(
    list(),
    names = character(),
    class = "data.frame",
    row.names = .set_row_names(n)
  )
}