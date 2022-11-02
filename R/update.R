#' Update components of a date-time object
#'
#' @param time a date-time object
#' @param updates a named list of components
#' @param year,month,yday,wday,mday,hour,minute,second components of the date-time to be
#'   updated. All components except `second` will be converted to integer. Components
#'   are replicated according to `vctrs` semantics, i.e. vectors must be either of
#'   length 1 or same length as `time` vector.
#' @param tz time zone component (a singleton character vector)
#' @param week_start first day of the week (default is 1, Monday). Set
#'   `timechange.week_start` option to change this globally.
#' @param exact logical (TRUE), whether the update should be exact. If set to `FALSE` no
#'   rolling or unit-recycling is allowed and `NA` is produced whenever the units of the
#'   end date-time don't match the provided units. This can occur when an end date falls
#'   into a gap (e.g. DST or Feb.29) or when large components (e.g. `hour = 25`) are
#'   supplied and result in crossing boundaries of higher units. When `exact = TRUE`,
#'   `roll_month` and `roll_dst` arguments are ignored.
#' @return A date-time with the requested elements updated.  Retain its original class
#'   unless the original class is `Date` and at least one of the `hour`, `minute`,
#'   `second` or `tz` is supplied, in which case a `POSIXct` object is returned.
#' @inheritParams time_add
#' @seealso `[time_add()]`
#' @examples
#'
#' date <- as.Date("2009-02-10")
#' time_update(date, year = 2010, month = 1, mday = 1)
#' time_update(date, year = 2010, month = 13, mday = 1)
#' time_update(date, minute = 10, second = 3)
#' time_update(date, minute = 10, second = 3, tz = "America/New_York")
#'
#' time <- as.POSIXct("2015-02-03 01:02:03", tz = "America/New_York")
#' time_update(time, month = 2, mday = 31, roll_month = "preday")
#' time_update(time, month = 2, mday = 31, roll_month = "boundary")
#' time_update(time, month = 2, mday = 31, roll_month = "postday")
#' time_update(time, month = 2, mday = 31, exact = TRUE)
#' time_update(time, month = 2, mday = 31, exact = FALSE)
#'
#' ## DST skipped
#' time <- as.POSIXct("2015-02-03 01:02:03", tz = "America/New_York")
#' time_update(time, year = 2016, yday = 10)
#' time_update(time, year = 2016, yday = 10, tz = "Europe/Amsterdam")
#' time_update(time, second = 30,  tz = "America/New_York")
#' @export
time_update <- function(time, updates = NULL, year = NULL, month = NULL,
                        yday = NULL, mday = NULL, wday = NULL,
                        hour = NULL, minute = NULL, second = NULL,
                        tz = NULL,
                        roll_month = "preday",
                        roll_dst = c("boundary", "post"),
                        week_start = getOption("timechange.week_start", 1),
                        exact = FALSE) {

  if (length(time) == 0L)
    return(time)

  updts <- list(year = year, month = month,
                yday = yday, mday = mday, wday = wday,
                hour = hour, minute = minute,
                second = second)

  updates <- as.list(updates)
  for (nm in names(updts)) {
    if (!is.null(updts[[nm]]))
      if (is.null(updates[[nm]]))
        updates[[nm]] <- updts[[nm]]
      else
        updates[[nm]] <- updates[[nm]] + updts[[nm]]
  }
  updates <- updates[!vapply(updates, is.null, TRUE)]

  week_start <- as.integer(week_start)
  exact <- as.logical(exact)

  if (is.POSIXct(time)) {
    storage.mode(time) <- "double"
    C_time_update(time, updates, tz, roll_month, roll_dst, week_start, exact)
  } else if (is.Date(time)) {
    out <- date2posixct(time)
    out <- C_time_update(out, updates, tz, roll_month, roll_dst, week_start, exact)
    if (is.null(updates[["hour"]]) &&
        is.null(updates[["minute"]]) &&
        is.null(updates[["second"]]) &&
        is.null(tz)) {
      out <- as.Date(out, tz = "UTC")
    }
    out
  } else if (is.POSIXlt(time)) {
    as.POSIXlt.POSIXct(
      C_time_update(as.POSIXct.POSIXlt(time),
                    updates, tz, roll_month, roll_dst, week_start, exact))
  } else {
    unsupported_date_time(time)
  }
}
