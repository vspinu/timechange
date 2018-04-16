#' Update components of a date-time object
#'
#' `time_update` returns a date-time with the specified elements updated.
#' Elements not specified will be left unchanged.
#'
#'
#' @name time_update
#' @param time a date-time object
#' @param year,month,yday,wday,mday,day,hour,minute,second components of the
#'   date-time to be updated. All except `second` will be converted to integer.
#' @param tz time zone component (a singleton character vector)
#' @param roll logical. If `TRUE`, and the resulting date-time lands on a
#'   non-existent civil time instant (DST gap, 29th February, etc.) roll the
#'   date till next valid point. When `FALSE`, the default, produce NA for non
#'   existing date-times.
#' @param week_start week starting day (Default is 1, Monday). Set `week_start`
#'   option to change this globally.
#' @return a date object with the requested elements updated. The object will
#'   retain its original class unless the original class is `Date` and at least
#'   one of the `hour`, `minute`, `second` or `tz` is supplied, in which case a
#'   `POSIXct` object is returned.
#' @examples
#' date <- as.Date("2009-02-10")
#' time_update(date, year = 2010, month = 1, mday = 1)
#' time_update(date, year = 2010, month = 13, mday = 1)
#' time_update(date, minute = 10, second = 3)
#' time_update(date, minute = 10, second = 3, tz = "America/New_York")
#' @export
time_update <- function(time, year = NULL, month = NULL,
                        yday = NULL, mday = NULL, wday = NULL,
                        hour = NULL, minute = NULL, second = NULL,
                        tz = NULL, roll = FALSE, week_start = getOption("week_start", 1)) {

  if (length(time) == 0L)
    return(time)

  updates <- list(year = year, month = month,
                  yday = yday, mday = mday, wday = wday,
                  hour = hour, minute = minute,
                  second = second)

  updates <- normalize_units_length(updates)

  if (is.POSIXct(time)) {
    C_time_update(time, updates, tz, roll, week_start)
  } else if (is.Date(time)) {
    out <- as.POSIXct(time, tz = "UTC")
    attr(out, "tzone") <- "UTC"
    out <- C_time_update(out, updates, tz, roll, week_start)
    if (is.null(hour) && is.null(minute) && is.null(second) && is.null(tz)) {
      out <- as.Date(out, tz = "UTC")
    }
    out
  } else if (is.POSIXlt(time)) {
    as.POSIXlt.POSIXct(C_time_update(as.POSIXct.POSIXlt(time), updates, tz, roll, week_start))
  } else {
    unsupported_date_time(time)
  }
}
