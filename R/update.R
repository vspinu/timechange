#' Update components of a date-time object
#'
#' @name time_update
#' @param time a date-time object
#' @param updates a named list of components
#' @param year,month,yday,wday,mday,day,hour,minute,second components of the
#'   date-time to be updated. `day` is equivalent to `mday`. All components
#'   except `second` will be converted to integer.
#' @param tz time zone component (a singleton character vector)
#' @param roll_month,roll_dst See [time_add()].
#' @param week_start first day of the week (default is 1, Monday). Set
#'   `timechange.week_start` option to change this globally.
#' @return A date-time with the requested elements updated.  Retain its original
#'   class unless the original class is `Date` and at least one of the `hour`,
#'   `minute`, `second` or `tz` is supplied, in which case a `POSIXct` object is
#'   returned.
#' @examples
#' date <- as.Date("2009-02-10")
#' time_update(date, year = 2010, month = 1, mday = 1)
#' time_update(date, year = 2010, month = 13, mday = 1)
#' time_update(date, minute = 10, second = 3)
#' time_update(date, minute = 10, second = 3, tz = "America/New_York")
#'
#' time <- as.POSIXct("2015-02-03 01:02:03", tz = "America/New_York")
#' time_update(time, year = 2016, yday = 10)
#' time_update(time, year = 2016, yday = 10, tz = "Europe/Amsterdam")
#' time_update(time, second = 30,  tz = "America/New_York")
#' @export
time_update <- function(time, updates = NULL, year = NULL, month = NULL,
                        yday = NULL, day = NULL, mday = NULL, wday = NULL,
                        hour = NULL, minute = NULL, second = NULL,
                        tz = NULL,
                        roll_month = "last",
                        roll_dst = "boundary",
                        week_start = getOption("timechange.week_start", 1)) {

  if (length(time) == 0L)
    return(time)

  roll_month <- match.arg(roll_month, .roll_types)
  roll_dst <- match.arg(roll_dst, .roll_types)

  if (!is.null(day)) {
    if (!is.null(mday)) stop("both `mday` and `day` suplied")
    mday <- day
  }

  updates <- modifyList(as.list(updates),
                        list(year = year, month = month,
                             yday = yday, mday = mday, wday = wday,
                             hour = hour, minute = minute,
                             second = second))
  updates <- normalize_units_length(updates)

  if (is.POSIXct(time)) {
    C_time_update(time, updates, tz, roll_month, roll_dst, week_start)
  } else if (is.Date(time)) {
    out <- date2posixct(time)
    attr(out, "tzone") <- "UTC"
    out <- C_time_update(out, updates, tz, roll_month, roll_dst, week_start)
    if (is.null(hour) && is.null(minute) && is.null(second) && is.null(tz)) {
      out <- as.Date(out, tz = "UTC")
    }
    out
  } else if (is.POSIXlt(time)) {
    as.POSIXlt.POSIXct(C_time_update(as.POSIXct.POSIXlt(time),
                                     updates, tz, roll_month, roll_dst, week_start))
  } else {
    unsupported_date_time(time)
  }
}
