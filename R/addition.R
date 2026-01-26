##' Arithmetics with periods
##'
##' @description Add periods to date-time objects. Periods track the change in the
##' "clock time" between two civil times. They are measured in common civil time
##' units: years, months, days, hours, minutes, and seconds.
##'
##' @param time date-time object
##' @param periods a named list of the form `list(year = 1, month = 2, ...)`.
##' @param year,month,week,day,hour,minute,second Units to be added to `time`. Units
##'   except for seconds are converted to integer values. Components are replicated
##'   according to `vctrs` semantics, i.e vectors must be either of length 1 or same
##'   length as `time` vector.
##' @param roll_month controls how addition of months and years behaves when standard
##'   arithmetic rules exceed limits of the resulting date's month. Possible values are
##'   "preday", "boundary", "postday", "full" and "NA". See "Details" or
##'   `[(timechange::time_add())` for further details.
##' @param roll_dst is a string vector of length one or two. When two values are
##'   supplied they specify how to roll date-times when they fall into "skipped" and
##'   "repeated" DST transitions respectively. A single value is replicated to the
##'   length of two. Possible values are:
##'
##'     * `pre` - Use the time before the transition boundary.
##'     * `boundary` - Use the time exactly at the boundary transition.
##'     * `post` - Use the time after the boundary transition.
##'     * `xfirst` - crossed-first: First time which occurred when crossing the
##'        boundary. For addition with positive units pre interval is crossed first and
##'        post interval last. With negative units post interval is crossed first, pre -
##'        last. For subtraction the logic is reversed.
##'     * `xlast` - crossed-last.
##'     * `NA` - Produce NAs when the resulting time falls inside the problematic interval.
##'
##'   For example `roll_dst = c("NA", "pre") indicates that for skipped intervals
##'   return NA and for repeated times return the earlier time.
##'
##'   When multiple units are supplied the meaning of "negative period" is determined by
##'   the largest unit. For example `time_add(t, days = -1, hours = 2, roll_dst =
##'   "xfirst")` would operate as if with negative period, thus crossing the boundary
##'   from the "post" to "pre" side and "xfirst" and hence resolving to "post"
##'   time. As this might result in confusing behavior. See examples.
##'
##'   "xfirst" and "xlast" make sense for addition and subtraction only. An error is
##'   raised if an attempt is made to use them with other functions.
##' @param ... deprecated
##'
##' @details Arithmetic operations with multiple period units (years, months etc) are
##'   applied in decreasing size order, from year to second. Thus `time_add(x, month =
##'   1, days = 3)` first adds 1 month to `x`, then ads to the resulting date 3 days.
##'
##' Generally period arithmetic is undefined due to the irregular nature of
##' civil time and complexities with DST transitions. \pkg{`timechange`} allows
##' for a refined control of what happens when an addition of irregular periods
##' (years, months, days) results in "unclear" date.
##'
##' Let's start with an example. What happens when you add "1 month 3 days" to
##' "2000-01-31 01:02:03"? \pkg{`timechange`} operates by applying larger
##' periods first. First months are added`1 + 1 = February` which results in
##' non-existent time of `2000-02-31 01:02:03`. Here the `roll_month` adjustment
##' kicks in. After the adjustment, the remaining 3 days are added.
##'
##' `roll_month` can be one of the following:
##'
##' * `boundary` - if rolling over a month boundary occurred due to setting units
##' smaller than month, the date is adjusted to the beginning of the month (the
##' boundary). For example, `2000-01-31 01:02:03 + 1 month = 2000-03-01 00:00:00`.
##'
##' * `preday` - roll back to the last valid day of the previous month (pre-boundary
##' day) preserving the H, M, S units. For example, `2000-01-31 01:02:03 + 1 month =
##' 2000-02-28 01:02:03`. This is the default.
##'
##' * `postday` - roll to the first day post-boundary preserving the H, M, S units. For
##' example, `2000-01-31 01:02:03 + 1 month = 2000-03-01 01:02:03`.
##'
##' * `full` - full rolling. No adjustment is done to the simple arithmetic operations
##' (the gap is skipped as if it's not there). For example, `2000-01-31 01:02:03 + 1
##' month + 3 days` is equivalent to `2000-01-01 01:02:03 + 1 month + 31 days + 3 days`
##' resulting in `2000-03-05 01:02:03`.
##'
##' * `NA` - if end result was rolled over the month boundary due to addition of units
##' smaller than month (day, hour, minute, second) produce NA.
##'
##' * `NAym` - if intermediate date resulting from first adding years and months ends in
##' a non-existing date (e.g. Feb 31) produce NA. This is how period addition in
##' lubridate works for historical reasons.
##'
##' @examples
##'
##' # Addition
##'
##' ## Month gap
##' x <- as.POSIXct("2000-01-31 01:02:03", tz = "America/Chicago")
##' time_add(x, month = 1, roll_month = "postday")
##' time_add(x, month = 1, roll_month = "preday")
##' time_add(x, month = 1, roll_month = "boundary")
##' time_add(x, month = 1, roll_month = "full")
##' time_add(x, month = 1, roll_month = "NA")
##' time_add(x, month = 1, day = 3,  roll_month = "postday")
##' time_add(x, month = 1, day = 3,  roll_month = "preday")
##' time_add(x, month = 1, day = 3,  roll_month = "boundary")
##' time_add(x, month = 1, day = 3,  roll_month = "full")
##' time_add(x, month = 1, day = 3,  roll_month = "NA")
##'
##' ## DST gap
##' x <- as.POSIXlt("2010-03-14 01:02:03", tz = "America/Chicago")
##' time_add(x, hour = 1, minute = 50, roll_dst = "pre")
##' time_add(x, hour = 1, minute = 50, roll_dst = "boundary")
##' time_add(x, hour = 1, minute = 50, roll_dst = "post")
##' ##' time_add(x, hours = 1, minutes = 50, roll_dst = "NA")
##'
##' ## DST repeated time with cross-first and cross-last
##' (tt <- as.POSIXct(c("2014-11-02 00:15:00", "2014-11-02 02:15:00"), tz = "America/New_York"))
##' time_add(tt, hours = c(1, -1), roll_dst = "pre")
##' time_add(tt, hours = c(1, -1), roll_dst = "post")
##' time_add(tt, hours = c(1, -1), roll_dst = "xfirst")
##' time_add(tt, hours = c(1, -1), roll_dst = "xlast")
##'
##' ## DST skip with cross-first and cross-last
##' cst <- as.POSIXlt("2010-03-14 01:02:03", tz = "America/Chicago")
##' cdt <- as.POSIXlt("2010-03-14 03:02:03", tz = "America/Chicago")
##' time_add(cst, hour = 1, roll_dst = "xfirst")
##' time_add(cst, hour = 1, roll_dst = "xlast")
##' time_add(cdt, hour = -1, roll_dst = "xfirst")
##' time_add(cdt, hour = -1, roll_dst = "xlast")
##'
##' # WARNING:
##' # In the following example the overall period is treated as a negative period
##' # because the largest unit (hour) is negative. Thus `xfirst` roll_dst results in the
##' # "post" time. To avoid such confusing behavior either avoid supplying multiple
##' # units with heterogeneous sign.
##' time_add(cst, hour = -1, minute = 170, roll_dst = "xfirst")
##'
##' # SUBTRACTION
##'
##' ## Month gap
##' x <- as.POSIXct("2000-03-31 01:02:03", tz = "America/Chicago")
##' time_subtract(x, month = 1, roll_month = "postday")
##' time_subtract(x, month = 1, roll_month = "preday")
##' time_subtract(x, month = 1, roll_month = "boundary")
##' time_subtract(x, month = 1, roll_month = "full")
##' time_subtract(x, month = 1, roll_month = "NA")
##' time_subtract(x, month = 1, day = 0, roll_month = "postday")
##' time_subtract(x, month = 1, day = 3, roll_month = "postday")
##' time_subtract(x, month = 1, day = 0, roll_month = "preday")
##' time_subtract(x, month = 1, day = 3, roll_month = "preday")
##' time_subtract(x, month = 1, day = 3, roll_month = "boundary")
##' time_subtract(x, month = 1, day = 3, roll_month = "full")
##' time_subtract(x, month = 1, day = 3, roll_month = "NA")
##'
##' ## DST gap
##' y <- as.POSIXlt("2010-03-15 01:02:03", tz = "America/Chicago")
##' time_subtract(y, hour = 22, minute = 50, roll_dst = "pre")
##' time_subtract(y, hour = 22, minute = 50, roll_dst = "boundary")
##' time_subtract(y, hour = 22, minute = 50, roll_dst = "post")
##' time_subtract(y, hour = 22, minute = 50, roll_dst = "NA")
##'
##'
##' @export
time_add <- function(time, periods = NULL,
                     year = NULL, month = NULL, week = NULL, day = NULL,
                     hour = NULL, minute = NULL, second = NULL,
                     roll_month = "preday",
                     roll_dst = c("post", "pre"),
                     ...) {
  ## temporary workaround to allow for plurals
  dots <- list(...)
  if (length(dots) > 0) {
    names(dots) <- sub("s$", "", names(dots))
    periods <- c(periods, dots)
  }

  if (length(time) == 0L)
    return(time)

  if (is.null(periods)) {
    periods <- list()
  } else {
    if (!is.list(periods))
      stop("character periods are not implemented yet")
  }

  prds <- list(year = year, month = month, week = week, day = day,
               hour = hour, minute = minute, second = second)
  periods <- as.list(periods)
  for (nm in names(prds)) {
    if (!is.null(prds[[nm]]))
      if (is.null(periods[[nm]]))
        periods[[nm]] <- prds[[nm]]
      else
        periods[[nm]] <- periods[[nm]] + prds[[nm]]
  }
  periods <- periods[!vapply(periods, is.null, TRUE)]

  if (is.POSIXct(time)) {
    storage.mode(time) <- "double"
    C_time_add(time, periods, roll_month, roll_dst)
  } else if (is.Date(time)) {
    out <- date_to_posixct(time, tz(time))
    out <- C_time_add(out, periods, roll_month, roll_dst)
    if (is.null(periods[["hour"]]) &&
        is.null(periods[["minute"]]) &&
        is.null(periods[["second"]])) {
      out <- posixct_to_date(out)
    }
    out
  } else if (is.POSIXlt(time)) {
    as.POSIXlt.POSIXct(C_time_add(as.POSIXct.POSIXlt(time), periods, roll_month, roll_dst))
  } else {
    unsupported_date_time(time)
  }
}

##' @rdname time_add
##' @export
time_subtract <- function(time, periods = NULL,
                          year = NULL, month = NULL, week = NULL, day = NULL,
                          hour = NULL, minute = NULL, second = NULL,
                          roll_month = "preday",
                          roll_dst = c("pre", "post"),
                          ...) {
  if (length(time) == 0L)
    return(time)

  ## temporary workaround to allow for plurals
  dots <- list(...)
  if (length(dots) > 0) {
    names(dots) <- sub("s$", "", names(dots))
    periods <- c(periods, dots)
  }

  if (is.null(periods)) {
    periods <- list()
  } else {
    if (!is.list(periods))
      stop("character periods are not implemented yet")
  }
  for (nm in names(periods))
    periods[[nm]] <- -periods[[nm]]

  prds <- list(year = year, month = month, week = week, day = day,
               hour = hour, minute = minute, second = second)
  for (nm in names(prds)) {
    if (!is.null(prds[[nm]]))
      if (is.null(periods[[nm]]))
        periods[[nm]] <- -prds[[nm]]
      else
        periods[[nm]] <- periods[[nm]] - prds[[nm]]
  }
  periods <- periods[!vapply(periods, is.null, TRUE)]

  time_add(time, periods, roll_month = roll_month, roll_dst = roll_dst)

}
