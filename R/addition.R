##' Arithmetics with periods
##'
##' @description Add periods to date-time objects. Periods track the change in the
##' "clock time" between two civil times. They are measured in common civil time
##' units: years, months, days, hours, minutes, and seconds.
##'
##' @param time date-time object
##' @param periods string of units to add/subtract (not yet implemented) or a named list
##'   of the form `list(year = 1, month = 2, ...)`.
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
##'   "repeated" DST transitions respectively. Singleton strings is replicated to the
##'   length of two. Possible values are:
##'
##'     * `pre` - Use the time before the transition boundary.
##'     * `boundary` - Use the time exactly at the boundary transition.
##'     * `post` - Use the time after the boundary transition.
##'     * `NA` - Produce NAs when the resulting time falls inside the problematic interval.
##'
##'   For example `roll_dst = c("pre", "NA") indicates that for repeated intervals
##'   return the time in the earlier interval and for skipped intervals return NA.
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
##' # SUBTRACTION
##'
##' ## Month gap
##' x <- as.POSIXct("2000-03-31 01:02:03", tz = "America/Chicago")
##' time_subtract(x, month = 1, roll_month = "postday")
##' time_subtract(x, month = 1, roll_month = "preday")
##' time_subtract(x, month = 1, roll_month = "boundary")
##' time_subtract(x, month = 1, roll_month = "full")
##' time_subtract(x, month = 1, roll_month = "NA")
##' time_subtract(x, month = 1, day = 0,  roll_month = "postday")
##' time_subtract(x, month = 1, day = 3,  roll_month = "postday")
##' time_subtract(x, month = 1, day = 0,  roll_month = "preday")
##' time_subtract(x, month = 1, day = 3,  roll_month = "preday")
##' time_subtract(x, month = 1, day = 3,  roll_month = "boundary")
##' time_subtract(x, month = 1, day = 3,  roll_month = "full")
##' time_subtract(x, month = 1, day = 3,  roll_month = "NA")
##'
##' ## DST gap
##' y <- as.POSIXlt("2010-03-15 01:02:03", tz = "America/Chicago")
##' time_subtract(y, hour = 22, minute = 50, roll_dst = "pre")
##' time_subtract(y, hour = 22, minute = 50, roll_dst = "boundary")
##' time_subtract(y, hour = 22, minute = 50, roll_dst = "post")
##' time_subtract(y, hour = 22, minute = 50, roll_dst = "NA")
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
  for (nm in names(prds)) {
    if (!is.null(prds[[nm]]))
      if (is.null(periods[[nm]]))
        periods[[nm]] <- prds[[nm]]
      else
        periods[[nm]] <- periods[[nm]] + prds[[nm]]
  }
  periods <- periods[!vapply(periods, is.null, TRUE)]

  if (is.POSIXct(time)) {
    C_time_add(time, periods, roll_month, roll_dst)
  } else if (is.Date(time)) {
    out <- date2posixct(time)
    attr(out, "tzone") <- "UTC"
    out <- C_time_add(out, periods, roll_month, roll_dst)
    if (is.null(periods[["hour"]]) &&
        is.null(periods[["minute"]]) &&
        is.null(periods[["second"]])) {
      out <- as.Date(out, tz = "UTC")
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
