##' Arithmetics with periods
##'
##' @description Add periods to date-time objects. Periods track the change in the
##' "clock time" between two civil times. They are measured in common civil time
##' units: years, months, days, hours, minutes, and seconds.
##'
##' @details Arithmetic operations with multiple period units (years, months
##'   etc) are applied in decreasing size order, from year to second. Thus
##'   `time_add(x, months = 1, days = 3)` first adds 1 month to `x`, then ads to
##'   the resulting date 3 days.
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
##' `roll_month` and `roll_dst` can be one of the following:
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
##' * `boundary` - if rolling over a month boundary occurred due to setting units
##' smaller than month, the date is adjusted to the beginning of the month. For example,
##' `2000-01-31 01:02:03 + month = 2000-03-01 00:00:00`.
##'
##' * `first_day` - is like `boundary` but preserves the smaller units. For example,
##' `2000-01-31 01:02:03 + 1 month = 2000-03-01 01:02:03`.
##'
##' * `last_day` - is like `first` but instead of rolling forward to the first day of
##' the month, it rolls back to the last valid day of the previous month. For example,
##' `2000-01-31 01:02:03 + 1 month = 2000-02-28 01:02:03`. This is the default.
##'
##' @param time date-time object
##' @param periods string of units to add/subtract (not yet implemented) or a
##'   named list of the form `list(years = 1, months = 2, ...)`.
##' @param years,months,weeks,days,hours,minutes,seconds Units to be added to
##'   `time`. Each unit except for seconds must be expressed as integer values.
##' @param roll_month controls how addition of months and years behaves when
##'   standard arithmetic rules exceed limits of the resulting date's month. See
##'   "Details" for the description of possible values.
##' @param roll_dst controls how to adjust the updated time if it falls within a
##'   DST transition intervals. See Details.
##' @examples
##'
##' # Addition
##'
##' ## Month gap
##' x <- as.POSIXct("2000-01-31 01:02:03", tz = "America/Chicago")
##' time_add(x, months = 1, roll_month = "first_day")
##' time_add(x, months = 1, roll_month = "last_day")
##' time_add(x, months = 1, roll_month = "boundary")
##' time_add(x, months = 1, roll_month = "full")
##' time_add(x, months = 1, roll_month = "NA")
##' time_add(x, months = 1, days = 3,  roll_month = "first_day")
##' time_add(x, months = 1, days = 3,  roll_month = "last_day")
##' time_add(x, months = 1, days = 3,  roll_month = "boundary")
##' time_add(x, months = 1, days = 3,  roll_month = "full")
##' time_add(x, months = 1, days = 3,  roll_month = "NA")
##'
##' ## DST gap
##' x <- as.POSIXlt("2010-03-14 01:02:03", tz = "America/Chicago")
##' time_add(x, hours = 1, minutes = 50, roll_dst = "first_day")
##' time_add(x, hours = 1, minutes = 50, roll_dst = "last_day")
##' time_add(x, hours = 1, minutes = 50, roll_dst = "boundary")
##' time_add(x, hours = 1, minutes = 50, roll_dst = "full")
##' time_add(x, hours = 1, minutes = 50, roll_dst = "NA")
##'
##' # SUBTRACTION
##'
##' ## Month gap
##' x <- as.POSIXct("2000-03-31 01:02:03", tz = "America/Chicago")
##' time_subtract(x, months = 1, roll_month = "first_day")
##' time_subtract(x, months = 1, roll_month = "last_day")
##' time_subtract(x, months = 1, roll_month = "boundary")
##' time_subtract(x, months = 1, roll_month = "full")
##' time_subtract(x, months = 1, roll_month = "NA")
##' time_subtract(x, months = 1, days = 0,  roll_month = "first_day")
##' time_subtract(x, months = 1, days = 3,  roll_month = "first_day")
##' time_subtract(x, months = 1, days = 0,  roll_month = "last_day")
##' time_subtract(x, months = 1, days = 3,  roll_month = "last_day")
##' time_subtract(x, months = 1, days = 3,  roll_month = "boundary")
##' time_subtract(x, months = 1, days = 3,  roll_month = "full")
##' time_subtract(x, months = 1, days = 3,  roll_month = "NA")
##'
##' ## DST gap
##' y <- as.POSIXlt("2010-03-15 01:02:03", tz = "America/Chicago")
##' time_subtract(y, hours = 22, minutes = 50, roll_dst = "first")
##' time_subtract(y, hours = 22, minutes = 50, roll_dst = "last")
##' time_subtract(y, hours = 22, minutes = 50, roll_dst = "boundary")
##' time_subtract(y, hours = 22, minutes = 50, roll_dst = "full")
##' time_subtract(y, hours = 22, minutes = 50, roll_dst = "NA")
##' @export
time_add <- function(time, periods = NULL,
                     years = NULL, months = NULL, weeks = NULL, days = NULL,
                     hours = NULL, minutes = NULL, seconds = NULL,
                     roll_month = "last_day",
                     roll_dst = "first") {

  if (length(time) == 0L)
    return(time)

  roll_month <- match.arg(roll_month[[1]], .month_roll_types)
  roll_dst <- match.arg(roll_dst[[1]], .dst_roll_types)

  if (is.null(periods)) {
    periods <- list()
  } else {
    if (!is.list(periods))
      stop("character periods are not implemented yet")
  }

  argperiods <- list(years = years, months = months, weeks = weeks, days = days,
                     hours = hours, minutes = minutes, seconds = seconds)
  for (nm in names(argperiods)) {
    if (!is.null(argperiods[[nm]]))
      if (is.null(periods[[nm]]))
        periods[[nm]] <- argperiods[[nm]]
      else
        periods[[nm]] <- periods[[nm]] + argperiods[[nm]]
  }
  periods <- normalize_units_length(periods)

  if (is.POSIXct(time)) {
    C_time_add(time, periods, roll_month, roll_dst)
  } else if (is.Date(time)) {
    out <- date2posixct(time)
    attr(out, "tzone") <- "UTC"
    out <- C_time_add(out, periods, roll_month, roll_dst)
    if (is.null(hours) && is.null(minutes) && is.null(seconds)) {
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
                          years = NULL, months = NULL, weeks = NULL, days = NULL,
                          hours = NULL, minutes = NULL, seconds = NULL,
                          roll_month = "last_day",
                          roll_dst = "last") {
  if (length(time) == 0L)
    return(time)

  roll_month <- match.arg(roll_month, .month_roll_types)
  roll_dst <- match.arg(roll_dst, .dst_roll_types)

  ## fixme: no longer needed?
  if (roll_dst %in% c("skip", "full"))
    roll_dst <- "last"

  if (is.null(periods)) {
    periods <- list()
  } else {
    if (!is.list(periods))
      stop("character periods are not implemented yet")
  }
  for (nm in names(periods))
    periods[[nm]] <- -periods[[nm]]

  argperiods <- list(years = years, months = months, weeks = weeks, days = days,
                     hours = hours, minutes = minutes, seconds = seconds)
  for (nm in names(argperiods)) {
    if (!is.null(argperiods[[nm]]))
      if (is.null(periods[[nm]]))
        periods[[nm]] <- -argperiods[[nm]]
      else
        periods[[nm]] <- periods[[nm]] - argperiods[[nm]]
  }
  periods <- normalize_units_length(periods)

  time_add(time, periods, roll_month = roll_month, roll_dst = roll_dst)

}
