##' Arithmetics with periods
##'
##' @description Add periods to date-time objects. Periods track the change in
##'   the "clock time" between two civil times. They are measured in common
##'   civil time units: years, months, days, hours, minutes, and seconds.
##'
##' @description Arithmetic operations with multiple period units (years, months
##'   etc) are applied in decreasing size order, from year to second. Thus
##'   `time_add(x, months = 1, days = 3)` first adds 1 to `x` and then 3
##'   days.
##'
##' Generally period arithmetic is undefined due to the irregular nature of
##' civil time and complexities with DST transitions. \pkg{`timechange`} allows
##' for a refined control of what happens when an addition of irregular periods
##' (years, months, days) results in "unclear" date.
##'
##' Let's start with an example. What happens when you add "1 month 3 days" to
##' "2000-01-31 01:02:03"? \pkg{`timechange`} operates by applying larger
##' periods first. First months are added`1 + 1 = February` which results in
##' non-existent time of `2000-02-31 01:02:03`. Here the `adjust_month`
##' adjustment kicks in:
##'
##' * `none` - no adjustment is done to the simple arithmetic operations. Days
##' are simply rolled over the calendar. Thus, `2000-01-31 01:02:03 + 1 month + 3 days`
##' is equivalent to `2000-02-01 01:02:03 + 34 days` resulting in `2000-03-05 01:02:03`.
##'
##' * `NA` - if any of the intermediate additions result in non-existent dates
##' `NA` is produced. This is the how arithmetic in `lubridate` operates.
##'
##' * `boundary` - if an intermediate computation results in a non-existent
##' date, the date is adjusted to the boundary between the months. Thus,
##' `2000-01-31 01:02:03 + month = 2000-03-01 00:00:00`. Adding 3 days results
##' in `2000-03-03 00:00:00`.
##'
##' * `firstday` - is like `boundary` but preserves the sub-day units. Thus,
##' `2000-01-31 01:02:03 + 1 month = 2000-03-01 01:02:03`. This is the default
##' for `time_add`.
##'
##' * `lastday` - is like `firstday` but instead of rolling forward to the first
##' day of the month, it rolls back to the last valid day of the previous
##' month. Thus, `2000-01-31 01:02:03 + month = 2000-02-28 01:02:03`. This is
##' the default for `time_subtract`.
##'
##' For the purpose of the month adjustment simultaneous addition of `y` years
##' and `m` months is equivalent to adding `12*y + m` months. Thus `2000-02-29 +
##' year + month` is equivalent to `2000-02-29 + 13 months` and results in a
##' valid date `2001-03-29`.
##'
##' @param time date-time object
##' @param periods string or list (currently unimplemented)
##' @param years years,months,weeks,days,hours,minutes,seconds Units to be added
##'   to `time`. Each unit except for seconds must be expressed as integer
##'   values.
##' @param adjust_month controls how addition of months and years behaves when
##'   standard arithmetic rules exceed limits of the resulting date's month. See
##'   Details for the description of possible values.
##' @param roll_dst if `TRUE` and the resulting date-time falls within the DST
##'   gap, the time is rolled forward to the closest valid civil time. If FALSE,
##'   NA is returned.
##' @export
##' @examples
##'
##' x <- as.POSIXct("2000-01-31 01:02:03", tz = "America/Chicago")
##' time_add(x, months = 1, adjust_month = "firstday")
##' time_add(x, months = 1, adjust_month = "lastday")
##' time_add(x, months = 1, adjust_month = "boundary")
##' time_add(x, months = 1, adjust_month = "none")
##' time_add(x, months = 1, adjust_month = "NA")
##'
##' time_add(x, months = 1, days = 3,  adjust_month = "firstday")
##' time_add(x, months = 1, days = 3,  adjust_month = "lastday")
##' time_add(x, months = 1, days = 3,  adjust_month = "boundary")
##' time_add(x, months = 1, days = 3,  adjust_month = "none")
##' time_add(x, months = 1, days = 3,  adjust_month = "NA")
##'
time_add <- function(time, periods = NULL,
                     years = NULL, months = NULL, weeks = NULL, days = NULL,
                     hours = NULL, minutes = NULL, seconds = NULL,
                     adjust_month = c("firstday", "lastday", "boundary", "none", "NA"),
                     roll_dst = TRUE) {

  if (length(time) == 0L)
    return(time)

  adjust_month <- match.arg(adjust_month)

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
    C_time_add(time, periods, adjust_month, roll_dst)
  } else if (is.Date(time)) {
    out <- as.POSIXct(time, tz = "UTC")
    attr(out, "tzone") <- "UTC"
    out <- C_time_add(out, periods, adjust_month, roll_dst)
    if (is.null(hours) && is.null(minutes) && is.null(seconds)) {
      out <- as.Date(out, tz = "UTC")
    }
    out
  } else if (is.POSIXlt(time)) {
    as.POSIXlt.POSIXct(C_time_add(as.POSIXct.POSIXlt(time), periods, adjust_month, roll_dst))
  } else {
    unsupported_date_time(time)
  }
}

##' @rdname time_add
##' @examples
##'
##' x <- as.POSIXct("2000-03-31 01:02:03", tz = "America/Chicago")
##' time_subtract(x, months = 1, adjust_month = "firstday")
##' time_subtract(x, months = 1, adjust_month = "lastday")
##' time_subtract(x, months = 1, adjust_month = "boundary")
##' time_subtract(x, months = 1, adjust_month = "none")
##' time_subtract(x, months = 1, adjust_month = "NA")
##'
##' time_subtract(x, months = 1, days = 3,  adjust_month = "firstday")
##' time_subtract(x, months = 1, days = 3,  adjust_month = "lastday")
##' time_subtract(x, months = 1, days = 3,  adjust_month = "boundary")
##' time_subtract(x, months = 1, days = 3,  adjust_month = "none")
##' time_subtract(x, months = 1, days = 3,  adjust_month = "NA")
##'
##' @export
time_subtract <- function(time, periods = NULL,
                          years = NULL, months = NULL, weeks = NULL, days = NULL,
                          hours = NULL, minutes = NULL, seconds = NULL,
                          adjust_month = c("lastday", "firstday", "boundary", "none", "NA"),
                          roll_dst = TRUE) {
  if (length(time) == 0L)
    return(time)


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

  time_add(time, periods, adjust_month = adjust_month, roll_dst = roll_dst)

}
