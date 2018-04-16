#' Round, floor and ceiling for date-time objects
#'
#' @description \pkg{timechange} provides rounding to the nearest unit or
#'   multiple of a unit. Units can be specified flexibly as strings; all common
#'   abbreviations are supported - secs, min, mins, 2 minutes, 3 years, 2s, 1d
#'   etc.
#'
#' @description `time_round()` takes a date-time object and rounds it to the
#'   nearest value of the specified time unit. For rounding date-times which is
#'   exactly halfway between two consecutive units, the convention is to round
#'   up. Note that this is in line with the behavior of R's
#'   [base::round.POSIXt()] function but does not follow the convention of the
#'   base [base::round()] function which "rounds to the even digit" per IEC
#'   60559.
#'
#' For all units, with the exception of seconds and weeks, maximally allowed
#' multi-unit is capped by the parent unit (60 mins, 24 hours etc). Multi-unit
#' rounding of weeks is currently not supported.
#'
#' @section Rounding of seconds:
#'
#' Rounding of seconds follows different rules from other units. It is performed
#' directly on the numeric `POSIXct` representation, hence bypassing all
#' complexities with civil times like time-zones and DST. This also allows for
#' multi-units seconds larger than 60.
#'
#' Rounding to fractional seconds is allowed. Please note that rounding to
#' fractions smaller than 1ms will lead to large precision errors due to the
#' floating point representation of the POSIXct objects.
#'
#' @section Ceiling of `Date` objects:
#'
#' By default rounding up `Date` objects follows 3 steps:
#'
#' 1. Convert to an instant representing lower bound of the Date:
#'    `2000-01-01` --> `2000-01-01 00:00:00`
#'
#' 2. Round up to the \strong{next} closest rounding unit boundary. For example,
#'    if the rounding unit is `month` then next closest boundary of `2000-01-01`
#'    is `2000-02-01 00:00:00`.
#'
#'    The motivation for this is that the "partial" `2000-01-01` is conceptually
#'    an interval (`2000-01-01 00:00:00` -- `2000-01-02 00:00:00`) and the day
#'    hasn't started clocking yet at the exact boundary `00:00:00`. Thus, it
#'    seems wrong to round up a day to its lower boundary.
#'
#'    The behavior on the boundary can be changed by setting
#'    `change_on_boundary` to a non-`NULL` value.
#'
#' 3. If rounding unit is smaller than a day, return the instant from step 2
#'     (`POSIXct`), otherwise convert to and return a `Date` object.
#'
#' @name time_round
#' @param time a date-time vector (`Date`, `POSIXct` or `POSIXlt`)
#' @param unit a character string specifying a time unit or a multiple of a
#'   unit. Valid base units are `second`, `minute`, `hour`, `day`, `week`,
#'   `month`, `bimonth`, `quarter`, `season`, `halfyear` and `year`. Arbitrary
#'   unique English abbreviations constructor are allowed. With one letter
#'   abreviations "m" stands for month and "M" stands for minute.
#' @param change_on_boundary If NULL (the default) don't change instants on the
#'   boundary (`time_ceiling(ymd_hms('2000-01-01 00:00:00'))` is `2000-01-01
#'   00:00:00`), but round up `Date` objects to the next boundary
#'   (`time_ceiling(ymd("2000-01-01"), "month")` is `"2000-02-01"`). When
#'   `TRUE`, instants on the boundary are rounded up to the next boundary. When
#'   `FALSE`, date-time on the boundary are never rounded up (this was the
#'   default for \pkg{lubridate} prior to `v1.6.0`. See section `Rounding Up
#'   Date Objects` below for more details.
#' @param week_start when unit is `weeks` specify the reference day; 7 being
#'   Sunday.
#' @return An object of the same class as the input object. When input is a
#'   `Date` object and unit is smaller than `day` a `POSIXct` object is
#'   returned.
#' @seealso [base::round()]
#' @examples
#'
#' ## print fractional seconds
#' options(digits.secs=6)
#'
#' x <- as.POSIXct("2009-08-03 12:01:59.23")
#' time_round(x, ".5s")
#' time_round(x, "sec")
#' time_round(x, "second")
#' time_round(x, "minute")
#' time_round(x, "5 mins")
#' time_round(x, "5M") # "M" for minute "m" for month
#' time_round(x, "hour")
#' time_round(x, "2 hours")
#' time_round(x, "2h")
#' time_round(x, "day")
#' time_round(x, "week")
#' time_round(x, "month")
#' time_round(x, "bimonth")
#' time_round(x, "quarter") == time_round(x, "3 months")
#' time_round(x, "halfyear")
#' time_round(x, "year")
#'
#' x <- as.POSIXct("2009-08-03 12:01:59.23")
#' time_floor(x, ".1s")
#' time_floor(x, "second")
#' time_floor(x, "minute")
#' time_floor(x, "M")
#' time_floor(x, "hour")
#' time_floor(x, "day")
#' time_floor(x, "week")
#' time_floor(x, "m")
#' time_floor(x, "month")
#' time_floor(x, "bimonth")
#' time_floor(x, "quarter")
#' time_floor(x, "season")
#' time_floor(x, "halfyear")
#' time_floor(x, "year")
#'
#' x <- as.POSIXct("2009-08-03 12:01:59.23")
#' time_ceiling(x, ".1 sec")
#' time_ceiling(x, "second")
#' time_ceiling(x, "minute")
#' time_ceiling(x, "5 mins")
#' time_ceiling(x, "hour")
#' time_ceiling(x, "day")
#' time_ceiling(x, "week")
#' time_ceiling(x, "month")
#' time_ceiling(x, "bimonth") == time_ceiling(x, "2 months")
#' time_ceiling(x, "quarter")
#' time_ceiling(x, "season")
#' time_ceiling(x, "halfyear")
#' time_ceiling(x, "year")
#'
#' ## behavior on the boundary
#' x <- as.Date("2000-01-01")
#' time_ceiling(x, "month")
#' time_ceiling(x, "month", change_on_boundary = FALSE)
#'
#' ## As of R 3.4.2 POSIXct printing of fractional numbers is wrong
#' as.POSIXct("2009-08-03 12:01:59.3") ## -> "2009-08-03 12:01:59.2 CEST"
#' time_ceiling(x, ".1 sec") ## -> "2009-08-03 12:01:59.2 CEST"
#'
#' @export
time_round <- function(time, unit = "second", week_start = getOption("week_start", 1)) {
  if (length(time) == 0L)
    return(time)

  parsed_unit <- parse_units(unit)
  n <- parsed_unit$n
  unit <- standardise_unit_name(parsed_unit$unit)
  validate_nunit(unit, n)

  if (n == 1 && is.POSIXct(time) && unit %in% c("second", "minute", "hour", "day")) {
    ## special case for fast rounding
    round.POSIXt(time, units = base_units[[unit]])
  } else {
    ct <- to_posixct(time)
    above <- unclass(C_time_ceiling(ct, unit, n, week_start, TRUE))
    mid <- unclass(ct)
    below <- unclass(C_time_floor(ct, unit, n, week_start))
    wabove <- (above - mid) <= (mid - below)
    wabove <- !is.na(wabove) & wabove
    below[wabove] <- above[wabove]
    from_posixct(.POSIXct(below, tz = tz(time)), time,
                 force_date = !unit %in% c("second", "minute", "hour"))
  }
}

#' @description `time_floor` takes a date-time object and rounds it down to the
#'   nearest boundary of the specified time unit.
#' @name time_round
#' @export
time_floor <- function(time, unit = "seconds", week_start = getOption("week_start", 1)) {

  if (length(time) == 0)
    return(time)

  parsed_unit <- parse_units(unit)
  n <- parsed_unit$n
  unit <- standardise_unit_name(parsed_unit$unit)
  validate_nunit(unit, n)

  from_posixct(C_time_floor(to_posixct(time), unit, n, week_start),
               time, force_date = !unit %in% c("second", "minute", "hour"))

}

#' @description`time_ceiling()` takes a date-time object and rounds it up to the
#'   nearest boundary of the specified time unit.
#' @name time_round
#' @export
time_ceiling <- function(time, unit = "seconds",
                         change_on_boundary = inherits(time, "Date"),
                         week_start = getOption("week_start", 1)) {

  if (length(time) == 0)
    return(time)

  parsed_unit <- parse_units(unit)
  n <- parsed_unit$n
  unit <- standardise_unit_name(parsed_unit$unit)
  validate_nunit(unit, n)

  from_posixct(C_time_ceiling(to_posixct(time), unit, n, week_start, change_on_boundary),
               time, force_date = !unit %in% c("second", "minute", "hour"))
}



## UTILS

base_units <- list(second = "secs", minute = "mins", hour = "hours", day = "days")

trunc_multi_limits <- c(second = Inf, minute = 60, hour = 24, day = 31, year = Inf, week = 1,
                        month = 12, bimonth = 6, quarter = 4, season = 4, halfyear = 2)

validate_nunit <- function(unit, n) {
  if (n > trunc_multi_limits[[unit]])
    stop(sprintf("Rounding with %s > %d is not supported", unit, trunc_multi_limits[[unit]]))
}
