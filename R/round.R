#' Round, floor and ceiling for date-time objects
#'
#' @description \pkg{timechange} provides rounding to the nearest unit or multiple of a
#'   unit with fractional support whenever makes sense. Units can be specified flexibly
#'   as strings. All common abbreviations are supported - secs, min, mins, 2 minutes, 3
#'   years, 2s, 1d etc.
#'
#' @section Civil Time vs Absolute Time rounding:
#'
#' Rounding in civil time is done on actual clock time (ymdHMS) and is affected
#' by civil time irregularities like DST. One important characteristic of civil
#' time rounding is that floor (ceiling) does not produce civil times that are
#' bigger (smaller) than the rounded civil time.
#'
#' Absolute time rounding (with `aseconds`) is done on the absolute time (number
#' of seconds since origin), Thus, rounding of `aseconds` allows for fractional
#' seconds and multi-units larger than 60. See examples of rounding around DST
#' transition where rounding in civil time does not give same result as rounding
#' with the corresponding `X aseconds`.
#'
#' Please note that absolute rounding to fractions smaller than 1ms will result
#' to large precision errors due to the floating point representation of the
#' POSIXct objects.
#'
#' @section Note on `time_round()`:
#'
#' For rounding date-times which is exactly halfway between two consecutive units,
#' the convention is to round up. Note that this is in line with the behavior of R's
#' [base::round.POSIXt()] function but does not follow the convention of the base
#' [base::round()] function which "rounds to the even digit" per IEC 60559.
#'
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
#' @param unit a character string specifying a time unit or a multiple of a unit. Valid
#'   base periods for civil time rounding are `second`, `minute`, `hour`, `day`, `week`,
#'   `month`, `bimonth`, `quarter`, `season`, `halfyear` and `year`. The only unit for
#'   absolute time rounding is `asecond`. Other absolute units can be achieved with
#'   multiples of `asecond` ("60a", "3600a" etc). See "Details" and examples. Arbitrary
#'   unique English abbreviations are allowed. One letter abbreviations follow
#'   `strptime` formats "y", "m", "d", "M", "H", "S". Multi-unit rounding of weeks is
#'   currently not supported.
#'
#'   Rounding for a unit is performed from the parent's unit origin. For example when
#'   rounding to seconds origin is start of the minute. When rounding to days, origin is
#'   first date of the month. See examples.
#'
#'   With fractional sub-unit (unit < 1) rounding with child unit is performed
#'   instead. For example 0.5mins == 30secs, .2hours == 12min etc.
#'
#'   Please note that for fractions which don't match exactly to integer number of the
#'   child units only the integer part is used for computation. For example .7days =
#'   16.8hours will use 16 hours during the computation.
#'
#' @param change_on_boundary If NULL (the default) don't change instants on the boundary
#'   (`time_ceiling(ymd_hms('2000-01-01 00:00:00'))` is `2000-01-01 00:00:00`), but
#'   round up `Date` objects to the next boundary (`time_ceiling(ymd("2000-01-01"),
#'   "month")` is `"2000-02-01"`). When `TRUE`, instants on the boundary are rounded up
#'   to the next boundary. When `FALSE`, date-time on the boundary are never rounded up
#'   (this was the default for \pkg{lubridate} prior to `v1.6.0`. See section `Rounding
#'   Up Date Objects` below for more details.
#' @param week_start When unit is `weeks`, this is the first day of the week. Defaults
#'   to 1 (Monday).
#' @param origin Origin with respect to which to perform the rounding operation. For
#'   `unit = "aseconds"` only. Can be a vector of the same length as the input `time`
#'   vector. Defaults to the Unix origin "1970-01-01 UTC".
#' @return An object of the same class as the input object. When input is a `Date`
#'   object and unit is smaller than `day` a `POSIXct` object is returned.
#' @seealso [base::round()]
#' @examples
#'
#' ## print fractional seconds
#' options(digits.secs=6)
#'
#' x <- as.POSIXct("2009-08-03 12:01:59.23")
#' time_round(x, ".5 asec")
#' time_round(x, "sec")
#' time_round(x, "second")
#' time_round(x, "asecond")
#' time_round(x, "minute")
#' time_round(x, "5 mins")
#' time_round(x, "5M") # "M" for minute "m" for month
#' time_round(x, "hour")
#' time_round(x, "2 hours")
#' time_round(x, "2H")
#' time_round(x, "day")
#' time_round(x, "week")
#' time_round(x, "month")
#' time_round(x, "bimonth")
#' time_round(x, "quarter") == time_round(x, "3 months")
#' time_round(x, "halfyear")
#' time_round(x, "year")
#'
#' x <- as.POSIXct("2009-08-03 12:01:59.23")
#' time_floor(x, ".1 asec")
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
#' time_ceiling(x, ".1 asec")
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
#' as.POSIXct("2009-08-03 12:01:59.3", tz = "UTC") ## -> "2009-08-03 12:01:59.2 UTC"
#' time_ceiling(x, ".1 asec") ## -> "2009-08-03 12:01:59.2 UTC"
#'
#' ## Civil Time vs Absolute Time Rounding
#'
#' # "2014-11-02 01:59:59.5 EDT" before 1h backroll at 2AM
#' x <- .POSIXct(1414907999.5, tz = "America/New_York")
#' x
#' time_ceiling(x, "hour") # "2014-11-02 02:00:00 EST"
#' time_ceiling(x, "minute")
#' time_ceiling(x, "sec")
#' difftime(time_ceiling(x, "s"), x)
#' time_ceiling(x, "1a") # "2014-11-02 01:00:00 EST"
#' difftime(time_ceiling(x, "a"), x)
#'
#' # "2014-11-02 01:00:00.5 EST" after 1h backroll at 2AM
#' x <- .POSIXct(1414908000.5, tz = "America/New_York")
#' x
#' time_floor(x, "hour") # "2014-11-02 01:00:00 EST"
#' difftime(time_floor(x, "hour"), x)
#' time_floor(x, "3600a") # "2014-11-02 01:00:00 EST"  - 25m
#' difftime(time_floor(x, "a"), x)
#'
#' ## behavior on the boundary when rounding multi-units
#'
#' x <- as.POSIXct("2009-08-28 22:56:59.23", tz = "UTC")
#' time_ceiling(x, "3.4 secs")  # "2009-08-28 22:57:03.4"
#' time_ceiling(x, "50.5 secs")  # "2009-08-28 22:57:50.5"
#' time_ceiling(x, "57 min")  # "2009-08-28 22:57:00"
#' time_ceiling(x, "56 min")  # "2009-08-28 23:56:00"
#' time_ceiling(x, "7h")  # "2009-08-29 07:00:00"
#' time_ceiling(x, "7d")  # "2009-08-29 00:00:00"
#' time_ceiling(x, "8d")  # "2009-09-09 00:00:00"
#' time_ceiling(x, "8m")  # "2009-09-01 00:00:00"
#' time_ceiling(x, "6m")  # "2010-01-01 00:00:00"
#' time_ceiling(x, "7m")  # "2010-08-01 00:00:00"
#'
#' x <- as.POSIXct("2010-11-25 22:56:57", tz = "UTC")
#' time_ceiling(x, "6sec") # "2010-11-25 22:57:00"
#' time_ceiling(x, "60sec") # "2010-11-25 22:57:00"
#' time_ceiling(x, "6min") # "2010-11-25 23:00:00"
#' time_ceiling(x, "60min") # "2010-11-25 23:00:00"
#' time_ceiling(x, "4h") # "2010-11-26 00:00:00"
#' time_ceiling(x, "15d") # "2010-12-01 00:00:00"
#' time_ceiling(x, "15d") # "2010-12-01 00:00:00"
#' time_ceiling(x, "6m") # "2011-01-01 00:00:00"
#'
#'
#' ## custom origin
#' x <- as.POSIXct(c("2010-10-01 01:00:01", "2010-11-02 02:00:01"), tz = "America/New_York")
#' # 50 minutes from the day or month start
#' time_floor(x, "3000a")
#' time_floor(x, "3000a", origin = time_floor(x, "day"))
#' time_floor(x, "3000a", origin = time_floor(x, "month"))
#' time_ceiling(x, "3000a")
#' time_ceiling(x, "3000a", origin = time_floor(x, "day"))
#' time_ceiling(x, "3000a", origin = time_floor(x, "month"))
#'
#' @export
time_round <- function(time, unit = "second",
                       week_start = getOption("timechange.week_start", 1),
                       origin = unix_origin) {
  if (length(time) == 0L)
    return(time)

  nu <- parse_rounding_unit(unit)
  n <- nu$n
  unit <- nu$unit

  ct <- to_posixct(time)

  ## special case for fast absolute time rounding
  if (n == 1 && (
    unit %in% c("day", "hour", "minute", "second") ||
    (unit == "asecond" && identical(origin, unix_origin))
  )) {
    out <- round.POSIXt(ct, units = base_units[[unit]])
    return(from_posixlt(out, time, force_date = unit != "hour"))
  }

  ## FIXME: Behavior or this logic is likely slightly different from the above base
  ## rounding around DST. It has to do with hard-coded post-pre values in ceiling and
  ## floor.
  above <- unclass(C_time_ceiling(ct, unit, n, week_start, TRUE, origin))
  mid <- unclass(ct)
  below <- unclass(C_time_floor(ct, unit, n, week_start, origin))
  wabove <- (above - mid) <= (mid - below)
  wabove <- !is.na(wabove) & wabove
  below[wabove] <- above[wabove]

  from_posixct(.POSIXct(below, tz = tz(time)), time,
               force_date = !unit %in% c("hour", "minute", "second", "asecond"))
}

#' @name time_round
#' @export
time_floor <- function(time, unit = "seconds",
                       week_start = getOption("timechange.week_start", 1),
                       origin = unix_origin) {

  if (length(time) == 0)
    return(time)

  nu <- parse_rounding_unit(unit)

  from_posixct(C_time_floor(to_posixct(time), nu$unit, nu$n, as.integer(week_start), origin),
               time, force_date = !nu$unit %in% c("asecond", "second", "minute", "hour"))

}

#' @name time_round
#' @export
time_ceiling <- function(time, unit = "seconds",
                         change_on_boundary = inherits(time, "Date"),
                         week_start = getOption("timechange.week_start", 1),
                         origin = unix_origin) {

  if (length(time) == 0)
    return(time)

  nu <- parse_rounding_unit(unit)

  from_posixct(C_time_ceiling(to_posixct(time), nu$unit, nu$n, as.integer(week_start),
                              as.logical(change_on_boundary), origin),
               time, force_date = !nu$unit %in% c("second", "minute", "hour"))
}



## UTILS

base_units <- list(second = "secs", minute = "mins", hour = "hours", day = "days")

trunc_multi_limits <- c(asecond = Inf, second = 60, minute = 60, hour = 24, day = 31, year = Inf, week = 1,
                        month = 12, bimonth = 6, quarter = 4, season = 4, halfyear = 2)

parse_rounding_unit <- function(unit) {
  if (length(unit) > 1) {
    warning("'unit' argument has length larger than 1. Using first element.")
    unit <- unit[[1]]
  }
  validate_rounding_nunit(.Call(C_parse_unit, as.character(unit)))
}

# cOmpat: TODO: remove once lubridate no longer uses .normalize_multi_week_unit
# https://github.com/tidyverse/lubridate/blob/8c67d9ceca5315ef636d4727348d8914aa5552ea/R/round.r#L206
parse_units <- parse_rounding_unit

validate_rounding_nunit <- function(nunit) {
  if (nunit$n > trunc_multi_limits[[nunit$unit]])
    stop(sprintf("Rounding with %s > %d is not supported. Use aseconds for arbitrary units.",
                 nunit$unit, trunc_multi_limits[[nunit$unit]]))
  nunit
}
