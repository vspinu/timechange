#' Time-zone manipulation
#'
#' @description `time_at_tz` returns a date-time as it would appear in a
#'   different time zone. The actual moment of time measured does not change,
#'   just the time zone it is measured in. `time_at_tz` defaults to the
#'   Universal Coordinated time zone (UTC) when an unrecognized time zone is
#'   supplied.
#'
#' @param time a date-time object (POSIXct, POSIXlt, Date) or a list of
#'   date-time objects. When a list, all contained elements are updated the new
#'   list is returned.
#' @param tz a character string containing the time zone to convert to. R must
#'   recognize the name contained in the string as a time zone on your
#'   system. For `time_force_tz` and `time_clock_at_tzs`, `tz` can be a vector
#'   of heterogeneous time-zones, in which case `time` and `tz` arguments are
#'   paired. If `time` and `tz` lengths differ, the smaller one is recycled
#'   according with usual R conventions.
#' @return a POSIXct object with the updated time zone
#' @name time-zones
#' @examples
#'
#' x <- as.POSIXct("2009-08-07 00:00:00", tz = "America/New_York")
#' time_at_tz(x, "UTC")
#' time_force_tz(x, "UTC")
#' time_force_tz(x, "Europe/Amsterdam")
#'
#' ## DST skip:
#'
#' y <- as.POSIXct("2010-03-14 02:05:05", tz = "UTC")
#' time_force_tz(y, "America/New_York", roll = FALSE)
#' time_force_tz(y, "America/New_York", roll = TRUE)
#'
#' ## Heterogeneous time-zones:
#'
#' x <- as.POSIXct(c("2009-08-07 00:00:01", "2009-08-07 01:02:03"), tz = "UTC")
#' time_force_tz(x, tz = c("America/New_York", "Europe/Amsterdam"))
#' time_force_tz(x, tz = c("America/New_York", "Europe/Amsterdam"), tzout = "America/New_York")
#'
#' x <- as.POSIXct("2009-08-07 00:00:01", tz = "UTC")
#' time_force_tz(x, tz = c("America/New_York", "Europe/Amsterdam"))
#'
#' ## Local clock:
#'
#' x <- as.POSIXct(c("2009-08-07 01:02:03", "2009-08-07 10:20:30"), tz = "UTC")
#' time_clock_at_tz(x, units = "secs")
#' time_clock_at_tz(x, units = "hours")
#' time_clock_at_tz(x, "Europe/Amsterdam")
#'
#' x <- as.POSIXct("2009-08-07 01:02:03", tz = "UTC")
#' time_clock_at_tz(x, tz = c("America/New_York", "Europe/Amsterdam", "Asia/Shanghai"), unit = "hours")
#' @export
time_at_tz <- function(time, tz = "UTC") {
  if (!C_valid_tz(tz))
    warning(sprintf("Unrecognized time zone '%s'", tz))
  if (is.list(time) && !is.POSIXlt(time)) {
    for (nm in names(time)) {
      if (is.instant(time[[nm]])) {
        time[[nm]] <- .at_tz(time[[nm]], tz)
      }
    }
    time
  } else {
    .at_tz(time, tz)
  }
}

.at_tz <- function(time, tz) {
  posixlt <- is.POSIXlt(time)
  new <-
    if (posixlt) as.POSIXct(time)
    else time
  attr(new, "tzone") <- tz
  if (posixlt) as.POSIXlt.POSIXct(new)
  else new
}

#' @description `time_force_tz` returns the date-time that has the same clock
#'   time as input time, but in the new time zone. Although the new date-time
#'   has the same clock time (e.g. the same values in the seconds, minutes,
#'   hours, etc.) it is a different moment of time than the input
#'   date-time. Computation is vectorized over both `time` and `tz` arguments.
#'
#' @param roll logical. If TRUE, and `time` falls into the DST-break, assume the
#'   next valid civil time, otherwise return NA. See examples.
#' @param tzout timezone of the output date-time vector. Meaningfull only when
#'   `tz` argument is a vector of heterogenuous time-zones. This argument is
#'   necessary because R date-time vectors cannot hold elements with different
#'   time-zones.
#' @name time-zones
#' @export
time_force_tz <- function(time, tz = "UTC", tzout = tz[[1]], roll = TRUE) {
  if (is.list(time) && !is.POSIXlt(time)) {
    for (nm in names(time)) {
      if (is.instant(time[[nm]])) {
        time[[nm]] <- .force_tz(time[[nm]], tz, tzout, roll)
      }
    }
    time
  } else {
    .force_tz(time, tz, tzout, roll)
  }
}

.force_tz <- function(time, tz, tzout, roll) {
  if (length(tz) == 1L && tz == tzout) {
    from_posixct(C_force_tz(to_posixct(time), tz, roll), time)
  } else {
    if (length(tz) < length(time))
      tz <- rep_len(tz, length(time))
    else if (length(tz) > length(time)) {
      attr <- attributes(time)
      time <- rep_len(time, length(tz))
      attributes(time) <- attr
    }
    from_posixct(C_force_tzs(to_posixct(time), tz, tzout, roll), time)
  }
}

#' @description `time_clock_at_tz` retrieves day clock time in specified time
#'   zones. Computation is vectorized over both `dt` and `tz` arguments, `tz`
#'   defaults to the timezone of `time`.
#'
#' @param units passed directly to [as.difftime()].
#' @name time-zones
#' @export
time_clock_at_tz <- function(time, tz = NULL, units = "secs") {
  if (is.list(time) && !is.POSIXlt(time)) {
    for (nm in names(time)) {
      if (is.instant(time[[nm]])) {
        time[[nm]] <- .clock_at_tz(time[[nm]], tz, units)
      }
    }
    time
  } else {
    .clock_at_tz(time, tz, units)
  }
}

.clock_at_tz <- function(time, tz, units) {
  if (is.null(tz))
    tz <- tz(time)
  if (length(tz) < length(time))
    tz <- rep_len(tz, length(time))
  else if (length(tz) > length(time)) {
    attr <- attributes(time)
    time <- rep_len(time, length(tz))
    attributes(time) <- attr
  }
  secs <- C_local_clock(as.POSIXct(time), tz)
  out <- structure(secs, units = "secs", class = "difftime")
  units(out) <- units
  out
}
