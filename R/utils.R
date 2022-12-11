
is.POSIXt <- function(x) "POSIXt" %in% class(x)
is.POSIXlt <- function(x) "POSIXlt" %in% class(x)
is.POSIXct <- function(x) "POSIXct" %in% class(x)
is.Date <- function(x) "Date" %in% class(x)
is.instant <- function(x) any(c("POSIXt", "Date") %in% class(x))

unsupported_date_time <- function(x) {
  stop(sprintf("Unsupported date-time class '%s'", paste(class(x), sep = "/")))
}

date_to_posixct <- function(date, tz = "UTC") {
  utc <- .POSIXct(unclass(date) * 86400, tz = "UTC")
  if (tz == "UTC") utc
  else time_force_tz(utc, tz)
}

tz <- function(x) {
  tzone <- attr(x, "tzone")[[1]]
  if (is.null(tzone) && !is.POSIXt(x))
    return("UTC")
  if (is.character(tzone) && nzchar(tzone))
    return(tzone)
  tzone <- attr(as.POSIXlt(x[1]), "tzone")[[1]]
  if (is.null(tzone))
    return("UTC")
  tzone
}

to_posixct <- function(time) {
  if (is.POSIXct(time)) {
    # Catch rare integer POSIXct, retaining attributes
    storage.mode(time) <- "double"
    time
  } else if (is.Date(time))
    date_to_posixct(time)
  else if (is.POSIXlt(time)) {
    as.POSIXct.POSIXlt(time, tz = tz(time))
  } else {
    unsupported_date_time(time)
  }
}

from_posixct <- function(ct, time, force_date = FALSE) {
  if (is.POSIXct(time))
    ct
  else if (is.Date(time)) {
    if (force_date) {
      as.Date(ct, tz = tz(time))
    } else {
      ct
    }
  } else if (is.POSIXlt(time)) {
    as.POSIXlt.POSIXct(ct)
  } else {
    ct
  }
}

from_posixlt <- function(new, old, force_date = FALSE) {
  if (is.POSIXlt(old))
    new
  else if (is.Date(old)) {
    if (force_date) {
      as.Date(new, tz = tz(old))
    } else {
      as.POSIXct.POSIXlt(new)
    }
  } else if (is.POSIXct(old)) {
    as.POSIXct.POSIXlt(new)
  } else {
    as.POSIXct.POSIXlt(new)
  }
}

standardise_unit_name <- function(x) {
  parse_unit(x)$unit
}

#' @return list(n=nr_units, unit="unit-name")
parse_unit <- function(unit) {
  .Call(C_parse_unit, as.character(unit))
}

# Because `as.POSIXct.Date()` always uses local timezone
date2posixct <- function(x) {
  structure(unclass(x) * 86400,
            tzone = "UTC",
            class = c("POSIXct", "POSIXt"))
}
