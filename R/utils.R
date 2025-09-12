
is.POSIXt <- function(x) "POSIXt" %in% class(x)
is.POSIXlt <- function(x) "POSIXlt" %in% class(x)
is.POSIXct <- function(x) "POSIXct" %in% class(x)
is.Date <- function(x) "Date" %in% class(x)
is.instant <- function(x) any(c("POSIXt", "Date") %in% class(x))

is.utc <- function(tz) tz %in% c('UTC', 'GMT', 'Etc/UTC', 'Etc/GMT', 'GMT-0', 'GMT+0', 'GMT0')

unsupported_date_time <- function(x) {
  stop(sprintf("Unsupported date-time class '%s'", paste(class(x), sep = "/")))
}

date_to_posixct <- function(date, tz = "UTC") {
  utc <- .POSIXct(unclass(date) * 86400, tz = "UTC")
  if (is.utc(tz)) utc
  else C_force_tz(utc, tz, c("boundary", "post"))
}

posixct_to_date <- function(x) {
  tz <- tz(x)
  if (is.utc(tz)) {
    structure(floor(unclass(x)/86400), class = "Date", tzone = NULL)
  } else {
    x <- C_force_tz(x, "UTC", c("boundary", "post"))
    structure(floor(unclass(x)/86400), class = "Date", tzone = tz)
  }
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
    date_to_posixct(time, tz(time))
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
      posixct_to_date(ct)
    } else {
      ct
    }
  } else if (is.POSIXlt(time)) {
    as.POSIXlt.POSIXct(ct)
  } else {
    ct
  }
}

## as.POSIXct.POSIXlt does not treat Inf values correctly
lt2ct <- function(lt) {
    out <- as.POSIXct.POSIXlt(lt)
    isinf <- is.infinite(lt$sec)
    out[isinf] <- lt$sec[isinf]
    out
}

from_posixlt <- function(new, old, force_date = FALSE) {
  if (is.POSIXlt(old))
    new
  else if (is.Date(old)) {
    if (force_date) {
      as.Date.POSIXlt(new, tz = tz(old))
    } else {
      lt2ct(new)
    }
  } else {
    lt2ct(new)
  }
}

standardise_unit_name <- function(x) {
  parse_unit(x)$unit
}

# @return list(n=nr_units, unit="unit-name")
parse_unit <- function(unit) {
  .Call(C_parse_unit, as.character(unit))
}
