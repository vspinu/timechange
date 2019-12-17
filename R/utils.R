
.roll_types <- c("first", "last", "boundary", "skip", "NA")

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
  if (is.POSIXct(time))
    time
  else if (is.Date(time))
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
    unsupported_date_time(ct)
  }
}

normalize_units_length <- function(units) {
  if (length(units) == 0)
    return(units)
  maxlen <- max(unlist(lapply(units, length)))
  if (maxlen > 1) {
    for (nm in names(units)) {
      len <- length(units[[nm]])
      ## len == 1 is treated at C_level
      if (len != maxlen && len > 1)
        units[[nm]] <- rep_len(units[[nm]], maxlen)
    }
  }
  units
}


## utilities copied from lubridate

standardise_unit_name <- function(x) {
  dates <- c("second", "minute", "hour", "day", "week", "month", "year",
             ## these ones are used for rounding only
             "asecond", "bimonth", "quarter", "halfyear", "season")
  y <- gsub("(.)s$", "\\1", x)
  y <- substr(y, 1, 3)
  res <- dates[pmatch(y, dates)]
  if (any(is.na(res))) {
    stop("Invalid unit name: ", paste(x[is.na(res)], collapse = ", "),
         call. = FALSE)
  }
  res
}

## return list(n=nr_units, unit="unit_name")
parse_units <- function(unit) {

  if (length(unit) > 1) {
    warning("Unit argument longer than 1. Taking first element.")
    unit <- unit[[1]]
  }

  p <- .Call(C_parse_period, as.character(unit))

  if (!is.na(p[[1]])) {

    units <- c("second", "minute", "hour", "day", "week", "month", "year")

    wp <- which(p > 0)
    if (length(wp) > 1) {
      ## Fractional units are actually supported but only when it leads to one
      ## final unit.
      stop("Cannot't parse heterogenuous or fractional units larger than one minute.")
    }

    list(n = p[wp], unit = units[wp])

  } else {
    ## allow for bimonth, halfyear, quarter, season and asecond

    m <- regexpr(" *(?<n>[0-9.,]+)? *(?<unit>[^ \t\n]+)", unit[[1]], perl = T)
    if (m > 0) {
      ## should always match
      nms <- attr(m, "capture.names")
      nms <- nms[nzchar(nms)]
      start <- attr(m, "capture.start")
      end <- start + attr(m, "capture.length") - 1L
      n <- if (end[[1]] >= start[[1]]) {
             as.numeric(substring(unit, start[[1]], end[[1]]))
           } else {
             1
           }
      unit <- substring(unit, start[[2]], end[[2]])
      list(n = n, unit = unit)
    } else {
      stop(sprintf("Invalid unit specification '%s'", unit))
    }

  }
}


# Because `as.POSIXct.Date()` always uses local timezone
date2posixct <- function(x) {
  out <- unclass(x) * 86400
  attributes(out) <- list(tzone = "UTC", class = c("POSIXct", "POSIXt"))
  out
}
