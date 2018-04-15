
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



## utilities copied from lubridate


standardise_unit_name <- function(x) {
  dates <- c("second", "minute", "hour", "day", "week", "month", "year",
             ## these ones are used for rounding only
             "bimonth", "quarter", "halfyear", "season")
  y <- gsub("(.)s$", "\\1", x)
  y <- substr(y, 1, 3)
  res <- dates[pmatch(y, dates)]
  if (any(is.na(res))) {
    stop("Invalid period name: ", paste(x[is.na(res)], collapse = ", "),
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

    period_units <- c("second", "minute", "hour", "day", "week", "month", "year")

    wp <- which(p > 0)
    if (length(wp) > 1) {
      ## Fractional units are actually supported but only when it leads to one
      ## final unit.
      stop("Cannot't parse heterogenuous or fractional units larger than one minute.")
    }

    list(n = p[wp], unit = period_units[wp])

  } else {
    ## this part is for backward compatibility and allows for bimonth, halfyear
    ## and quarter

    m <- regexpr(" *(?<n>[0-9.,]+)? *(?<unit>[^ \t\n]+)", unit[[1]], perl = T)
    if (m > 0) {
      ## should always match
      nms <- attr(m, "capture.names")
      nms <- nms[nzchar(nms)]
      start <- attr(m, "capture.start")
      end <- start + attr(m, "capture.length") - 1L
      n <- if (end[[1]] >= start[[1]]) {
             as.integer(substring(unit, start[[1]], end[[1]]))
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