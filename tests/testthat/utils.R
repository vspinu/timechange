
ctutc <- function(x) {
  as.POSIXct(x, tz = "UTC")
}
ctus <- function(x) {
  as.POSIXct(x, tz = "America/New_York")
}
cteu <- function(x) {
  as.POSIXct(x, tz = "Europe/Amsterdam")
}
ltutc <- function(x) {
  as.POSIXlt(x, tz = "UTC")
}
ltus <- function(x) {
  as.POSIXlt(x, tz = "America/New_York")
}
ltam <- ltus
lteu <- function(x) {
  as.POSIXct(x, tz = "Europe/Amsterdam")
}
NAam <- .POSIXct(NA_real_, tz = "America/New_York")
NAem <- .POSIXct(NA_real_, tz = "Europe/Amsterdam")
NAutc <- .POSIXct(NA_real_, tz = "UTC")

ymd_hms <- function(..., tz = "UTC") {
  x <- unlist(list(...))
  out <- .POSIXct(rep_len(NA_real_, length(x)), tz = tz)
  nna <- !is.na(x)
  out[nna] <- as.POSIXct(x[nna], tz = tz)
  out
}

origin <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")

ymd <- function(..., tz = NULL) {
  x <- unlist(list(...))
  if (is.null(tz)) {
    out <- as.Date(rep_len(NA_real_, length(x)), origin = origin)
    nna <- !is.na(x)
    out[nna] <- as.Date(x[nna], tz = "UTC")
  } else {
    out <- .POSIXct(rep_len(NA_real_, length(x)), tz = tz)
    nna <- !is.na(x)
    out[nna] <- as.POSIXct(paste(x[nna], "00:00:00", sep = " "), tz = tz)
  }
  out
}

second <- function(x)
  as.POSIXlt(x, tz = timechange:::tz(x))$sec

hour <- function(x)
  as.POSIXlt(x, tz = timechange:::tz(x))$hour

minute <- function(x)
  as.POSIXlt(x, tz = timechange:::tz(x))$min

month <- function(x, label = FALSE, abbr = TRUE, locale = Sys.getlocale("LC_TIME")) {
  x <- as.POSIXlt(x, tz = timechange:::tz(x))$mon + 1
  if (!label) return(x)
  names <- .get_locale_regs(locale)$month_names
  labels <- if (abbr) names$abr else names$full
  ordered(x, levels = 1:12, labels = labels)
}

year <- function(x)
  as.POSIXlt(x, tz = timechange:::tz(x))$year + 1900

mday <- function(x)
  as.POSIXlt(x, tz = timechange:::tz(x))$mday

day <- function(x)
  as.POSIXlt(x, tz = timechange:::tz(x))$mday

wday <- function(x, label = FALSE, abbr = TRUE,
                 week_start = 1, locale = Sys.getlocale("LC_TIME")) {
  x <- as.POSIXlt(x, tz = timechange:::tz(x))$wday + 1
  start <- as.integer(week_start)
  if (start > 7 || start < 1) stop("Invalid 'week_start' argument; must be between 1 and 7")
  if (start != 7) {
    x <- 1 + (x + (6 - start)) %% 7
  }
  if (!label) {
    return(x)
  }
  names <- .get_locale_regs(locale)$wday_names
  labels <- if (abbr) names$abr else names$full
  ordered(x, levels = 1:7, labels = .shift_wday_names(labels, week_start = start))
}

yday <- function(x)
  as.POSIXlt(x, tz = timechange:::tz(x))$yday + 1

now <- function(tzone = "")
  time_at_tz(Sys.time(), tzone)
