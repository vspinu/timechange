

ctutc <- function(x) {
  as.POSIXct(x, tz = "UTC")
}
ctam <- function(x) {
  as.POSIXct(x, tz = "America/New_York")
}
cteu <- function(x) {
  as.POSIXct(x, tz = "Europe/Amsterdam")
}
ltutc <- function(x) {
  as.POSIXlt(x, tz = "UTC")
}
ltam <- function(x) {
  as.POSIXlt(x, tz = "America/New_York")
}
lteu <- function(x) {
  as.POSIXct(x, tz = "Europe/Amsterdam")
}
NAam <- .POSIXct(NA_real_, tz = "America/New_York")
NAem <- .POSIXct(NA_real_, tz = "Europe/Amsterdam")
NAutc <- .POSIXct(NA_real_, tz = "UTC")

if (packageVersion("testthat") >= "0.7.1.99") {
  library(testthat)
  test_check("timechange")
}
