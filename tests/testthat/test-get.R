context("Time Get")


test_that("time_get handles different date-time types correctly", {

  date <- as.Date("1970-01-01")
  datetime_ct <- as.POSIXct("1970-01-01 01:02:03", tz = "America/New_York")
  datetime_lt <- as.POSIXlt(datetime_ct)

  expect_identical(time_get(date), data.frame(year = 1970L, month = 1L,
                                              yday = 1L, mday = 1L, wday = 3L,
                                              hour = 0L, minute = 0L, second = 0))

  expect_identical(time_get(datetime_ct, c("month", "year", "second", "hour")),
                   data.frame(month = 1L, year = 1970L, second = 3, hour = 1L))

  expect_identical(time_get(datetime_ct, c("month", "year", "second", "hour")),
                   time_get(datetime_lt, c("month", "year", "second", "hour")))

  ## duplicates allowed
  expect_identical(time_get(datetime_ct, c("month", "year", "month", "year")),
                   data.frame(month = 1L, year = 1970L, month = 1L, year = 1970L))


  ## day / mday return the same value but are different columns
  expect_identical(time_get(datetime_ct, c("day", "mday")),
                   data.frame(day = 1L, mday = 1L))
  expect_identical(time_get(datetime_ct, c("day", "mday")),
                   time_get(datetime_ct, c("day", "mday")))

})

test_that("tzone attributes of Dates is preserved", {
  d <- ymd("2020-01-01")
  tzone <- "America/New_York"
  attr(d, "tzone") <- tzone
  time_get(d, "month")
  expect_is(time_update(d, hour = 2), "POSIXct")
  expect_identical(time_update(d, month = 2), structure(ymd("2020-02-01"), tzone = tzone))
  expect_identical(time_update(d, hour = 1), ymd_hms("2020-01-01 01:00:00", tz = tzone))
})

## speed tests

## library(microbenchmark)

## x <- .POSIXct(runif(1e5, -17987443200, 32503680000)) # random times between 1400 and 3000

## microbenchmark(y = timechange::time_get(x, "year"),
##                w = timechange::time_get(x, "wday"),
##                s = timechange::time_get(x, "second"),
##                yhs = timechange::time_get(x, c("year", "hour", "second")),
##                yhs_C = timechange:::C_time_get(x, c("year", "hour", "second")),
##                ry = timerip::rip_year(x),
##                rw = timerip::rip_wday(x),
##                rs = timerip::rip_second(x),
##                rall = timerip::rip_info(x),
##                py = as.POSIXlt(x)$year + 1900L,
##                times = 10)
