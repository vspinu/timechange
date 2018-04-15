context("Time Update")

## most of the tests are borrowed from lubridate

library(lubridate)
options(lubridate.week.start = 1)

test_that("update.Date returns a date object", {
  date <- as.Date("05/05/2010", "%m/%d/%Y")
  expect_that(time_update(date, day = 1), is_a("Date"))
  expect_that(time_update(date, yday = 1), is_a("Date"))
  expect_that(time_update(date, mday = 1), is_a("Date"))
  expect_that(time_update(date, wday = 1), is_a("Date"))
  expect_that(time_update(date, month = 1), is_a("Date"))
  expect_that(time_update(date, year = 2001), is_a("Date"))
})

test_that("update.Date returns a posix object if time is manipulated", {
  date <- as.Date("05/05/2010", "%m/%d/%Y")
  expect_that(time_update(date, second = 1), is_a("POSIXct"))
  expect_that(time_update(date, minute = 1), is_a("POSIXct"))
  expect_that(time_update(date, hour = 1), is_a("POSIXct"))
  expect_that(time_update(date, tz = "UTC"), is_a("POSIXct"))
  expect_equal(time_update(date, second = 1, minute = 1, hour = 1),
               as.POSIXct("2010-05-05 01:01:01", tz = "UTC"))
})

test_that("update.Date performs simple operation as expected", {
  date <- as.Date("05/05/2010", "%m/%d/%Y")
  expect_that(second(time_update(date, second = 1)), equals(1))
  expect_that(minute(time_update(date, minute = 1)), equals(1))
  expect_that(hour(time_update(date, hour = 1)), equals(1))
  expect_that(mday(time_update(date, mday = 1)), equals(1))
  expect_that(wday(time_update(date, mday = 1)), equals(6))
  expect_that(yday(time_update(date, mday = 1)), equals(121))
  expect_that(yday(time_update(date, yday = 1)), equals(1))
  expect_that(mday(time_update(date, yday = 1)), equals(1))
  expect_that(wday(time_update(date, yday = 1)), equals(5))
  expect_that(wday(time_update(date, wday = 1)), equals(1))
  expect_that(yday(time_update(date, wday = 1)), equals(123))
  expect_that(mday(time_update(date, wday = 1)), equals(3))
  expect_that(month(time_update(date, month = 1)), equals(1))
  expect_that(year(time_update(date, year = 2000)), equals(2000))
  expect_match(tz(time_update(date, tz = "UTC")), "UTC")
})

test_that("update.POSIXlt returns a POSIXlt object", {
  poslt <- as.POSIXlt("2010-02-03 13:45:59", tz = "GMT", format = "%Y-%m-%d %H:%M:%S")
  expect_that(time_update(poslt, second = 1), is_a("POSIXlt"))
  expect_that(time_update(poslt, minute = 1), is_a("POSIXlt"))
  expect_that(time_update(poslt, hour = 1), is_a("POSIXlt"))
  expect_that(time_update(poslt, day = 1), is_a("POSIXlt"))
  expect_that(time_update(poslt, yday = 1), is_a("POSIXlt"))
  expect_that(time_update(poslt, mday = 1), is_a("POSIXlt"))
  expect_that(time_update(poslt, wday = 1), is_a("POSIXlt"))
  expect_that(time_update(poslt, month = 1), is_a("POSIXlt"))
  expect_that(time_update(poslt, year = 2001), is_a("POSIXlt"))
  expect_that(time_update(poslt, tz = "UTC"), is_a("POSIXlt"))
})

test_that("update.POSIXct returns a POSIXct object", {
  posct <- as.POSIXct("2010-02-03 13:45:59",
                      tz = "GMT", format = "%Y-%m-%d %H:%M:%S")
  expect_that(time_update(posct, second = 1), is_a("POSIXct"))
  expect_that(time_update(posct, minute = 1), is_a("POSIXct"))
  expect_that(time_update(posct, hour = 1), is_a("POSIXct"))
  expect_that(time_update(posct, day = 1), is_a("POSIXct"))
  expect_that(time_update(posct, yday = 1), is_a("POSIXct"))
  expect_that(time_update(posct, mday = 1), is_a("POSIXct"))
  expect_that(time_update(posct, wday = 1), is_a("POSIXct"))
  expect_that(time_update(posct, month = 1), is_a("POSIXct"))
  expect_that(time_update(posct, year = 2001), is_a("POSIXct"))
  expect_that(time_update(posct, tz = "UTC"), is_a("POSIXct"))
})

test_that("update.POSIXt performs simple operation as expected", {
  poslt <- as.POSIXlt("2010-02-03 13:45:59", tz = "GMT", format = "%Y-%m-%d %H:%M:%S")
  posct <- as.POSIXct("2010-02-03 13:45:59", tz = "GMT", format = "%Y-%m-%d %H:%M:%S")
  expect_that(second(time_update(poslt, second = 1)), equals(1))
  expect_that(minute(time_update(poslt, minute = 1)), equals(1))
  expect_that(hour(time_update(poslt, hour = 1)), equals(1))
  expect_that(mday(time_update(poslt, mday = 1)), equals(1))
  expect_that(wday(time_update(poslt, mday = 1)), equals(1))
  expect_that(yday(time_update(poslt, mday = 1)), equals(32))
  expect_that(yday(time_update(poslt, yday = 1)), equals(1))
  expect_that(mday(time_update(poslt, yday = 1)), equals(1))
  expect_that(wday(time_update(poslt, yday = 1)), equals(5))
  expect_that(wday(time_update(poslt, wday = 1)), equals(1))
  expect_that(yday(time_update(poslt, wday = 1)), equals(32))
  expect_that(mday(time_update(poslt, wday = 1)), equals(1))
  expect_that(month(time_update(poslt, month = 1)), equals(1))
  expect_that(year(time_update(poslt, year = 2000)), equals(2000))
  expect_that(second(time_update(posct, second = 1)), equals(1))
  expect_that(minute(time_update(posct, minute = 1)), equals(1))
  expect_that(hour(time_update(posct, hour = 1)), equals(1))
  expect_that(mday(time_update(posct, mday = 1)), equals(1))
  expect_that(wday(time_update(posct, mday = 1)), equals(1))
  expect_that(yday(time_update(posct, mday = 1)), equals(32))
  expect_that(yday(time_update(posct, yday = 1)), equals(1))
  expect_that(mday(time_update(posct, yday = 1)), equals(1))
  expect_that(wday(time_update(posct, yday = 1)), equals(5))
  expect_that(wday(time_update(posct, wday = 1), week_start = 1), equals(1))
  expect_that(yday(time_update(posct, wday = 1, week_start = 7)), equals(31))
  expect_that(mday(time_update(posct, wday = 1, week_start = 7)), equals(31))
  expect_that(month(time_update(posct, month = 1)), equals(1))
  expect_that(year(time_update(posct, year = 2000)), equals(2000))
  expect_match(tz(time_update(poslt, tz = "UTC")), "UTC")
  expect_match(tz(time_update(posct, tz = "UTC")), "UTC")
})

test_that("update.POSIXt works on wdays", {
  date <- ymd("2017-05-07") ## sunday
  ct <- as.POSIXct("2010-02-03 13:45:59", tz = "America/New_York", format = "%Y-%m-%d %H:%M:%S") ## Wednesday
  expect_equal(wday(time_update(ct, wday = 1)), 1)
  expect_equal(wday(time_update(ct, wday = 2)), 2)
  expect_equal(wday(time_update(ct, wday = 5)), 5)
  expect_equal(wday(time_update(ct, wday = 7)), 7)
  expect_equal(wday(time_update(date, wday = 1)), 1)
  expect_equal(wday(time_update(date, wday = 2)), 2)
  expect_equal(wday(time_update(date, wday = 5)), 5)
  expect_equal(wday(time_update(date, wday = 7)), 7)

  ws <- 1
  expect_equal(wday(time_update(ct, wday = 1, week_start = ws)), 1)
  expect_equal(wday(time_update(ct, wday = 2, week_start = ws)), 2)
  expect_equal(wday(time_update(ct, wday = 5, week_start = ws)), 5)
  expect_equal(wday(time_update(ct, wday = 7, week_start = ws)), 7)
  expect_equal(wday(time_update(date, wday = 1, week_start = ws)), 1)
  expect_equal(wday(time_update(date, wday = 2, week_start = ws)), 2)
  expect_equal(wday(time_update(date, wday = 5, week_start = ws)), 5)
  expect_equal(wday(time_update(date, wday = 7, week_start = ws)), 7)

  ws <- 1
  expect_equal(wday(time_update(ct, wday = 1, week_start = ws), week_start = ws), 1)
  expect_equal(wday(time_update(ct, wday = 2, week_start = ws), week_start = ws), 2)
  expect_equal(wday(time_update(ct, wday = 5, week_start = ws), week_start = ws), 5)
  expect_equal(wday(time_update(ct, wday = 7, week_start = ws), week_start = ws), 7)
  expect_equal(wday(time_update(date, wday = 1, week_start = ws), week_start = ws), 1)
  expect_equal(wday(time_update(date, wday = 2, week_start = ws), week_start = ws), 2)
  expect_equal(wday(time_update(date, wday = 5, week_start = ws), week_start = ws), 5)
  expect_equal(wday(time_update(date, wday = 7, week_start = ws), week_start = ws), 7)

  ws <- 3
  expect_equal(wday(time_update(ct, wday = 1, week_start = ws), week_start = ws), 1)
  expect_equal(wday(time_update(ct, wday = 2, week_start = ws), week_start = ws), 2)
  expect_equal(wday(time_update(ct, wday = 5, week_start = ws), week_start = ws), 5)
  expect_equal(wday(time_update(ct, wday = 7, week_start = ws), week_start = ws), 7)
  expect_equal(wday(time_update(date, wday = 1, week_start = ws), week_start = ws), 1)
  expect_equal(wday(time_update(date, wday = 2, week_start = ws), week_start = ws), 2)
  expect_equal(wday(time_update(date, wday = 5, week_start = ws), week_start = ws), 5)
  expect_equal(wday(time_update(date, wday = 7, week_start = ws), week_start = ws), 7)

})

test_that("updates on ydas works correctly with leap year", {
  expect_equal(time_update(ymd("15-02-03", tz = "UTC"), year = 2000, yday = 1),
               ymd("2000-01-01", tz = "UTC"))
  expect_equal(time_update(ymd("15-02-03", tz = "UTC"), year = 2015, yday = 1),
               ymd("2015-01-01", tz = "UTC"))
  expect_equal(time_update(ymd("15-02-03", tz = "UTC"), year = 2016, yday = 10),
               ymd("2016-01-10", tz = "UTC"))
  expect_equal(time_update(ymd("15-02-03", tz = "America/New_York"), year = 2000, yday = 1),
               ymd("2000-01-01", tz = "America/New_York"))
  expect_equal(time_update(ymd("15-02-03", tz = "America/New_York"), year = 2015, yday = 1),
               ymd("2015-01-01", tz = "America/New_York"))
  expect_equal(time_update(ymd("15-02-03", tz = "America/New_York"), year = 2016, yday = 10),
               ymd("2016-01-10", tz = "America/New_York"))
  expect_equal(time_update(ymd(c("2016-02-29", "2016-03-01")), yday = 1),
               ymd(c("2016-01-01", "2016-01-01")))
  expect_equal(time_update(ymd(c("2016-02-29", "2016-03-01"), tz = "America/New_York"), yday = 1),
               ymd(c("2016-01-01", "2016-01-01"), tz = "America/New_York"))
  expect_equal(time_update(ymd_hms(c("2016-02-29 1:2:3", "2016-03-01 10:20:30")), yday = 1),
               ymd_hms(c("2016-01-01 1:2:3", "2016-01-01 10:20:30")))
  expect_equal(time_update(ymd_hms(c("2016-02-29 1:2:3", "2016-03-01 10:20:30"), tz = "America/New_York"), yday = 1),
               ymd_hms(c("2016-01-01 1:2:3", "2016-01-01 10:20:30"), tz = "America/New_York"))
})


test_that("update performs roll overs correctly for Date objects", {
  date <- as.Date("05/05/2010", "%m/%d/%Y")
  expect_that(second(time_update(date, second = 61)), equals(1))
  expect_that(minute(time_update(date, second = 61)), equals(1))
  expect_that(minute(time_update(date, minute = 61)), equals(1))
  expect_that(hour(time_update(date, minute = 61)), equals(1))
  expect_that(hour(time_update(date, hour = 25)), equals(1))
  expect_that(mday(time_update(date, hour = 25)), equals(6))
  expect_that(yday(time_update(date, hour = 25)), equals(126))
  expect_equal(wday(time_update(date, hour = 25)), (wday(date) + 1) %% 7)
  expect_that(mday(time_update(date, mday = 32)), equals(1))
  expect_that(month(time_update(date, mday = 32)), equals(6))
  expect_that(wday(time_update(date, wday = 31)), equals(3))
  expect_that(month(time_update(date, wday = 31)), equals(6))
  expect_that(yday(time_update(date, yday = 366)), equals(1))
  expect_that(month(time_update(date, yday = 366)), equals(1))
  expect_that(month(time_update(date, month = 13)), equals(1))
  expect_that(year(time_update(date, month = 13)), equals(2011))
  expect_match(tz(time_update(date, month = 13)), "UTC")
})

test_that("update performs roll overs correctly for POSIXlt objects", {
  poslt <- as.POSIXlt("2010-05-05 00:00:00", tz = "GMT", format = "%Y-%m-%d %H:%M:%S")
  expect_that(second(time_update(poslt, second = 61)), equals(1))
  expect_that(minute(time_update(poslt, second = 61)), equals(1))
  expect_that(minute(time_update(poslt, minute = 61)), equals(1))
  expect_that(hour(time_update(poslt, minute = 61)), equals(1))
  expect_that(hour(time_update(poslt, hour = 25)), equals(1))
  expect_that(mday(time_update(poslt, hour = 25)), equals(6))
  expect_that(yday(time_update(poslt, hour = 25)), equals(126))
  expect_that(wday(time_update(poslt, hour = 25)), equals(4))
  expect_that(mday(time_update(poslt, mday = 32)), equals(1))
  expect_that(month(time_update(poslt, mday = 32)), equals(6))
  expect_that(wday(time_update(poslt, wday = 31)), equals(3))
  expect_that(month(time_update(poslt, wday = 31)), equals(6))
  expect_that(yday(time_update(poslt, yday = 366)), equals(1))
  expect_that(month(time_update(poslt, yday = 366)), equals(1))
  expect_that(month(time_update(poslt, month = 13)), equals(1))
  expect_that(year(time_update(poslt, month = 13)), equals(2011))
  expect_match(tz(time_update(poslt, month = 13)), "GMT")
})

test_that("update performs roll overs correctly for POSIXct objects", {
  posct <- as.POSIXct("2010-05-05 00:00:00", tz = "GMT", format = "%Y-%m-%d %H:%M:%S")
  expect_that(second(time_update(posct, second = 61)), equals(1))
  expect_that(minute(time_update(posct, second = 61)), equals(1))
  expect_that(minute(time_update(posct, minute = 61)), equals(1))
  expect_that(hour(time_update(posct, minute = 61)), equals(1))
  expect_that(hour(time_update(posct, hour = 25)), equals(1))
  expect_that(mday(time_update(posct, hour = 25)), equals(6))
  expect_that(yday(time_update(posct, hour = 25)), equals(126))
  expect_that(wday(time_update(posct, hour = 25)), equals(4))
  expect_that(mday(time_update(posct, mday = 32)), equals(1))
  expect_that(month(time_update(posct, mday = 32)), equals(6))
  expect_that(wday(time_update(posct, wday = 31)), equals(3))
  expect_that(month(time_update(posct, wday = 31)), equals(6))
  expect_that(yday(time_update(posct, yday = 366)), equals(1))
  expect_that(month(time_update(posct, yday = 366)), equals(1))
  expect_that(month(time_update(posct, month = 13)), equals(1))
  expect_that(year(time_update(posct, month = 13)), equals(2011))
  expect_match(tz(time_update(posct, month = 13)), "GMT")
})

test_that("update performs consecutive roll overs correctly for
  Date objects regardless of order", {
    date <- time_update(as.Date("11/01/2010", "%m/%d/%Y"),
                        month = 13, day = 32, hour = 25, minute = 61, second = 61)
    expect_that(second(date), equals(1))
    expect_that(minute(date), equals(2))
    expect_that(hour(date), equals(2))
    expect_that(mday(date), equals(2))
    expect_that(wday(date), equals(3))
    expect_that(yday(date), equals(33))
    expect_that(month(date), equals(2))
    expect_that(year(date), equals(2011))
    expect_match(tz(date), "UTC")
    date2 <- time_update(as.Date("11/01/2010", "%m/%d/%Y"),
                         second = 61, minute = 61, hour = 25, day = 32, month = 13)
    expect_that(second(date2), equals(1))
    expect_that(minute(date2), equals(2))
    expect_that(hour(date2), equals(2))
    expect_that(mday(date2), equals(2))
    expect_that(wday(date2), equals(3))
    expect_that(yday(date2), equals(33))
    expect_that(month(date2), equals(2))
    expect_that(year(date2), equals(2011))
    expect_match(tz(date2), "UTC")
  })

test_that("update performs consecutive roll overs correctly for POSIXlt objects", {
  posl <- as.POSIXlt("2010-11-01 00:00:00",
                     tz = "GMT", format = "%Y-%m-%d %H:%M:%S")
  poslt <- time_update(posl, month = 13, day = 32, hour = 25,
                       minute = 61, second = 61)
  expect_that(second(poslt), equals(1))
  expect_that(minute(poslt), equals(2))
  expect_that(hour(poslt), equals(2))
  expect_that(mday(poslt), equals(2))
  expect_that(wday(poslt), equals(3))
  expect_that(yday(poslt), equals(33))
  expect_that(month(poslt), equals(2))
  expect_that(year(poslt), equals(2011))
  expect_match(tz(poslt), "GMT")
  poslt2 <- time_update(posl, second = 61, minute = 61, hour = 25,
                        day = 32, month = 13)
  expect_that(second(poslt2), equals(1))
  expect_that(minute(poslt2), equals(2))
  expect_that(hour(poslt2), equals(2))
  expect_that(mday(poslt2), equals(2))
  expect_that(wday(poslt2), equals(3))
  expect_that(yday(poslt2), equals(33))
  expect_that(month(poslt2), equals(2))
  expect_that(year(poslt2), equals(2011))
  expect_match(tz(poslt2), "GMT")
})

test_that("update performs consecutive roll overs correctly for POSIXct objects", {
  posc <- as.POSIXct("2010-11-01 00:00:00",
                     tz = "GMT", format = "%Y-%m-%d %H:%M:%S")
  posct <- time_update(posc, month = 13, day = 32, hour = 25,
                       minute = 61, second = 61)
  expect_that(second(posct), equals(1))
  expect_that(minute(posct), equals(2))
  expect_that(hour(posct), equals(2))
  expect_that(mday(posct), equals(2))
  expect_that(wday(posct), equals(3))
  expect_that(yday(posct), equals(33))
  expect_that(month(posct), equals(2))
  expect_that(year(posct), equals(2011))
  expect_match(tz(posct), "GMT")
  posct2 <- time_update(posc, second = 61, minute = 61, hour = 25,
                        day = 32, month = 13)
  expect_that(second(posct2), equals(1))
  expect_that(minute(posct2), equals(2))
  expect_that(hour(posct2), equals(2))
  expect_that(mday(posct2), equals(2))
  expect_that(wday(posct2), equals(3))
  expect_that(yday(posct2), equals(33))
  expect_that(month(posct2), equals(2))
  expect_that(year(posct2), equals(2011))
  expect_match(tz(posct2), "GMT")
})

test_that("update returns NA for date-times in the spring dst gap", {
  poslt <- as.POSIXlt("2010-03-14 01:59:59", tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  poslt <- force_tz(poslt, tz = "America/New_York")
  expect_that(is.na(time_update(poslt, second = 65)), is_true())
  expect_that(is.na(time_update(poslt, minute = 65)), is_true())
  expect_that(is.na(time_update(poslt, hour = 2)), is_true())
  poslt <- as.POSIXlt("2010-03-13 02:59:59", tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  poslt <- force_tz(poslt, tz = "America/New_York")
  expect_that(is.na(time_update(poslt, mday = 14)), is_true())
  expect_that(is.na(time_update(poslt, wday = 7)), is_true())
  expect_that(is.na(time_update(poslt, yday = 73)), is_true())
  poslt <- as.POSIXlt("2010-02-14 02:59:59", tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  poslt <- force_tz(poslt, tz = "America/New_York")
  expect_that(is.na(time_update(poslt, month = 3)), is_true())
  poslt <- as.POSIXlt("2009-03-14 02:59:59", tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  poslt <- force_tz(poslt, tz = "America/New_York")
  expect_that(is.na(time_update(poslt, year = 2010)), is_true())
  poslt <- as.POSIXlt("2010-03-14 02:59:59", tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  expect_that(is.na(time_update(poslt, tz = "America/New_York")), is_true())
})

test_that("update handles vectors of dates", {
  poslt <- as.POSIXlt(c("2010-02-14 01:59:59", "2010-02-15 01:59:59", "2010-02-16 01:59:59"),
                      tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  posct <- as.POSIXct(poslt)
  date <- as.Date(poslt)
  expect_that(second(time_update(poslt, second = 1)), equals(c(1, 1, 1)))
  expect_that(second(time_update(posct, second = 1)), equals(c(1, 1, 1)))
  expect_that(day(time_update(date, day = 1)), equals(c(1, 1, 1)))
})

test_that("update handles vectors of dates and conformable vector of inputs", {
  poslt <- as.POSIXlt(c("2010-02-14 01:59:59", "2010-02-15 01:59:59", "2010-02-16
    01:59:59"), tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  posct <- as.POSIXct(poslt)
  date <- as.Date(poslt)
  expect_that(second(time_update(poslt, second = c(1, 2, 3))), equals(c(1, 2, 3)))
  expect_that(second(time_update(posct, second = c(1, 2, 3))), equals(c(1, 2, 3)))
  expect_that(day(time_update(date, day = c(1, 2, 3))), equals(c(1, 2, 3)))
})

test_that("update handles single vector of inputs", {
  poslt <- as.POSIXlt("2010-03-14 01:59:59", tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  posct <- as.POSIXct(poslt)
  date <- as.Date(poslt)
  expect_that(second(time_update(poslt, second = c(1, 2, 3))), equals(c(1, 2, 3)))
  expect_that(second(time_update(posct, second = c(1, 2, 3))), equals(c(1, 2, 3)))
  expect_that(day(time_update(date, day = c(1, 2, 3))), equals(c(1, 2, 3)))
})

test_that("update handles conformable vectors of inputs", {
  poslt <- as.POSIXlt("2010-03-10 01:59:59", tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  posct <- as.POSIXct(poslt)
  date <- as.Date(poslt)
  expect_that(second(time_update(poslt, second = c(1, 2), minute = c(1, 2, 3, 4))), equals(c(1, 2, 1, 2)))
  expect_that(second(time_update(posct, second = c(1, 2), minute = c(1, 2, 3, 4))), equals(c(1, 2, 1, 2)))
  expect_that(day(time_update(date, day = c(1, 2), month = c(1, 2, 3, 4))), equals(c(1, 2, 1, 2)))
})

test_that("update.POSIXct returns input of length zero when given input of length zero", {
  x <- structure(vector(mode = "numeric"), class = c("POSIXct", "POSIXt"))
  expect_equal(time_update(x, day = 1), x)
})

test_that("update.POSIXlt returns input of length zero when given input of length zero", {
  x <- as.POSIXlt(structure(vector(mode = "numeric"), class = c("POSIXct", "POSIXt")))
  expect_equal(time_update(x, day = 1), x)
})

test_that("Updateing on second doesn't affect hour", {
  ## https://github.com/tidyverse/lubridate/issues/619
  tt <- Sys.time()
  tt2 <- tt
  second(tt2) <- 5
  expect_equal(hour(tt), hour(tt2))
})