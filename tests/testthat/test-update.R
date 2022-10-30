context("Time Update")

test_that("Non-finite date-times are handled correctly", {
  expect_identical(unclass(time_update(.POSIXct(Inf), hour = 1)), Inf)
  expect_identical(unclass(time_add(.POSIXct(-Inf), hour = 1)), -Inf)
  expect_identical(unclass(time_add(.POSIXct(NA_real_), hour = 1)), NA_real_)

  expect_identical(unclass(time_add(.Date(Inf), day = 1)), Inf)
  expect_identical(unclass(time_add(.Date(-Inf), day = 1)), -Inf)
  expect_identical(unclass(time_add(.POSIXct(NA_real_), day = 1)), NA_real_)
})

## most of the tests are borrowed from lubridate

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
  expect_equal(second(time_update(date, second = 1)), 1)
  expect_equal(minute(time_update(date, minute = 1)), 1)
  expect_equal(hour(time_update(date, hour = 1)), 1)
  expect_equal(mday(time_update(date, mday = 1)), 1)
  expect_equal(wday(time_update(date, mday = 1)), 6)
  expect_equal(yday(time_update(date, mday = 1)), 121)
  expect_equal(yday(time_update(date, yday = 1)), 1)
  expect_equal(mday(time_update(date, yday = 1)), 1)
  expect_equal(wday(time_update(date, yday = 1)), 5)
  expect_equal(wday(time_update(date, wday = 1)), 1)
  expect_equal(yday(time_update(date, wday = 1)), 123)
  expect_equal(mday(time_update(date, wday = 1)), 3)
  expect_equal(month(time_update(date, month = 1)), 1)
  expect_equal(year(time_update(date, year = 2000)), 2000)
  expect_equal(timechange:::tz(time_update(date, tz = "UTC")), "UTC")
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
  expect_equal(second(time_update(poslt, second = 1)), 1)
  expect_equal(minute(time_update(poslt, minute = 1)), 1)
  expect_equal(hour(time_update(poslt, hour = 1)), 1)
  expect_equal(mday(time_update(poslt, mday = 1)), 1)
  expect_equal(wday(time_update(poslt, mday = 1)), 1)
  expect_equal(yday(time_update(poslt, mday = 1)), 32)
  expect_equal(yday(time_update(poslt, yday = 1)), 1)
  expect_equal(mday(time_update(poslt, yday = 1)), 1)
  expect_equal(wday(time_update(poslt, yday = 1)), 5)
  expect_equal(wday(time_update(poslt, wday = 1)), 1)
  expect_equal(yday(time_update(poslt, wday = 1)), 32)
  expect_equal(mday(time_update(poslt, wday = 1)), 1)
  expect_equal(month(time_update(poslt, month = 1)), 1)
  expect_equal(year(time_update(poslt, year = 2000)), 2000)
  expect_equal(second(time_update(posct, second = 1)), 1)
  expect_equal(minute(time_update(posct, minute = 1)), 1)
  expect_equal(hour(time_update(posct, hour = 1)), 1)
  expect_equal(mday(time_update(posct, mday = 1)), 1)
  expect_equal(wday(time_update(posct, mday = 1)), 1)
  expect_equal(yday(time_update(posct, mday = 1)), 32)
  expect_equal(yday(time_update(posct, yday = 1)), 1)
  expect_equal(mday(time_update(posct, yday = 1)), 1)
  expect_equal(wday(time_update(posct, yday = 1)), 5)
  expect_equal(wday(time_update(posct, wday = 1), week_start = 1), 1)
  expect_equal(yday(time_update(posct, wday = 1, week_start = 7)), 31)
  expect_equal(mday(time_update(posct, wday = 1, week_start = 7)), 31)
  expect_equal(month(time_update(posct, month = 1)), 1)
  expect_equal(year(time_update(posct, year = 2000)), 2000)
  expect_equal(timechange:::tz(time_update(poslt, tz = "UTC")), "UTC")
  expect_equal(timechange:::tz(time_update(posct, tz = "UTC")), "UTC")
})

test_that("update works with fractional second", {
  poslt <- ltutc("2010-02-03 13:45:59.234")
  posct <- ltutc("2010-02-03 13:45:59.323")
  expect_equal(time_update(poslt, year = 2002, minute = 3), ltutc("2002-02-03 13:03:59.234"))
  expect_equal(time_update(poslt, day = 1, minute = 1, hour = 3), ltutc("2010-02-01 03:01:59.234"))
  expect_equal(time_update(poslt, year = 2002, minute = 3, second = 0.5), ltutc("2002-02-03 13:03:00.5"))
  expect_equal(time_update(poslt, day = 1, minute = 1, hour = 3, second = 0.5), ltutc("2010-02-01 03:01:00.5"))
})

## test_that("update works with non-existent dates", {
##   ct <- as.POSIXct("2010-01-31 13:45:59.234", tz = "UTC")
##   time_update(ct, month = 2)
## })

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

test_that("Updates on ydays works correctly with leap year", {
  expect_equal(time_update(ymd("1915-02-03", tz = "UTC"), year = 2000, yday = 1),
               ymd("2000-01-01", tz = "UTC"))
  expect_equal(time_update(ymd("1915-02-03", tz = "UTC"), year = 2015, yday = 1),
               ymd("2015-01-01", tz = "UTC"))
  expect_equal(time_update(ymd("1915-02-03", tz = "UTC"), year = 2016, yday = 10),
               ymd("2016-01-10", tz = "UTC"))
  expect_equal(time_update(ymd("1915-02-03", tz = "America/New_York"), year = 2000, yday = 1),
               ymd("2000-01-01", tz = "America/New_York"))
  expect_equal(time_update(ymd("1915-02-03", tz = "America/New_York"), year = 2015, yday = 1),
               ymd("2015-01-01", tz = "America/New_York"))
  expect_equal(time_update(ymd("1915-02-03", tz = "America/New_York"), year = 2016, yday = 10),
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
  date <- ymd("2010-05-05")
  expect_equal(time_update(date, second = 61), ymd_hms("2010-05-05 00:01:01"))
  expect_equal(time_update(date, day = 29, second = 61), ymd_hms("2010-05-29 00:01:01"))

  date <- ymd("2001-02-20")
  expect_equal(time_update(date, day = 29, roll_month = "preday"), ymd("2001-02-28"))
  expect_equal(time_update(date, day = 29, roll_month = "postday"), ymd("2001-03-01"))
  expect_equal(time_update(date, day = 28, hour = 25, second = 2, roll_month = "postday"),
               ymd_hms("2001-03-01 01:00:02"))
  expect_equal(time_update(date, day = 28, hour = 25, second = 2, roll_month = "preday"),
               ymd_hms("2001-02-28 01:00:02"))
  expect_equal(time_update(date, day = 28, hour = 25, second = 2, roll_month = "boundary"),
               ymd_hms("2001-03-01 00:00:00"))


  expect_equal(time_update(date, hour = 25),  ymd_hms("2001-02-21 01:00:00"))
  expect_equal(time_update(date, hour = 240), ymd_hms("2001-02-28 00:00:00"))
  expect_equal(time_update(date, hour = 241, roll_month = "postday"), ymd_hms("2001-03-01 01:00:00"))
  expect_equal(time_update(date, hour = 241, roll_month = "boundary"), ymd_hms("2001-03-01 00:00:00"))
  expect_equal(time_update(date, hour = 241, roll_month = "NA"), NA_POSIXct_)
  expect_equal(time_update(date, hour = 241, exact = T), NA_POSIXct_)

  date <- ymd("2010-05-05")
  ## setting yday is exactly what we want so roll_month has no effect
  expect_equal(time_update(date, yday = 365, exact = T), ymd("2010-12-31"))
  expect_equal(time_update(date, yday = 370, exact = T), NA_Date_)
  expect_equal(time_update(date, yday = 370, roll_month = "postday"), ymd("2011-01-05"))
  expect_equal(time_update(date, yday = 370, roll_month = "preday"),  ymd("2011-01-05"))
  expect_equal(time_update(date, yday = 370, roll_month = "boundary"), ymd("2011-01-05"))

  expect_equal(time_update(date, month = 13, roll_month = "postday"), ymd("2011-01-05"))
  expect_equal(time_update(date, month = 13, roll_month = "preday"), ymd("2011-01-05"))
  expect_equal(time_update(date, month = 13, roll_month = "boundary"), ymd("2011-01-05"))

  expect_equal(timechange:::tz(time_update(date, month = 13)), "UTC")
})

test_that("update performs roll overs correctly for POSIXlt objects", {
  poslt <- ymd_hms("2010-05-05 00:00:00")
  expect_equal(second(time_update(poslt, second = 61)), 1)
  expect_equal(minute(time_update(poslt, second = 61)), 1)
  expect_equal(minute(time_update(poslt, minute = 61)), 1)
  expect_equal(hour(time_update(poslt, minute = 61)), 1)
  expect_equal(hour(time_update(poslt, hour = 25)), 1)
  expect_equal(mday(time_update(poslt, hour = 25)), 6)
  expect_equal(yday(time_update(poslt, hour = 25)), 126)
  expect_equal(wday(time_update(poslt, hour = 25)), 4)
  expect_equal(mday(time_update(poslt, mday = 32)), 31)
  expect_equal(mday(time_update(poslt, mday = 32, roll_month = "postday")), 1)
  expect_equal(month(time_update(poslt, mday = 32)), 5)
  expect_equal(wday(time_update(poslt, wday = 31)), 3)
  expect_equal(month(time_update(poslt, wday = 31)), 6)
  expect_equal(yday(time_update(poslt, yday = 366)), 1)
  expect_equal(month(time_update(poslt, yday = 366)), 1)
  expect_equal(month(time_update(poslt, month = 13)), 1)
  expect_equal(year(time_update(poslt, month = 13)), 2011)
  expect_equal(timechange:::tz(time_update(poslt, month = 13)), "UTC")
})

test_that("update performs roll overs correctly for POSIXct objects", {
  posct <- as.POSIXct("2010-05-05 00:00:00", tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  expect_equal(second(time_update(posct, second = 61)), 1)
  expect_equal(minute(time_update(posct, second = 61)), 1)
  expect_equal(minute(time_update(posct, minute = 61)), 1)
  expect_equal(hour(time_update(posct, minute = 61)), 1)
  expect_equal(hour(time_update(posct, hour = 25)), 1)
  expect_equal(mday(time_update(posct, hour = 25)), 6)
  expect_equal(yday(time_update(posct, hour = 25)), 126)
  expect_equal(wday(time_update(posct, hour = 25)), 4)
  expect_equal(mday(time_update(posct, mday = 32)), 31)
  expect_equal(time_update(posct, mday = 32, exact = T), NA_POSIXct_)
  expect_equal(month(time_update(posct, mday = 32)), 5)
  expect_equal(wday(time_update(posct, wday = 31)), 3)
  expect_equal(month(time_update(posct, wday = 31)), 6)
  expect_equal(yday(time_update(posct, yday = 366)), 1)
  expect_equal(month(time_update(posct, yday = 366)), 1)
  expect_equal(month(time_update(posct, month = 13)), 1)
  expect_equal(year(time_update(posct, month = 13)), 2011)
  expect_equal(timechange:::tz(time_update(posct, month = 13)), "UTC")
})

test_that("update performs consecutive roll overs correctly for
  Date objects regardless of order", {
    expect_equal(time_update(ymd("2010-01-11"),
                             month = 13, day = 32, hour = 25,
                             minute = 61, second = 61),
                 ymd_hms("2011-01-31 02:02:01"))
    expect_equal(time_update(ymd("2010-01-11"),
                             month = 13, day = 32, hour = 25,
                             minute = 61, second = 61,
                             roll_month = "boundary"),
                 ymd_hms("2011-02-01 00:00:00"))
    expect_equal(time_update(ymd("2010-01-11"),
                             month = 13, day = 32, hour = 25,
                             minute = 61, second = 61,
                             roll_month = "NA"),
                 NA_POSIXct_)
  })

test_that("update performs consecutive roll overs correctly for POSIXlt objects", {
  posl <- as.POSIXlt("2010-11-01 00:00:00",
                     tz = "GMT", format = "%Y-%m-%d %H:%M:%S")
  poslt <- time_update(posl, month = 13, day = 32, hour = 25,
                       minute = 61, second = 61,
                       roll_month = "skip")
  expect_equal(second(poslt), 1)
  expect_equal(minute(poslt), 2)
  expect_equal(hour(poslt), 2)
  expect_equal(mday(poslt), 2)
  expect_equal(wday(poslt), 3)
  expect_equal(yday(poslt), 33)
  expect_equal(month(poslt), 2)
  expect_equal(year(poslt), 2011)
  expect_equal(timechange:::tz(poslt), "GMT")
  poslt2 <- time_update(posl, second = 61, minute = 61, hour = 25,
                        day = 32, month = 13, roll_month = "skip")
  expect_equal(second(poslt2), 1)
  expect_equal(minute(poslt2), 2)
  expect_equal(hour(poslt2), 2)
  expect_equal(mday(poslt2), 2)
  expect_equal(wday(poslt2), 3)
  expect_equal(yday(poslt2), 33)
  expect_equal(month(poslt2), 2)
  expect_equal(year(poslt2), 2011)
  expect_equal(timechange:::tz(poslt2), "GMT")
})

test_that("update performs consecutive roll overs correctly for POSIXct objects", {
  posc <- as.POSIXct("2010-11-01 00:00:00",
                     tz = "GMT", format = "%Y-%m-%d %H:%M:%S")
  posct <- time_update(posc, month = 13, day = 32, hour = 25,
                       minute = 61, second = 61, roll_month = "skip")
  expect_equal(second(posct), 1)
  expect_equal(minute(posct), 2)
  expect_equal(hour(posct), 2)
  expect_equal(mday(posct), 2)
  expect_equal(wday(posct), 3)
  expect_equal(yday(posct), 33)
  expect_equal(month(posct), 2)
  expect_equal(year(posct), 2011)
  expect_equal(timechange:::tz(posct), "GMT")
  posct2 <- time_update(posc, second = 61, minute = 61, hour = 25,
                        day = 32, month = 13, roll_month = "skip")
  expect_equal(second(posct2), 1)
  expect_equal(minute(posct2), 2)
  expect_equal(hour(posct2), 2)
  expect_equal(mday(posct2), 2)
  expect_equal(wday(posct2), 3)
  expect_equal(yday(posct2), 33)
  expect_equal(month(posct2), 2)
  expect_equal(year(posct2), 2011)
  expect_equal(timechange:::tz(posct2), "GMT")
})

test_that("update returns NA for date-times in the spring dst gap", {
  poslt <- as.POSIXlt("2010-03-14 01:59:59", tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  poslt <- time_force_tz(poslt, tz = "America/New_York")
  expect_true(is.na(time_update(poslt, second = 65, roll_dst = "NA")))
  expect_true(is.na(time_update(poslt, minute = 65, roll_dst = "NA")))
  expect_true(is.na(time_update(poslt, hour = 2, roll_dst = "NA")))
  poslt <- as.POSIXlt("2010-03-13 02:59:59", tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  poslt <- time_force_tz(poslt, tz = "America/New_York")
  expect_true(is.na(time_update(poslt, mday = 14, roll_dst = "NA")))
  expect_true(is.na(time_update(poslt, wday = 7, roll_dst = "NA")))
  expect_true(is.na(time_update(poslt, yday = 73, roll_dst = "NA")))
  poslt <- as.POSIXlt("2010-02-14 02:59:59", tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  poslt <- time_force_tz(poslt, tz = "America/New_York")
  expect_true(is.na(time_update(poslt, month = 3, roll_dst = "NA")))
  poslt <- as.POSIXlt("2009-03-14 02:59:59", tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  poslt <- time_force_tz(poslt, tz = "America/New_York")
  expect_true(is.na(time_update(poslt, year = 2010, roll_dst = "NA")))
  poslt <- as.POSIXlt("2010-03-14 02:59:59", tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  expect_true(is.na(time_update(poslt, tz = "America/New_York", roll_dst = "NA")))
})

test_that("time_update roll_dst specs  work correctly", {
  ## DST repeat 2022-10-29 02:00:00 CEST -- 2022-10-29 03:00:00 CET"
  repref <- ymd_hms("2022-10-30 01:00:00", tz = "Europe/Amsterdam")
  rep <- ymd_hms("2022-10-29 00:00:00", tz = "Europe/Amsterdam")

  expect_equal(time_update(rep, day = 30, hour = 2, minute = 3, roll_dst = "pre"),
               repref + 3600 + 3*60)
  expect_equal(time_update(rep, day = 30, hour = 2, minute = 3, roll_dst = "boundary"),
               repref + 2*3600)
  expect_equal(time_update(rep, day = 30, hour = 2, minute = 3, roll_dst = "post"),
               repref + 2*3600 + 3*60)
  expect_equal(time_update(rep, day = 30, hour = 2, second = 3.35, roll_dst = "pre"),
               repref + 3600 + 3.35)
  expect_equal(time_update(rep, day = 30, hour = 2, second = 3.35, roll_dst = "boundary"),
               repref + 2*3600)
  expect_equal(time_update(rep, day = 30, hour = 2, second = 3.35, roll_dst = "post"),
               repref + 2*3600 + 3.35)

  ## DST gap    2022-03-27 01:00:00 CET  -- 2022-03-27 02:00:00 CEST
  gapref <- ymd_hms("2022-03-27 00:00:00", tz = "Europe/Amsterdam")
  gap <- ymd_hms("2022-03-26 00:00:00", tz = "Europe/Amsterdam")
  ### FIXME: finish these tests

})

test_that("update with roll_dst = 'boundary' works in dst gap", {
  poslt <- ltus("2010-03-14 01:59:59")
  boundary <- ltus("2010-03-14 03:00:00")
  expect_equal(time_update(poslt, second = 65, roll_dst = "boundary"), boundary)
  expect_equal(time_update(poslt, minute = 65, roll_dst = "boundary"), boundary)
  expect_equal(time_update(poslt, hour = 2, roll_dst = "boundary"), boundary)
  poslt <- ltus("2010-03-13 02:59:59")
  expect_equal(time_update(poslt, mday = 14, roll_dst = "boundary"), boundary)
  expect_equal(time_update(poslt, wday = 7, roll_dst = "boundary"), boundary)
  expect_equal(time_update(poslt, yday = 73, roll_dst = "boundary"), boundary)
  poslt <- ltus("2010-02-14 02:59:59")
  expect_equal(time_update(poslt, month = 3, roll_dst = "boundary"), boundary)
  poslt <- ltus("2009-03-14 02:59:59")
  expect_equal(time_update(poslt, year = 2010, roll_dst = "boundary"), boundary)
  poslt <- ltutc("2010-03-14 02:59:59")
  expect_equal(time_update(poslt, tz = "America/New_York", roll_dst = "boundary"), boundary)
})

test_that("update with roll_dst = 'next' works in dst gap", {
  poslt <- ltus("2010-03-14 01:59:59")
  boundary <- ltus("2010-03-14 03:00:00")
  expect_equal(time_update(poslt, second = 65, roll_dst = "post"), boundary + 5)
  expect_equal(time_update(poslt, minute = 65, roll_dst = "post"), boundary + 5 * 60 + 59)
  expect_equal(time_update(poslt, hour = 2, roll_dst = "post"), boundary +  59*60 + 59)
  poslt <- ltus("2010-03-13 02:59:59")
  expect_equal(time_update(poslt, mday = 14, roll_dst = "post"), boundary + 59*60 + 59)
  expect_equal(time_update(poslt, wday = 7, roll_dst = "post"), boundary + 59*60 + 59)
  expect_equal(time_update(poslt, yday = 73, roll_dst = "post"), boundary + 59*60 + 59)
  poslt <- ltus("2010-02-14 02:59:59")
  expect_equal(time_update(poslt, month = 3, roll_dst = "post"), boundary + 59*60 + 59)
  poslt <- ltus("2009-03-14 02:59:59")
  expect_equal(time_update(poslt, year = 2010, roll_dst = "post"), boundary + 59*60 + 59)
  poslt <- ltutc("2010-03-14 02:59:59")
  expect_equal(time_update(poslt, tz = "America/New_York", roll_dst = "post"), boundary + 59*60 + 59)
})

test_that("update with roll_dst = 'prev' works in dst gap", {
  poslt <- ltus("2010-03-14 01:59:59")
  boundary <- ltus("2010-03-14 01:00:00")
  expect_equal(time_update(poslt, second = 65, roll_dst = "pre"), boundary + 5)
  expect_equal(time_update(poslt, minute = 65, roll_dst = "pre"), boundary + 5 * 60 + 59)
  expect_equal(time_update(poslt, hour = 2, roll_dst = "pre"), boundary +  59*60 + 59)
  poslt <- ltus("2010-03-13 02:59:59")
  expect_equal(time_update(poslt, mday = 14, roll_dst = "pre"), boundary + 59*60 + 59)
  expect_equal(time_update(poslt, wday = 7, roll_dst = "pre"), boundary + 59*60 + 59)
  expect_equal(time_update(poslt, yday = 73, roll_dst = "pre"), boundary + 59*60 + 59)
  poslt <- ltus("2010-02-14 02:59:59")
  expect_equal(time_update(poslt, month = 3, roll_dst = "pre"), boundary + 59*60 + 59)
  poslt <- ltus("2009-03-14 02:59:59")
  expect_equal(time_update(poslt, year = 2010, roll_dst = "pre"), boundary + 59*60 + 59)
  poslt <- ltutc("2010-03-14 02:59:59")
  expect_equal(time_update(poslt, tz = "America/New_York", roll_dst = "pre"), boundary + 59*60 + 59)
})

test_that("update handles vectors of dates", {
  poslt <- as.POSIXlt(c("2010-02-14 01:59:59", "2010-02-15 01:59:59", "2010-02-16 01:59:59"),
                      tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  posct <- as.POSIXct(poslt)
  date <- as.Date(poslt)
  expect_equal(second(time_update(poslt, second = 1)), c(1, 1, 1))
  expect_equal(second(time_update(posct, second = 1)), c(1, 1, 1))
  expect_equal(day(time_update(date, day = 1)), c(1, 1, 1))
})

test_that("update handles vectors of dates and conformable vector of inputs", {
  poslt <- as.POSIXlt(c("2010-02-14 01:59:59", "2010-02-15 01:59:59", "2010-02-16
    01:59:59"), tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  posct <- as.POSIXct(poslt)
  date <- as.Date(poslt)
  expect_equal(second(time_update(poslt, second = c(1, 2, 3))), c(1, 2, 3))
  expect_equal(second(time_update(posct, second = c(1, 2, 3))), c(1, 2, 3))
  expect_equal(day(time_update(date, day = c(1, 2, 3))), c(1, 2, 3))
})

test_that("update handles single vector of inputs", {
  poslt <- as.POSIXlt("2010-03-14 01:59:59", tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  posct <- as.POSIXct(poslt)
  date <- as.Date(poslt)
  expect_equal(second(time_update(poslt, second = c(1, 2, 3))), c(1, 2, 3))
  expect_equal(second(time_update(posct, second = c(1, 2, 3))), c(1, 2, 3))
  expect_equal(day(time_update(date, day = c(1, 2, 3))), c(1, 2, 3))
})

test_that("update handles conformable vectors of inputs", {
  poslt <- as.POSIXlt("2010-03-10 01:59:59", tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  posct <- as.POSIXct(poslt)
  date <- as.Date(poslt)
  expect_equal(second(time_update(poslt, second = c(1, 2), minute = c(1, 2, 3, 4))), c(1, 2, 1, 2))
  expect_equal(second(time_update(posct, second = c(1, 2), minute = c(1, 2, 3, 4))), c(1, 2, 1, 2))
  expect_equal(day(time_update(date, day = c(1, 2), month = c(1, 2, 3, 4))), c(1, 2, 1, 2))
})

test_that("update.POSIXct returns input of length zero when given input of length zero", {
  x <- structure(vector(mode = "numeric"), class = c("POSIXct", "POSIXt"))
  expect_equal(time_update(x, day = 1), x)
})

test_that("update.POSIXlt returns input of length zero when given input of length zero", {
  x <- as.POSIXlt(structure(vector(mode = "numeric"), class = c("POSIXct", "POSIXt")))
  expect_equal(time_update(x, day = 1), x)
})

test_that("update correctly works for yday", {
  x <- ymd("2020-01-01")
  expect_equal(time_update(x, yday = 45), ymd("2020-02-14"))
  ## FIXME: with NA rolling check the boundary
  ## expect_equal(time_update(x, yday = 1000, roll_month = "NA"), NA_Date_)
  ## expect_equal(time_update(x, wday = 10, roll_month = "NA"), NA_Date_)

  x <- ymd_hms("2020-01-01 01:02:03")
  expect_equal(time_update(x, yday = 45), ymd_hms("2020-02-14 01:02:03"))
  ## FIXME: with NA rolling check the boundary
  ## expect_equal(time_update(x, yday = 1000, roll_month = "NA"), NA_POSIXct_)
  ## expect_equal(time_update(x, wday = 10, roll_month = "NA"), NA_POSIXct_)
})


test_that("update errors on empty unit vectors", {
  y <- ymd_hms("2020-03-29 01:00:00", tz = "Asia/Beirut")
  expect_error(time_update(y, minute = integer()), "Invalid size of 'minute' vector")
  expect_error(time_update(y, hour = 1, minute = integer()), "Invalid size of 'minute' vector")
})


test_that("updating Dates with 0 hms units produces POSIXct.", {
  expect_equal(time_update(ymd("2020-03-03"), hour = 0, second = 0), ymd("2020-03-03", tz = "UTC"))
  expect_equal(time_update(ymd("2020-03-03"), hour = NULL), ymd("2020-03-03"))
})
