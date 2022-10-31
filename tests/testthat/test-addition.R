context("Addition Operations")

test_that("Non-finite date-times are handled correctly", {
  expect_identical(unclass(time_add(.POSIXct(Inf), hours = 1)), Inf)
  expect_identical(unclass(time_add(.POSIXct(-Inf), hours = 1)), -Inf)
  expect_identical(unclass(time_add(.POSIXct(NA_real_), hours = 1)), NA_real_)

  expect_identical(unclass(time_add(.Date(Inf), days = 1)), Inf)
  expect_identical(unclass(time_add(.Date(-Inf), days = 1)), -Inf)
  expect_identical(unclass(time_add(.POSIXct(NA_real_), days = 1)), NA_real_)
})

test_that("addition handles daylight savings time", {
  x <- ctus("2010-03-14 01:02:03")
  y <- ctus("2010-03-15 01:02:03")
  expect_equal(time_add(x, days = 1), y)

  expect_equal(time_add(x, hours = 1, minutes = 50, roll_dst = "post"), ctus("2010-03-14 03:52:03"))
  expect_equal(time_add(x, hours = 1, minutes = 50, roll_dst = "pre"), ctus("2010-03-14 01:52:03"))
  expect_equal(time_add(x, hours = 1, minutes = 50, roll_dst = "boundary"), ctus("2010-03-14 03:00:00"))
  expect_equal(time_add(x, hours = 1, minutes = 50, roll_dst = "NA"), NAam)

  expect_equal(time_subtract(y, hours = 22, minutes = 50, roll_dst = "post"), ctus("2010-03-14 03:12:03"))
  expect_equal(time_subtract(y, hours = 22, minutes = 50, roll_dst = "pre"), ctus("2010-03-14 01:12:03"))
  expect_equal(time_subtract(y, hours = 22, minutes = 50, roll_dst = "boundary"), ctus("2010-03-14 03:00:00"))
  expect_equal(time_subtract(y, hours = 22, minutes = 50, roll_dst = "NA"), NAam)
  expect_equal(time_subtract(y, days = 1), x)
})

test_that("addition works as expected for instants", {

  x <- ctutc("2008-01-01 00:00:00")
  y <- ltutc("2008-01-01 00:00:00")
  z <- as.Date("2008-01-01")
  expect_equal(time_add(x, years = 1), ctutc("2009-01-01 00:00:00"))
  expect_equal(time_add(y, years = 1), ltutc("2009-01-01 00:00:00"))
  expect_equal(time_add(z, years = 1), as.Date("2009-01-01"))

  x <- ctus("2008-01-01 00:00:00")
  y <- ctus("2008-01-01 00:00:00")
  expect_equal(time_add(x, years = 1), ctus("2009-01-01 00:00:00"))
  expect_equal(time_add(y, years = 1), ctus("2009-01-01 00:00:00"))
  expect_equal(time_add(x, years = 1, months = 2, days = 3, hours = 4, minutes = 5, seconds = 6.6),
               ctus("2009-03-04 04:05:06.6"))
  expect_equal(time_add(y, years = 1), ctus("2009-01-01 00:00:00"))

  x <- ctus("2008-01-01 00:00:00")
  y <- ctus("2008-01-01 00:00:00")
  expect_equal(time_subtract(x, years = 1), ctus("2007-01-01 00:00:00"))
  expect_equal(time_subtract(y, years = 1), ctus("2007-01-01 00:00:00"))
  expect_equal(time_subtract(x, years = 1, months = 2, days = 3, hours = 4, minutes = 5, seconds = 6.6),
               ctus("2006-10-28 19:54:53.4"))

})

test_that("addition works with month gap", {

  x <- ctutc("2008-01-31 01:02:03")
  y <- ltutc("2008-01-31 01:02:03")
  z <- as.Date("2008-01-31")
  expect_equal(time_add(x, months = 1), ctutc("2008-02-29 01:02:03"))
  expect_equal(time_add(y, months = 1), ltutc("2008-02-29 01:02:03"))
  expect_equal(time_add(z, months = 1), as.Date("2008-02-29"))

  expect_equal(time_add(x, years = 2, months = 1, roll_month = "preday"), ctutc("2010-02-28 01:02:03"))
  expect_equal(time_add(y, years = 2, months = 1, roll_month = "preday"), ltutc("2010-02-28 01:02:03"))
  expect_equal(time_add(z, years = 2, months = 1, roll_month = "preday"), as.Date("2010-02-28"))

  expect_equal(time_add(x, years = 2, months = 1, roll_month = "postday"), ctutc("2010-03-01 01:02:03"))
  expect_equal(time_add(y, years = 2, months = 1, roll_month = "postday"), ltutc("2010-03-01 01:02:03"))
  expect_equal(time_add(z, years = 2, months = 1, roll_month = "postday"), as.Date("2010-03-01"))

  expect_equal(time_add(x, years = 2, months = 1, roll_month = "boundary"), ctutc("2010-03-01 00:00:00"))
  expect_equal(time_add(y, years = 2, months = 1, roll_month = "boundary"), ltutc("2010-03-01 00:00:00"))
  expect_equal(time_add(z, years = 2, months = 1, roll_month = "boundary"), as.Date("2010-03-01"))

  expect_equal(time_add(x, years = 2, months = 1, roll_month = "NA"), NAutc)
  expect_equal(time_add(y, years = 2, months = 1, roll_month = "NA"), as.POSIXlt(NAutc))
  expect_equal(time_add(z, years = 2, months = 1, roll_month = "NA"), as.Date(NA))

  x <- ctutc("2008-03-31 01:02:03")
  y <- ltutc("2008-03-31 01:02:03")
  z <- as.Date("2008-03-31")

  expect_equal(time_subtract(x, months = 1), ctutc("2008-02-29 01:02:03"))
  expect_equal(time_subtract(y, months = 1), ltutc("2008-02-29 01:02:03"))
  expect_equal(time_subtract(z, months = 1), as.Date("2008-02-29"))

  expect_equal(time_subtract(x, years = 2, months = 1, roll_month = "preday"), ctutc("2006-02-28 01:02:03"))
  expect_equal(time_subtract(y, years = 2, months = 1, roll_month = "preday"), ltutc("2006-02-28 01:02:03"))
  expect_equal(time_subtract(z, years = 2, months = 1, roll_month = "preday"), as.Date("2006-02-28"))

  expect_equal(time_subtract(x, years = 2, months = 1, roll_month = "postday"), ctutc("2006-03-01 01:02:03"))
  expect_equal(time_subtract(y, years = 2, months = 1, roll_month = "postday"), ltutc("2006-03-01 01:02:03"))
  expect_equal(time_subtract(z, years = 2, months = 1, roll_month = "postday"), as.Date("2006-03-01"))

  expect_equal(time_subtract(x, years = 2, months = 1, roll_month = "boundary"), ctutc("2006-03-01 00:00:00"))
  expect_equal(time_subtract(y, years = 2, months = 1, roll_month = "boundary"), ltutc("2006-03-01 00:00:00"))
  expect_equal(time_subtract(z, years = 2, months = 1, roll_month = "boundary"), as.Date("2006-03-01"))

  expect_equal(time_subtract(x, years = 2, months = 1, roll_month = "NA"), NAutc)
  expect_equal(time_subtract(y, years = 2, months = 1, roll_month = "NA"), as.POSIXlt(NAutc))
  expect_equal(time_subtract(z, years = 2, months = 1, roll_month = "NA"), as.Date(NA))

})

test_that("adding vectors works as expected for instants", {
  x <- ctutc(c("2008-01-01 00:00:00", "2009-01-01 00:00:00"))
  y <- ltutc(c("2008-01-01 00:00:00", "2009-01-01 00:00:00"))
  z <- c(as.Date("2008-01-01"), as.Date("2008-01-10"))

  expect_equal(time_add(x, years = 1), ctutc(c("2009-01-01 00:00:00", "2010-01-01 00:00:00")))
  expect_equal(time_add(y, years = 1), ltutc(c("2009-01-01 00:00:00", "2010-01-01 00:00:00")))
  expect_equal(time_add(z, years = 1), as.Date(c("2009-01-01", "2009-01-10")))

  expect_equal(time_subtract(x, years = 1), ctutc(c("2007-01-01 00:00:00", "2008-01-01 00:00:00")))
  expect_equal(time_subtract(y, years = 1), ltutc(c("2007-01-01 00:00:00", "2008-01-01 00:00:00")))
  expect_equal(time_subtract(z, years = 1), as.Date(c("2007-01-01", "2007-01-10")))

  x <- ctutc(c("2008-01-31 01:02:03", "2009-01-30 01:02:03"))
  y <- ltutc(c("2008-01-31 01:02:03", "2009-01-30 01:02:03"))
  z <- c(as.Date("2008-01-31"), as.Date("2008-01-30"))
  expect_equal(time_add(x, years = 1, month = 1, roll_month = "preday"),
               ctutc(c("2009-02-28 01:02:03", "2010-02-28 01:02:03")))
  expect_equal(time_add(y, years = 1, month = 1, roll_month = "preday"),
               ltutc(c("2009-02-28 01:02:03", "2010-02-28 01:02:03")))
  expect_equal(time_add(z, years = 1, month = 1, roll_month = "preday"),
               as.Date(c("2009-02-28", "2009-02-28")))

  expect_equal(time_add(x, years = 1, month = 1, roll_month = "postday"),
               ctutc(c("2009-03-01 01:02:03", "2010-03-01 01:02:03")))
  expect_equal(time_add(y, years = 1, month = 1, roll_month = "postday"),
               ltutc(c("2009-03-01 01:02:03", "2010-03-01 01:02:03")))
  expect_equal(time_add(z, years = 1, month = 1, roll_month = "postday"),
               as.Date(c("2009-03-01", "2009-03-01")))

  expect_equal(time_add(x, years = 1, month = 1, roll_month = "boundary"),
               ctutc(c("2009-03-01 00:00:00", "2010-03-01 00:00:00")))
  expect_equal(time_add(y, years = 1, month = 1, roll_month = "boundary"),
               ltutc(c("2009-03-01 00:00:00", "2010-03-01 00:00:00")))
  expect_equal(time_add(z, years = 1, month = 1, roll_month = "boundary"),
               as.Date(c("2009-03-01", "2009-03-01")))

  expect_equal(time_add(x, years = 1, month = 1, roll_month = "skip"),
               ctutc(c("2009-03-03 01:02:03", "2010-03-02 01:02:03")))
  expect_equal(time_add(y, years = 1, month = 1, roll_month = "skip"),
               ltutc(c("2009-03-03 01:02:03", "2010-03-02 01:02:03")))
  expect_equal(time_add(z, years = 1, month = 1, roll_month = "skip"),
               as.Date(c("2009-03-03", "2009-03-02")))

})


test_that("addition and subtraction work with repeated DST", {
  am1 <- .POSIXct(1414904400, tz = "America/New_York")
  am2 <- am1 + 3600*2

  expect_equal(time_subtract(am2, hours = 1, roll_dst = "post"), am1 + 3600)
  expect_equal(time_subtract(am2, hours = 1, roll_dst = "pre"), am1)
  expect_equal(time_subtract(am2, hours = 1, roll_dst = c("pre", "post")), am1 + 3600)
  expect_equal(time_subtract(am2, hours = 1, minutes = 1, roll_dst = "pre"), am1 - 60)
  expect_equal(time_subtract(am2 + 60, hours = 1, roll_dst = "post"), am1 + 3660)
  expect_equal(time_subtract(am2 + 60, hours = 1, minutes = 1, roll_dst = "post"), am1 + 3600)
  expect_equal(time_subtract(am2 + 60, hours = 1, minutes = 2, roll_dst = "pre"), am1 - 60)
  expect_equal(time_subtract(am2 + 60, hours = 1, minutes = 1, seconds = 1, roll_dst = "pre"), am1 - 1)

  expect_equal(time_add(am1, minutes = 2, roll_dst = "pre"), am1 + 120)
  expect_equal(time_add(am1, minutes = 60, roll_dst = "pre"), am2)
  expect_equal(time_add(am1 + 60, minutes = 1, roll_dst = "pre"), am1 + 120)
  expect_equal(time_add(am1 + 60, hours = 1, roll_dst = "pre"), am2 + 60)
  expect_equal(time_add(am1 + 60, minutes = 1, roll_dst = "pre"), am1 + 120)
  expect_equal(time_add(am1 + 60, minutes = 60, roll_dst = "pre"), am2 + 60)
  expect_equal(time_add(am1 + 60, minutes = 120, roll_dst = "pre"), am2 + 3660)

})

test_that("addition works on 'strange' DST gaps", {
  ## Midnight doesn't exist. DST spring forward happens at 2020-03-29 00:00:00
  ## and they spring forward to hour 1
  y <- as.POSIXct("2020-03-29 01:00:00", tz = "Asia/Beirut")
  x <- as.POSIXct("2020-03-28 00:00:00", tz = "Asia/Beirut")
  expect_equal(y, time_add(x, days = 1))
  expect_equal(time_add(y, minutes = 5), time_add(x, hours = 24, minutes = 5))
  expect_equal(time_add(y, minutes = 5), time_add(x, hours = 23, minutes = 65))
})

test_that("addition errors on empty unit vectors", {
  y <- ymd_hms("2020-03-29 01:00:00", tz = "Asia/Beirut")
  expect_error(time_add(y, minute = integer()), "Invalid size of 'minute' vector")
  expect_error(time_add(y, hour = 1, minute = integer()), "Invalid size of 'minute' vector")
})

test_that("Subtracting months to March 1 produces correct results", {
  ## https://github.com/tidyverse/lubridate/issues/1037
  time <- ymd("2022-04-01", tz = "America/New_York")
  expect_equal(time_add(time, months = -1), ymd("2022-03-01", tz = "America/New_York"))
  time <- ymd("2022-05-01", tz = "America/New_York")
  expect_equal(time_add(time, months = -2), ymd("2022-03-01", tz = "America/New_York"))
  time <- ymd_hms("2022-04-02 04:01:01", tz = "America/New_York")
  expect_equal(time_add(time, months = -1, days = -1, hours = -4, minutes = -1, seconds = -1),
               ymd("2022-03-01", tz = "America/New_York"))
})
