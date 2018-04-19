context("Rounding")

test_that("time_floor works for each time element", {
  x <- as.POSIXct("2009-08-03 12:01:59.34", tz = "UTC")
  expect_identical(time_floor(x, "second"), as.POSIXct("2009-08-03 12:01:59", tz = "UTC"))
  expect_identical(time_floor(x, "minute"), as.POSIXct("2009-08-03 12:01:00", tz = "UTC"))
  expect_identical(time_floor(x, "hour"), as.POSIXct("2009-08-03 12:00:00", tz = "UTC"))
  expect_identical(time_floor(x, "day"), as.POSIXct("2009-08-03 00:00:00", tz = "UTC"))
  expect_identical(time_floor(x, "week"), as.POSIXct("2009-08-03 00:00:00", tz = "UTC"))
  expect_identical(time_floor(x, "month"), as.POSIXct("2009-08-01 00:00:00", tz = "UTC"))
  expect_identical(time_floor(x, "bimonth"), as.POSIXct("2009-07-01 00:00:00", tz = "UTC"))
  expect_identical(time_floor(x, "quarter"), as.POSIXct("2009-07-01 00:00:00", tz = "UTC"))
  expect_identical(time_floor(x, "halfyear"), as.POSIXct("2009-07-01 00:00:00", tz = "UTC"))
  expect_identical(time_floor(x, "year"), as.POSIXct("2009-01-01 00:00:00", tz = "UTC"))
})

test_that("time_floor and time_round throw on invalid multi-unit spec", {
  x <- ymd_hms("2009-08-03 12:01:57.11")
  expect_silent(time_floor(x, "120 asec"))
  expect_error(time_ceiling(x, "120 sec"))
  expect_error(time_floor(x, "120 min"))
  expect_error(time_floor(x, "25 h"))
  expect_error(time_floor(x, "32 days"))
  expect_error(time_ceiling(x, "120 min"))
  expect_error(time_ceiling(x, "25 h"))
  expect_error(time_ceiling(x, "32 days"))
})

test_that("time_floor works for multi-units", {
  x <- cteu("2009-08-03 12:01:59.23")
  expect_identical(time_floor(x, "2 secs"),   cteu("2009-08-03 12:01:58"))
  expect_identical(time_floor(x, "2s"),   cteu("2009-08-03 12:01:58"))
  expect_identical(time_floor(x, "2 mins"),   cteu("2009-08-03 12:00:00"))
  expect_identical(time_floor(x, "2M"),   cteu("2009-08-03 12:00:00"))
  expect_identical(time_floor(x, "2 hours"),     cteu("2009-08-03 12:00:00"))
  expect_identical(time_floor(x, "2h"),     cteu("2009-08-03 12:00:00"))
  expect_identical(time_floor(x, "2 days"),      cteu("2009-08-03 00:00:00"))
  expect_identical(time_floor(x, "3 days"),      cteu("2009-08-01 00:00:00"))
  expect_identical(time_floor(x, "10 days"),      cteu("2009-08-01 00:00:00"))
  expect_identical(time_floor(x, "10d"),      cteu("2009-08-01 00:00:00"))
  expect_identical(time_floor(x, "2 month"),    cteu("2009-07-01 00:00:00"))
  expect_identical(time_floor(x, "2m"),    cteu("2009-07-01 00:00:00"))
  expect_identical(time_floor(x, "2 bimonth"),  time_floor(x, "4 months"))
  expect_identical(time_floor(x, "2 quarter"),  time_floor(x, "6 months"))
  expect_identical(time_floor(x, "2 halfyear"), time_floor(x, "year"))
  expect_identical(time_floor(x, "2 year"),     cteu("2008-01-01 00:00:00"))
})

test_that("time_floor works for fractional multi-units", {
  x <- as.POSIXct("2009-08-03 12:01:59.23001", tz = "UTC")
  expect_error(time_floor(x, ".2 secs"))
  expect_equal(time_floor(x, ".5 mins"),   as.POSIXct("2009-08-03 12:01:30", tz = "UTC"))
  expect_identical(time_floor(x, ".2asec"),   as.POSIXct("2009-08-03 12:01:59.2", tz = "UTC"))
  expect_identical(time_floor(x, ".1asec"),   as.POSIXct("2009-08-03 12:01:59.2", tz = "UTC"))
  expect_equal(time_floor(x, ".05as"),   as.POSIXct("2009-08-03 12:01:59.2", tz = "UTC"))
  expect_equal(time_floor(x, ".01as"),   as.POSIXct("2009-08-03 12:01:59.23", tz = "UTC"))
  expect_equal(time_floor(x, ".005as"),   as.POSIXct("2009-08-03 12:01:59.23", tz = "UTC"))
})

test_that("multi-unit rounding works the same for POSIX and Date objects", {
  px <- ymd("2009-08-01", tz = "UTC")
  dt <- ymd("2009-08-01")
  expect_identical(time_floor(px, "5 mins"), time_floor(dt, "5 mins"))
  expect_identical(time_floor(px, "5 mins"), time_floor(dt, "5 mins"))
  expect_identical(time_ceiling(px + 0.0001, "5 mins"), time_ceiling(dt, "5 mins"))
  expect_identical(time_ceiling(px, "5 mins", change_on_boundary = T),
                   time_ceiling(dt, "5 mins", change_on_boundary = T))
  expect_identical(time_ceiling(px + 0.001, "5 hours"), time_ceiling(dt, "5 hours"))
  expect_identical(time_ceiling(px + 0.0001, "5 hours", change_on_boundary = T), time_ceiling(dt, "5 hours", change_on_boundary = T))
  expect_identical(time_ceiling(px + 0.001, "2 hours"), time_ceiling(dt, "2 hours"))
  expect_identical(as.Date(time_floor(px, "2 days")), time_floor(dt, "2 days"))
  expect_identical(as.Date(time_ceiling(px + 0.001, "2 days")), time_ceiling(dt, "2 days"))
  expect_identical(as.Date(time_floor(px, "5 days")), time_floor(dt, "5 days"))
  expect_identical(as.Date(time_ceiling(px + 0.001, "5 days")), time_ceiling(dt, "5 days"))
  expect_identical(as.Date(time_floor(px, "2 months")), time_floor(dt, "2 months"))
  expect_identical(as.Date(time_ceiling(px, "2 months")), time_ceiling(dt, "2 months"))
  expect_identical(as.Date(time_floor(px, "5 months")), time_floor(dt, "5 months"))
  expect_identical(as.Date(time_ceiling(px, "5 months")), time_ceiling(dt, "5 months"))
})


test_that("time_ceiling works for multi-units", {
  x <- as.POSIXct("2009-08-03 12:01:59.23", tz = "UTC")
  y <- as.POSIXct("2009-08-03 12:01:30.23", tz = "UTC")
  z <- as.POSIXct("2009-08-24 12:01:30.23", tz = "UTC")
  d <- as.Date("2009-01-01")
  p1 <- as.POSIXct("2009-01-01 00:00:00", tz = "UTC")
  d5 <- as.Date("2009-05-01")
  p5 <- as.POSIXct("2009-05-01 00:00:00", tz = "Europe/Helsinki")
  expect_identical(time_ceiling(x, "2 secs"),   as.POSIXct("2009-08-03 12:02:00", tz = "UTC"))
  expect_identical(time_ceiling(x, "3 secs"),   as.POSIXct("2009-08-03 12:02:00", tz = "UTC"))
  expect_identical(time_ceiling(x, "5 secs"),   as.POSIXct("2009-08-03 12:02:00", tz = "UTC"))
  expect_identical(time_ceiling(y, "2 secs"),   as.POSIXct("2009-08-03 12:01:32", tz = "UTC"))
  expect_identical(time_ceiling(y, "3 secs"),   as.POSIXct("2009-08-03 12:01:33", tz = "UTC"))
  expect_identical(time_ceiling(y, "5 secs"),   as.POSIXct("2009-08-03 12:01:35", tz = "UTC"))
  expect_identical(time_ceiling(x, "2 mins"),   as.POSIXct("2009-08-03 12:02:00", tz = "UTC"))
  expect_identical(time_ceiling(x, "3 mins"),   as.POSIXct("2009-08-03 12:03:00", tz = "UTC"))
  expect_identical(time_ceiling(x, "5 mins"),   as.POSIXct("2009-08-03 12:05:00", tz = "UTC"))
  expect_identical(time_ceiling(x, "2 hours"),  as.POSIXct("2009-08-03 14:00:00", tz = "UTC"))
  expect_identical(time_ceiling(x, "3 hours"),  as.POSIXct("2009-08-03 15:00:00", tz = "UTC"))
  expect_identical(time_ceiling(x, "2 days"),   as.POSIXct("2009-08-05 00:00:00", tz = "UTC"))
  expect_identical(time_ceiling(x, "3 days"),   as.POSIXct("2009-08-04 00:00:00", tz = "UTC"))
  expect_identical(time_ceiling(x, "10 days"),  as.POSIXct("2009-08-11 00:00:00", tz = "UTC"))
  expect_identical(time_ceiling(z, "5 days"),   as.POSIXct("2009-08-26 00:00:00", tz = "UTC"))
  expect_identical(time_ceiling(z, "10 days"),  as.POSIXct("2009-08-31 00:00:00", tz = "UTC"))
  expect_identical(time_ceiling(x, "2 month"),  as.POSIXct("2009-09-01 00:00:00", tz = "UTC"))
  expect_identical(time_ceiling(x, "2 bimonth"),  time_ceiling(x, "4 months"))
  expect_identical(time_ceiling(x, "2 quarter"),  time_ceiling(x, "6 months"))
  expect_identical(time_ceiling(x, "2 halfyear"), time_ceiling(x, "year"))
  expect_identical(time_ceiling(x, "2 year"),     as.POSIXct("2010-01-01 00:00:00", tz = "UTC"))
  expect_identical(time_ceiling(d, "days", change_on_boundary = F), as.Date("2009-01-01"))
  expect_identical(time_ceiling(d, "days", change_on_boundary = T), as.Date("2009-01-02"))
  expect_identical(time_ceiling(d, "2 days", change_on_boundary = F), as.Date("2009-01-01"))
  expect_identical(time_ceiling(d, "2 days", change_on_boundary = T), as.Date("2009-01-03"))
  expect_identical(time_ceiling(d5, "days", change_on_boundary = F), as.Date("2009-05-01"))
  expect_identical(time_ceiling(d5, "days", change_on_boundary = T), as.Date("2009-05-02"))
  expect_identical(time_ceiling(d5, "2 days", change_on_boundary = F), as.Date("2009-05-01"))
  expect_identical(time_ceiling(d5, "2 days", change_on_boundary = T), as.Date("2009-05-03"))
  expect_identical(time_ceiling(p1, "2 days", change_on_boundary = T), as.POSIXct("2009-01-03 00:00:00", tz = "UTC"))
  expect_identical(time_ceiling(p1, "2 days", change_on_boundary = F), as.POSIXct("2009-01-01 00:00:00", tz = "UTC"))
  expect_identical(time_ceiling(p5, "2 days", change_on_boundary = T), as.POSIXct("2009-05-03 00:00:00", tz = "Europe/Helsinki"))
  expect_identical(time_ceiling(p5, "2 days", change_on_boundary = F), as.POSIXct("2009-05-01 00:00:00", tz = "Europe/Helsinki"))
})

test_that("time_ceiling works for fractional multi-units", {
  x <- as.POSIXct("2009-08-03 12:01:59.23", tz = "UTC")
  expect_identical(time_ceiling(x, ".2 asecs"),   as.POSIXct("2009-08-03 12:01:59.4", tz = "UTC"))
  expect_identical(time_ceiling(x, ".1a"),   as.POSIXct("2009-08-03 12:01:59.3", tz = "UTC"))
  expect_equal(time_ceiling(x, ".05as"),   as.POSIXct("2009-08-03 12:01:59.25", tz = "UTC"))
  expect_equal(time_ceiling(x, ".5 mins"),   as.POSIXct("2009-08-03 12:02:00", tz = "UTC"))
})

test_that("time_round works for each time element", {
  x <- ctus("2009-08-03 12:01:59.23")
  expect_equal(time_round(x, "second"), ctus("2009-08-03 12:01:59"))
  expect_equal(time_round(x, "minute"), ctus("2009-08-03 12:02:00"))
  expect_equal(time_round(x, "hour"), ctus("2009-08-03 12:00:00"))
  expect_equal(time_round(x, "day"), ctus("2009-08-04 00:00:00"))
  expect_equal(time_round(x, "week"), ctus("2009-08-03 00:00:00"))
  expect_equal(time_round(x, "month"), ctus("2009-08-01 00:00:00"))
  expect_equal(time_round(x, "bimonth"), ctus("2009-09-01 00:00:00"))
  expect_equal(time_round(x, "quarter"), ctus("2009-07-01 00:00:00"))
  expect_equal(time_round(x, "halfyear"), ctus("2009-07-01 00:00:00"))
  expect_equal(time_round(x, "year"), ctus("2010-01-01 00:00:00"))
})

test_that("time_floor and time_ceiling work with leap years", {
  expect_equal(time_floor(ymd_hms(c("2016-02-29 1:2:3", "2016-03-01 10:20:30")), "year"),
               ymd_hms(c("2016-01-01 0:0:0", "2016-01-01 0:0:0")))
  expect_equal(time_floor(ymd_hms(c("2016-02-29 1:2:3", "2016-03-01 10:20:30"), tz = "America/New_York"), "year"),
               ymd_hms(c("2016-01-01 0:0:0", "2016-01-01 0:0:0"), tz = "America/New_York"))
  expect_equal(time_ceiling(ymd_hms(c("2016-02-29 1:2:3", "2016-03-01 10:20:30")), "year"),
               ymd_hms(c("2017-01-01 0:0:0", "2017-01-01 0:0:0")))
  expect_equal(time_ceiling(ymd_hms(c("2016-02-29 1:2:3", "2016-03-01 10:20:30"), tz = "America/New_York"), "year"),
               ymd_hms(c("2017-01-01 0:0:0", "2017-01-01 0:0:0"), tz = "America/New_York"))
})

test_that("time_round works for multi-units", {
  x <- ctutc("2009-08-03 12:01:59.23")
  expect_equal(time_round(x, "2 second"), ctutc("2009-08-03 12:02:00"))
  expect_equal(time_round(x, "2 minute"), ctutc("2009-08-03 12:02:00"))
  expect_equal(time_round(x, "3 mins"), ctutc("2009-08-03 12:03:00"))
  expect_equal(time_round(x, "5 mins"), ctutc("2009-08-03 12:00:00"))
  expect_equal(time_round(x, "2 hour"), ctutc("2009-08-03 12:00:00"))
  expect_equal(time_round(x, "5 hour"), ctutc("2009-08-03 10:00:00"))
  expect_equal(time_round(x, "2 days"), ctutc("2009-08-03 00:00:00"))
  expect_equal(time_round(x, "2 months"), ctutc("2009-09-01 00:00:00"))
  expect_equal(time_round(x, "bimonth"), time_round(x, "2 months"))
  expect_equal(time_round(x, "bimonth"), time_round(x, "4 months"))
  expect_equal(time_round(x, "quarter"), time_round(x, "3 months"))
  expect_equal(time_round(x, "halfyear"), time_round(x, "6 months"))
  expect_equal(time_round(x, "3 years"), ctutc("2010-01-01 00:00:00"))
  expect_equal(time_round(x, "4 years"), ctutc("2008-01-01 00:00:00"))
})

test_that("time_round works for fractional multi-units", {
  x <- ctus("2009-08-03 12:01:59.23")
  expect_equal(time_round(x, ".2 asecs"),   ctus("2009-08-03 12:01:59.2"))
  expect_equal(time_round(x, ".1as"),   ctus("2009-08-03 12:01:59.2"))
  expect_equal(time_round(x, ".05as"),   ctus("2009-08-03 12:01:59.25"))
  expect_equal(time_round(x, ".5 mins"),   ctus("2009-08-03 12:02:00"))
})

test_that("time_floor handles vectors", {
  x <- as.POSIXct(c("2009-08-03 12:01:59.23", "2010-08-03 12:01:59.23"), tz = "UTC")
  expect_identical(time_floor(x, "second"),
                   as.POSIXct(c("2009-08-03 12:01:59", "2010-08-03 12:01:59"), tz = "UTC"))
  expect_identical(time_floor(x, "minute"),
                   as.POSIXct(c("2009-08-03 12:01:00", "2010-08-03 12:01:00"), tz = "UTC"))
  expect_identical(time_floor(x, "hour"),
                   as.POSIXct(c("2009-08-03 12:00:00", "2010-08-03 12:00:00"), tz = "UTC"))
  expect_identical(time_floor(x, "day"),
                   as.POSIXct(c("2009-08-03 00:00:00", "2010-08-03 00:00:00"), tz = "UTC"))
  expect_identical(time_floor(x, "week"),
                   as.POSIXct(c("2009-08-03 00:00:00", "2010-08-02 00:00:00"), tz = "UTC"))
  expect_identical(time_floor(x, "month"),
                   as.POSIXct(c("2009-08-01 00:00:00", "2010-08-01 00:00:00"), tz = "UTC"))
  expect_identical(time_floor(x, "year"),
                   as.POSIXct(c("2009-01-01 00:00:00", "2010-01-01 00:00:00"), tz = "UTC"))
})

test_that("time_ceiling handles vectors", {
  x <- as.POSIXct(c("2009-08-03 12:01:59.23", "2010-08-03 12:01:59.23"), tz = "UTC")
  expect_identical(time_ceiling(x, "second"),
                   as.POSIXct(c("2009-08-03 12:02:00", "2010-08-03 12:02:00"), tz = "UTC"))
  expect_identical(time_ceiling(x, "minute"),
                   as.POSIXct(c("2009-08-03 12:02:00", "2010-08-03 12:02:00"), tz = "UTC"))
  expect_identical(time_ceiling(x, "hour"),
                   as.POSIXct(c("2009-08-03 13:00:00", "2010-08-03 13:00:00"), tz = "UTC"))
  expect_identical(time_ceiling(x, "day"),
                   as.POSIXct(c("2009-08-04 00:00:00", "2010-08-04 00:00:00"), tz = "UTC"))
  expect_identical(time_ceiling(x, "week"),
                   as.POSIXct(c("2009-08-10 00:00:00", "2010-08-09 00:00:00"), tz = "UTC"))
  expect_identical(time_ceiling(x, "month"),
                   as.POSIXct(c("2009-09-01 00:00:00", "2010-09-01 00:00:00"), tz = "UTC"))
  expect_identical(time_ceiling(x, "year"),
                   as.POSIXct(c("2010-01-01 00:00:00", "2011-01-01 00:00:00"), tz = "UTC"))
})

test_that("time_ceiling handles days, weeks and months correctly on boundary", {
  x <- as.POSIXct(c("2009-08-03 00:00:00", "2010-08-02 00:00:00"), tz = "UTC")
  d <- as.Date(x)
  expect_identical(time_ceiling(x, "week", change_on_boundary = T),
                   as.POSIXct(c("2009-08-10 00:00:00", "2010-08-09 00:00:00"), tz = "UTC"))
  expect_identical(time_ceiling(x, "week", change_on_boundary = FALSE), x)
  expect_identical(time_ceiling(d, "week", change_on_boundary = T),
                   as.Date(c("2009-08-10", "2010-08-09")))
  expect_identical(time_ceiling(d, "week", change_on_boundary = F), d)
  expect_identical(time_ceiling(x, "day", change_on_boundary = T),
                   as.POSIXct(c("2009-08-04 00:00:00", "2010-08-03 00:00:00"), tz = "UTC"))
  expect_identical(time_ceiling(x, "day", change_on_boundary = FALSE), x)
  expect_identical(time_ceiling(d, "day", change_on_boundary = T),
                   as.Date(c("2009-08-04", "2010-08-03")))
  expect_identical(time_ceiling(d, "day", change_on_boundary = F), d)
  expect_identical(time_ceiling(x, "month", change_on_boundary = T),
                   as.POSIXct(c("2009-09-01 00:00:00", "2010-09-01 00:00:00"), tz = "UTC"))
  expect_identical(time_ceiling(x, "month", change_on_boundary = F),
                   as.POSIXct(c("2009-09-01 00:00:00", "2010-09-01 00:00:00"), tz = "UTC"))
  expect_identical(time_ceiling(d, "month", change_on_boundary = T),
                   as.Date(c("2009-09-01", "2010-09-01")))
  expect_identical(time_ceiling(d, "month", change_on_boundary = F),
                   as.Date(c("2009-09-01", "2010-09-01")))
  x <- as.POSIXct(c("2009-08-01 00:00:00", "2010-08-01 00:00:00"), tz = "UTC")
  d <- as.Date(x)
  expect_identical(time_ceiling(x, "month", change_on_boundary = T),
                   as.POSIXct(c("2009-09-01 00:00:00", "2010-09-01 00:00:00"), tz = "UTC"))
  expect_identical(time_ceiling(x, "month", change_on_boundary = F),
                   as.POSIXct(c("2009-08-01 00:00:00", "2010-08-01 00:00:00"), tz = "UTC"))
  expect_identical(time_ceiling(d, "month", change_on_boundary = T),
                   as.Date(c("2009-09-01", "2010-09-01")))
  expect_identical(time_ceiling(d, "month", change_on_boundary = F),
                   as.Date(c("2009-08-01", "2010-08-01")))
})


test_that("time_round handles vectors", {
  x <- as.POSIXct(c("2009-08-03 12:01:59.23", "2010-08-03 12:01:59.23"), tz = "UTC")
  expect_equal(time_round(x, "second"),
               as.POSIXct(c("2009-08-03 12:01:59",
                            "2010-08-03 12:01:59"), tz = "UTC"))
  expect_equal(time_round(x, "minute"),
               as.POSIXct(c("2009-08-03 12:02:00",
                            "2010-08-03 12:02:00"), tz = "UTC"))
  expect_equal(time_round(x, "hour"),
               as.POSIXct(c("2009-08-03 12:00:00",
                            "2010-08-03 12:00:00"), tz = "UTC"))
  expect_equal(time_round(x, "day"),
               as.POSIXct(c("2009-08-04 00:00:00",
                            "2010-08-04 00:00:00"), tz = "UTC"))
  expect_equal(time_round(x, "week"),
               as.POSIXct(c("2009-08-03 00:00:00",
                            "2010-08-02 00:00:00"), tz = "UTC"))
  expect_equal(time_round(x, "month"),
               as.POSIXct(c("2009-08-01 00:00:00",
                            "2010-08-01 00:00:00"), tz = "UTC"))
  expect_equal(time_round(x, "year"),
               as.POSIXct(c("2010-01-01 00:00:00",
                            "2011-01-01 00:00:00"), tz = "UTC"))
})

test_that("time_floor works for a variety of formats", {
  x <- as.POSIXct("2009-08-03 12:01:59", tz = "UTC")
  expect_equal(time_floor(x, "minute"),
               as.POSIXct("2009-08-03 12:01:00", tz = "UTC"))
  expect_equal(time_floor(as.Date(x), "month"),
               as.Date("2009-08-01"))
  expect_equal(time_floor(as.Date(x), "bimonth"),
               ymd("2009-07-01"))
  expect_equal(time_floor(as.Date(x), "quarter"),
               ymd("2009-07-01"))
  expect_equal(time_floor(as.Date(x), "halfyear"),
               ymd("2009-07-01"))
  expect_equal(time_floor(as.POSIXlt(x), "minute"),
               as.POSIXlt(as.POSIXct("2009-08-03 12:01:00", tz = "UTC")))
})

test_that("time_ceiling works for a variety of formats", {
  x <- as.POSIXct("2009-08-03 12:01:59", tz = "UTC")
  expect_equal(time_ceiling(x, "minute"),
               as.POSIXct("2009-08-03 12:02:00", tz = "UTC"))
  expect_equal(time_ceiling(as.Date(x), "month"),
               as.Date("2009-09-01"))
  expect_equal(time_ceiling(as.Date(x), "bimonth"),
               ymd("2009-09-01"))
  expect_equal(time_ceiling(as.Date(x), "quarter"),
               ymd("2009-10-01"))
  expect_equal(time_ceiling(as.Date(x), "halfyear"),
               ymd("2010-01-01"))
  expect_equal(time_ceiling(as.POSIXlt(x), "minute"),
               as.POSIXlt(as.POSIXct("2009-08-03 12:02:00", tz = "UTC")))
})

test_that("time_round works for a variety of formats", {
  x <- as.POSIXct("2009-08-03 12:01:59", tz = "UTC")
  expect_equal(time_round(x, "minute"), as.POSIXct("2009-08-03 12:02:00", tz = "UTC"))
  expect_equal(time_round(as.Date(x), "month"), as.Date("2009-08-01"))
  expect_equal(time_round(as.POSIXlt(x), "minute"), as.POSIXlt(as.POSIXct("2009-08-03 12:02:00", tz = "UTC")))
})

test_that("rounding works across DST", {
  ## https://github.com/hadley/lubridate/issues/399
  tt <- ymd("2016-03-27", tz = "Europe/Helsinki");
  expect_equal(time_ceiling(tt, "month"), as.POSIXct("2016-04-01", tz = "Europe/Helsinki"))
  expect_equal(time_ceiling(tt, "day"), as.POSIXct("2016-03-27", tz = "Europe/Helsinki"))
  tt <- ymd("2016-03-28", tz = "Europe/Helsinki");
  expect_equal(time_floor(tt, "month"), as.POSIXct("2016-03-01", tz = "Europe/Helsinki"))
  tt <- ymd_hms("2016-03-27 05:00:00", tz = "Europe/Helsinki");
  expect_equal(time_floor(tt, "day"), as.POSIXct("2016-03-27", tz = "Europe/Helsinki"))
  ## https://github.com/tidyverse/lubridate/issues/605
  x <- ymd_hms("2017-11-05 23:59:03", tz = 'America/New_York')
  expect_equal(time_ceiling(x, "day"), as.POSIXct("2017-11-06", tz = "America/New_York"))
})

test_that("Ceiling for partials (Date) rounds up on boundary", {
  expect_identical(time_ceiling(as.Date("2012-09-27"), "day"), ymd("2012-09-28"))
  expect_identical(time_ceiling(as.Date("2012-09-01"), "day"), ymd("2012-09-02"))
  expect_identical(time_ceiling(as.Date("2012-09-01"), "2 days"), ymd("2012-09-03"))
})

test_that("Ceiling for Date returns date when unit level is higher than day", {
  expect_true(is.Date(time_ceiling(ymd("2016-09-27"), "year")))
  expect_true(is.Date(time_ceiling(ymd("2016-09-27"), "halfyear")))
  expect_true(is.Date(time_ceiling(ymd("2016-09-27"), "quarter")))
  expect_true(is.Date(time_ceiling(ymd("2016-09-27"), "bimonth")))
  expect_true(is.Date(time_ceiling(ymd("2016-09-27"), "month")))
  expect_true(is.Date(time_ceiling(ymd("2016-09-27"), "week")))
  expect_true(is.Date(time_ceiling(ymd("2016-09-27"), "day")))
  expect_true(is.POSIXct(time_ceiling(ymd("2016-09-27"), "hour")))
  expect_true(is.POSIXct(time_ceiling(ymd("2016-09-27"), "minute")))
  expect_true(is.POSIXct(time_ceiling(ymd("2016-09-27"), "second")))
})

test_that("Ceiling for POSIXct always returns POSIXct", {
  expect_true(is.POSIXct(time_ceiling(ymd_hms("2016-09-27 00:00:00"), "year")))
  expect_true(is.POSIXct(time_ceiling(ymd_hms("2016-09-27 00:00:00"), "halfyear")))
  expect_true(is.POSIXct(time_ceiling(ymd_hms("2016-09-27 00:00:00"), "quarter")))
  expect_true(is.POSIXct(time_ceiling(ymd_hms("2016-09-27 00:00:00"), "bimonth")))
  expect_true(is.POSIXct(time_ceiling(ymd_hms("2016-09-27 00:00:00"), "month")))
  expect_true(is.POSIXct(time_ceiling(ymd_hms("2016-09-27 00:00:00"), "week")))
  expect_true(is.POSIXct(time_ceiling(ymd_hms("2016-09-27 00:00:00"), "day")))
  expect_true(is.POSIXct(time_ceiling(ymd_hms("2016-09-27 00:00:00"), "hour")))
  expect_true(is.POSIXct(time_ceiling(ymd_hms("2016-09-27 00:00:00"), "minute")))
  expect_true(is.POSIXct(time_ceiling(ymd_hms("2016-09-27 00:00:00"), "second")))
})

test_that("time_ceiling does not round up dates that are already on a boundary", {
  expect_equal(time_ceiling(ymd_hms("2012-09-01 00:00:00"), "month"), as.POSIXct("2012-09-01", tz = "UTC"))
  expect_equal(time_ceiling(ymd_hms("2012-01-01 00:00:00"), "year"), as.POSIXct("2012-01-01", tz = "UTC"))
  expect_equal(time_ceiling(ymd_hms("2012-01-01 00:00:00"), "2 year"), as.POSIXct("2012-01-01", tz = "UTC"))
  expect_equal(time_ceiling(ymd_hms("2012-01-01 00:00:00"), "3 year"), as.POSIXct("2013-01-01", tz = "UTC"))
  expect_equal(time_ceiling(ymd_hms("2012-01-01 00:00:00"), "5 year"), as.POSIXct("2015-01-01", tz = "UTC"))
  expect_equal(time_ceiling(ymd_hms("2012-01-01 01:00:00"), "second"), ymd_hms("2012-01-01 01:00:00"))
  expect_equal(time_ceiling(ymd_hms("2012-01-01 01:00:00"), "2 second"), ymd_hms("2012-01-01 01:00:00"))
  expect_equal(time_ceiling(ymd_hms("2012-01-01 01:00:00"), "2 second", change_on_boundary = T),
               ymd_hms("2012-01-01 01:00:02"))
  expect_equal(time_ceiling(ymd_hms("2012-01-01 01:00:00"), "5 second"), ymd_hms("2012-01-01 01:00:00"))
  expect_equal(time_ceiling(ymd_hms("2012-01-01 00:00:00"), "bimonth"), ymd_hms("2012-01-01 00:00:00"))
})

test_that("time_ceiling does round up dates on a boundary with change_on_boundary=TRUE", {
  expect_equal(time_ceiling(as.Date("2012-09-27"), "day", change_on_boundary = TRUE), as.Date("2012-09-28"))
  expect_equal(time_ceiling(as.Date("2012-09-01"), "month", change_on_boundary = TRUE), as.Date("2012-10-01"))
  expect_equal(time_ceiling(ymd_hms("2012-09-01 00:00:00"), "month", change_on_boundary = TRUE), ymd("2012-10-01", tz = "UTC"))
  expect_equal(time_ceiling(ymd_hms("2012-09-01 00:00:00"), "bimonth", change_on_boundary = TRUE), ymd("2012-11-01", tz = "UTC"))
  expect_equal(time_ceiling(ymd_hms("2012-01-01 00:00:00"), "year", change_on_boundary = TRUE), as.POSIXct("2013-01-01", tz = "UTC"))
  expect_equal(time_ceiling(ymd_hms("2012-01-01 01:00:00"), "hour", change_on_boundary = TRUE), ymd_hms("2012-01-01 02:00:00"))
  expect_equal(time_ceiling(ymd_hms("2012-01-01 00:00:00"), "day", change_on_boundary = TRUE), ymd("2012-01-02", tz = "UTC"))
  expect_equal(time_ceiling(ymd_hms("2012-01-01 01:00:00"), "second", change_on_boundary = TRUE), ymd_hms("2012-01-01 01:00:01"))
})

test_that("time_floor does not round down dates that are already on a boundary", {
  expect_equal(time_floor(as.Date("2012-09-27"), "day"), as.Date("2012-09-27"))
})

test_that("time_round does not round dates that are already on a boundary", {
  expect_equal(time_round(as.Date("2012-09-27"), "day"), as.Date("2012-09-27"))
})

test_that("time_ceiling returns input of length zero when given input of length zero", {
  x <- structure(vector(mode = "numeric"), class = c("POSIXct", "POSIXt"))
  expect_equal(time_ceiling(x), x)
})

test_that("time_floor returns input of length zero when given input of length zero", {
  x <- structure(vector(mode = "numeric"), class = c("POSIXct", "POSIXt"))
  expect_equal(time_floor(x), x)
})

test_that("time_round returns input of length zero when given input of length zero", {
  x <- structure(vector(mode = "numeric"), class = c("POSIXct", "POSIXt"))
  expect_equal(time_round(x), x)
})

test_that("time_round behaves correctly on 60th second", {
  ## (bug #217)
  x <- ymd_hms("2013-12-01 23:59:59.9999")
  expect_equal(time_round(x, unit = "second"),
               ymd("2013-12-02", tz = "UTC"))
  time_update(x, second = 60)
  expect_equal(x, ymd("2013-12-02", tz = "UTC"))
})

test_that("time_round and time_ceiling skip day time gap", {

  ##  (#240)
  tz <- "Europe/Amsterdam"
  times <- ymd_hms("2013-03-31 01:00:00 CET", "2013-03-31 01:15:00 CEST",
                   "2013-03-31 01:30:00 CEST", "2013-03-31 01:45:00 CEST",
                   "2013-03-31 03:00:00 CEST", "2013-03-31 03:15:00 CEST",
                   tz = tz)

  round <- ymd_hms("2013-03-31 01:00:00 CET",
                   "2013-03-31 01:00:00 CEST", "2013-03-31 03:00:00 CEST",
                   "2013-03-31 03:00:00 CEST", "2013-03-31 03:00:00 CEST",
                   "2013-03-31 03:00:00 CEST",
                   tz = tz)
  expect_equal(time_round(times, "hour"), round)

  ceiling <- ymd_hms("2013-03-31 01:00:00 CET",
                     "2013-03-31 03:00:00 CEST", "2013-03-31 03:00:00 CEST",
                     "2013-03-31 03:00:00 CEST", "2013-03-31 03:00:00 CEST",
                     "2013-03-31 04:00:00 CEST",
                     tz = tz)

  expect_equal(time_ceiling(times, "hour"), ceiling)


  tz <- "America/Chicago"

  x <- ymd_hms(c("2014-03-09 00:00:00", "2014-03-09 00:29:59", "2014-03-09 00:30:00",
                 "2014-03-09 00:59:59", "2014-03-09 01:35:00", "2014-03-09 03:15:00"),
               tz = tz)
  y <- as.POSIXct(c("2014-03-09 00:00:00", "2014-03-09 00:00:00", "2014-03-09 01:00:00",
                    "2014-03-09 01:00:00", "2014-03-09 03:00:00", "2014-03-09 03:00:00"),
                  tz = tz)
  expect_equal(time_round(x, "hour"),  y)
})

test_that("time rounding with hours works with repeated DST transitions", {

  ## WAF? as.POSIXct returns EDT/EST randomly on ctus("2014-11-02 01:00:00")
  am1 <- .POSIXct(1414904400, tz = "America/New_York")
  expect_equal(time_floor(am1 + 3600, "hour"), am1 + 3600)
  ## rounding is done in civil time for units > seconds
  expect_equal(time_ceiling(ctus("2014-11-02 00:30:00"), "hour"), am1) ## EDT (.5h)
  expect_equal(time_ceiling(ctus("2014-11-02 01:35:00"), "hour"), ctus("2014-11-02 02:00:00")) ## EST (1.5h)
  expect_equal(time_ceiling(ctus("2014-11-02 02:15:00"), "hour"), ctus("2014-11-02 03:00:00")) ## EST (45m)
  x <- ctus("2014-11-02 00:30:00")
  expect_equal(as.numeric(difftime(time_ceiling(x, "hour"), ctus(x), units = "min")), 30)
  x <- ctus("2014-11-02 01:30:00")
  expect_equal(as.numeric(difftime(time_ceiling(x, "hour"), ctus(x), units = "min")), 90)
  x <- ctus("2014-11-02 02:15:00")
  expect_equal(as.numeric(difftime(time_ceiling(x, "hour"), ctus(x), units = "min")), 45)

  ## rounding is done in absolute time for seconds
  expect_equal(time_ceiling(ctus("2014-11-02 00:30:00"), "3600a"), am1) ## EDT (.5h)
  x <- ctus("2014-11-02 00:30:00")
  expect_equal(as.numeric(difftime(time_ceiling(x, "3600a"), ctus(x), units = "min")), 30)
  x <- time_add(am1, minutes = 30) ## EDT
  expect_equal(as.numeric(difftime(time_ceiling(x, "3600a"), ctus(x), units = "min")), 30)

  ## rounding is done in civil time for units > seconds
  expect_equal(time_round(ctus("2014-11-02 00:30:00"), "hour"), am1) ## EDT (30m)
  x <- time_add(am1, minutes = 35) ## EDT
  expect_equal(as.numeric(difftime(time_round(x, "hour"), ctus(x), units = "min")), -35)
  expect_equal(as.numeric(difftime(time_round(x, "3600a"), ctus(x), units = "min")), 25)
  x <- ctus("2014-11-02 02:15:00")
  expect_equal(time_round(x, "hour"), ctus("2014-11-02 02:00:00")) ## EST (15m)
  expect_equal(as.numeric(difftime(time_round(x, "hour"), ctus(x), units = "min")), -15)
  expect_equal(as.numeric(difftime(time_round(x, "3600a"), ctus(x), units = "min")), -15)

  ## rounding is done in civil time for units > seconds
  expect_equal(time_round(ctus("2014-11-02 00:30:00"), "hour"), am1) ## EDT (30m)
  x <- .POSIXct(1414909500, tz = "America/New_York") # "2014-11-02 01:25:00 EST"
  expect_equal(as.numeric(difftime(time_floor(x, "hour"), x, units = "min")), -25)
  expect_equal(as.numeric(difftime(time_floor(x, "3600a"), x, units = "min")), -25)
  x <- ctus("2014-11-02 02:15:00") ## EST
  expect_equal(time_floor(x, "hour"), ctus("2014-11-02 02:00:00")) ## EST (15m)
  expect_equal(as.numeric(difftime(time_floor(x, "3600a"), ctus(x), units = "min")), -15)

})

test_that("time rounding with minutes works with repeated DST transitions", {

  am1 <- .POSIXct(1414904400, tz = "America/New_York")
  expect_equal(time_ceiling(am1 + 30, "min"), am1 + 60)
  expect_equal(time_ceiling(am1 + 3630, "min"), am1 + 3660)
  expect_equal(time_ceiling(am1 + 3690, "min"), am1 + 3720)
  expect_equal(time_ceiling(am1 + 30, "min"), am1 + 60)

  expect_equal(time_floor(am1 + 30, "min"), am1)
  expect_equal(time_floor(am1 + 90, "min"), am1 + 60)
  expect_equal(time_floor(am1 + 3600, "min"), am1 + 3600)
  expect_equal(time_floor(am1 + 3600, "sec"), am1 + 3600)
  expect_equal(time_floor(am1 + 3690, "min"), am1 + 3660)
  expect_equal(time_floor(am1 + 3690, "5min"), am1 + 3600)

  ## rounding is done in civil time for units > seconds
  x <- .POSIXct(1414909530, tz = "America/New_York") # "2014-11-02 01:25:30 EST"
  expect_equal(as.numeric(difftime(time_floor(x, "minute"), x, units = "secs")), -30)
  expect_equal(as.numeric(difftime(time_floor(x, "60s"), x, units = "secs")), -30)
  x <- am1 + 1890
  expect_equal(time_floor(x, "minute"), am1 + 1860)
  expect_equal(time_floor(x, "3minute"), am1 + 1800)
  x <- ctus("2014-11-02 02:15:00") ## EST
  expect_equal(time_floor(x, "minute"), ctus("2014-11-02 02:15:00"))
  expect_equal(time_floor(x, "5minute"), ctus("2014-11-02 02:15:00"))
  expect_equal(time_floor(x, "4minute"), ctus("2014-11-02 02:12:00"))
  expect_equal(as.numeric(difftime(time_floor(x, "3600a"), ctus(x), units = "min")), -15)

})

test_that("time_ceiling, time_round and time_floor behave correctly with NA", {
  am1 <- .POSIXct(1414904400, tz = "America/New_York")
  ## (bug lubridate #486)
  x <- time_add(ymd_hms("2009-08-03 12:01:59.23", tz = "UTC"), days = 0:1)
  x[2] <- NA
  expect_equal(time_ceiling(x, unit = "day"), ymd(c("2009-08-04", NA), tz = "UTC"))
  expect_equal(time_ceiling(x, unit = "seconds"), ymd_hms(c("2009-08-03 12:02:00", NA), tz = "UTC"))
  expect_equal(time_ceiling(x, unit = "months"), ymd(c("2009-09-01", NA), tz = "UTC"))
  expect_equal(time_floor(x, unit = "day"), ymd(c("2009-08-03", NA), tz = "UTC"))
  expect_equal(time_floor(x, unit = "months"), ymd(c("2009-08-01", NA), tz = "UTC"))
  expect_equal(time_round(x, unit = "minute"), ymd_hms(c("2009-08-03 12:02:00", NA), tz = "UTC"))
})

test_that("time_floor works for seasons", {
  dts <- ymd_hms(sprintf("2017-%d-02 0:34:3", 1:12))
  expect_equal(month(time_floor(dts, "season")), c(12, 12, 3, 3, 3, 6, 6, 6, 9, 9, 9, 12))
  dts <- time_force_tz(dts, "America/New_York")
  expect_equal(month(time_floor(dts, "season")), c(12, 12, 3, 3, 3, 6, 6, 6, 9, 9, 9, 12))
})

test_that("time_ceiling works for seasons", {
  dts <- ymd_hms(sprintf("2017-%d-02 0:34:3", 1:12))
  expect_equal(month(time_ceiling(dts, "season")), c(3, 3, 6, 6, 6, 9, 9, 9, 12, 12, 12, 3))
  dts <- time_force_tz(dts, "America/New_York")
  expect_equal(month(time_ceiling(dts, "season")), c(3, 3, 6, 6, 6, 9, 9, 9, 12, 12, 12, 3))
})

test_that("round on week respects week_start", {
  date <- ymd("2017-05-07") ## sunday
  ct <- as.POSIXct("2010-02-03 13:45:59", tz = "America/New_York", format = "%Y-%m-%d %H:%M:%S") ## Wednesday

  expect_equal(wday(time_floor(ct, "week", week_start = 1)), 1)
  expect_equal(wday(time_floor(ct, "week", week_start = 2)), 2)
  expect_equal(wday(time_floor(ct, "week", week_start = 5)), 5)
  expect_equal(wday(time_floor(ct, "week", week_start = 7)), 7)
  expect_equal(wday(time_floor(date, "week", week_start = 1)), 1)
  expect_equal(wday(time_floor(date, "week", week_start = 2)), 2)
  expect_equal(wday(time_floor(date, "week", week_start = 5)), 5)
  expect_equal(wday(time_floor(date, "week", week_start = 7)), 7)

  expect_equal(wday(time_ceiling(ct, "week", week_start = 1)), 1)
  expect_equal(wday(time_ceiling(ct, "week", week_start = 2)), 2)
  expect_equal(wday(time_ceiling(ct, "week", week_start = 5)), 5)
  expect_equal(wday(time_ceiling(ct, "week", week_start = 7)), 7)
  expect_equal(wday(time_ceiling(date, "week", week_start = 1)), 1)
  expect_equal(wday(time_ceiling(date, "week", week_start = 2)), 2)
  expect_equal(wday(time_ceiling(date, "week", week_start = 5)), 5)
  expect_equal(wday(time_ceiling(date, "week", week_start = 7)), 7)

})
