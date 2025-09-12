context("Time Zones")

test_that("time_at_tz works as expected", {
  x <- ctus("2008-08-03 10:01:59")
  y <- as.POSIXlt(x)
  expect_equal(time_at_tz(x, "UTC"),
               as.POSIXct(format(as.POSIXct(x), tz = "UTC"), tz = "UTC"))
  expect_equal(time_at_tz(y, "UTC"),
               as.POSIXlt(format(as.POSIXct(x), tz = "UTC"), tz = "UTC"))
})

test_that("time_at_tz handles vectors", {
  x <- as.POSIXct(c("2008-08-03 13:01:59", "2009-08-03 10:01:59"), tz = "America/New_York")
  y <- as.POSIXlt(x)
  expect_equal(time_at_tz(x, "UTC"),
               as.POSIXct(format(as.POSIXct(x), tz = "UTC"), tz = "UTC"))
  expect_equal(time_at_tz(y, "UTC"),
               as.POSIXlt(format(as.POSIXct(x), tz = "UTC"), tz = "UTC"))
})

test_that("time_at_tz handles various date-time classes", {
  x <- as.POSIXct("2008-08-03 13:01:59", tz = "America/New_York")
  expect_equal(time_at_tz(as.POSIXlt(x), "UTC"),
               as.POSIXlt(format(as.POSIXct(x), tz = "UTC"), tz = "UTC"))
})

test_that("time_at_tz handles data.frames", {
  x <- as.POSIXct("2008-08-03 10:01:59", tz = "America/New_York")
  df <- data.frame(x = x, y = as.POSIXlt(x), z = "blabla")
  df <- time_at_tz(df, "UTC")
  x_out <- as.POSIXct(format(as.POSIXct(x), tz = "UTC"), tz = "UTC")
  expect_equal(df$x, x_out)
  expect_equal(df$y, x_out)
})

test_that("time_force_tz works as expected", {
  x <- ctutc(c("2009-08-07 00:00:01", "2009-08-07 00:00:01"))
  expect_equal(time_force_tz(x, tz = c("America/New_York", "Europe/Amsterdam"), tzout = "UTC"),
               ymd_hms("2009-08-07 04:00:01 UTC", "2009-08-06 22:00:01 UTC"))
  expect_equal(time_force_tz(x, tz = c("America/New_York", "Europe/Amsterdam"), tzout = "America/New_York"),
               ymd_hms("2009-08-07 00:00:01 EDT", "2009-08-06 18:00:01 EDT", tz = "America/New_York"))
  ## recycling
  expect_equal(time_force_tz(x, tz = "America/New_York", tzout = "UTC"),
               ymd_hms("2009-08-07 04:00:01 UTC", "2009-08-07 04:00:01 UTC"))
  x <- ymd_hms("2009-08-07 00:00:01")
  expect_equal(time_force_tz(x, tz = c("America/New_York", "Europe/Amsterdam"), tzout = "UTC"),
               ymd_hms("2009-08-07 04:00:01 UTC", "2009-08-06 22:00:01 UTC"))
  expect_equal(time_force_tz(x, tz = c("America/New_York", "Europe/Amsterdam"), tzout = "America/New_York"),
               ymd_hms("2009-08-07 00:00:01 EDT", "2009-08-06 18:00:01 EDT", tz = "America/New_York"))
})

test_that("time_force_tzs is robusts against overflow", {
  s <- c("2038-01-19 03:14:06", "2038-01-19 03:14:07", "2038-01-19 03:14:08",
         "2038-01-19 03:14:09", "2038-01-19 03:14:10")
  expect_equal(time_force_tz(ymd_hms(s), tz = "America/New_York"),
               ymd_hms(s, tz = "America/New_York"))
  expect_equal(time_force_tz(ymd_hms(s, tz = "Europe/Helsinki"), tz = "America/New_York"),
               ymd_hms(s, tz = "America/New_York"))
})

test_that("time_clock_at_tz works as expected", {
  x <- ymd_hms(c("2009-08-07 01:02:03", "2009-08-07 10:20:30"))
  expect_equal(time_clock_at_tz(x, units = "secs"),
               as.difftime(c(3723, 37230), units = "secs"))
  expect_equal(time_clock_at_tz(x, units = "hours"),
               as.difftime(c(3723, 37230)/3600, units = "hours"))
  expect_equal(time_clock_at_tz(x, "Europe/Amsterdam"),
               time_clock_at_tz(time_at_tz(x, "Europe/Amsterdam")))
  x <- ymd_hms("2009-08-07 01:02:03")
  expect_equal(time_clock_at_tz(x, c("America/New_York", "Europe/Amsterdam", "Asia/Shanghai")),
               c(time_clock_at_tz(time_at_tz(x, "America/New_York")),
                 time_clock_at_tz(time_at_tz(x, "Europe/Amsterdam")),
                 time_clock_at_tz(time_at_tz(x, "Asia/Shanghai"))))
})

test_that("time_at_tz throws warning on unrecognized time zones", {
  expect_warning(time_at_tz(now(), "blablabla"))
  expect_silent(time_at_tz(now(), "UTC"))
  expect_silent(time_at_tz(now(), ""))
  expect_silent(time_at_tz(now(), "America/New_York"))
})

test_that("time_force_tz works as expected", {
  x <- as.POSIXct("2008-08-03 10:01:59", tz = "America/New_York")
  expect_equal(time_force_tz(x, "UTC"),
               as.POSIXct(format(as.POSIXct(x)), tz = "UTC"))
})

test_that("time_force_tz handles vectors", {
  x <- as.POSIXct(c("2008-08-03 13:01:59", "2009-08-03 10:01:59"), tz = "America/New_York")
  expect_equal(time_force_tz(x, "UTC"),
               as.POSIXct(format(as.POSIXct(x)), tz = "UTC"))
})

test_that("time_force_tz handles various date-time classes", {
  x <- as.POSIXct("2008-12-03 13:01:59", tz = "America/New_York")
  expect_equal(time_force_tz(as.POSIXlt(x), "UTC"),
               as.POSIXlt(format(x), tz = "UTC"))
  x <- as.Date("2008-12-03")
  expect_equal(time_force_tz(x, "UTC"),
               as.POSIXlt(format(x), tz = "UTC"))
  expect_equal(time_force_tz(x, "America/New_York"),
               as.POSIXlt(format(x), tz = "America/New_York"))
})

test_that("time_force_tz handles data.frames", {
  x <- as.POSIXct("2008-08-03 10:01:59", tz = "America/New_York")
  x_out <- as.POSIXct(format(as.POSIXct(x)), tz = "UTC")
  df <- data.frame(x = x, y = as.POSIXlt(x), z = "blabla")
  df <- time_force_tz(df, "UTC")
  expect_equal(df$x, x_out)
  expect_equal(df$y, x_out)
})

test_that("time_force_tz doesn't return NA just because new time zone uses DST", {
  poslt <- as.POSIXlt("2009-03-14 02:59:59", tz = "UTC", format
                      = "%Y-%m-%d %H:%M:%S")
  poslt2 <- time_force_tz(poslt, tz = "America/New_York")
  expect_true(!is.na(poslt2))
})

## test_that("olson_time_zones returns a non-trivial character vector", {
##   tz_olson <- olson_time_zones()
##   expect_true(length(tz_olson) > 0)
##   expect_is(tz_olson, "character")
## })

test_that("DST times are forced_tz correctly", {
  ## DST repeated "2014-11-02 01:35:00 CST"
  ref <- structure(1414913700, tzone = "America/Chicago", class = c("POSIXct", "POSIXt"))
  expect_equal(C_force_tz(ymd_hms("2014-11-02 01:35:00"), tz = "America/Chicago", "post"),
               ref)
  expect_equal(C_force_tz(ymd_hms("2014-11-02 01:35:00"), tz = "America/Chicago", "pre"),
               ref - 3600)
  expect_equal(C_force_tz(ymd_hms("2014-11-02 01:35:00"), tz = "America/Chicago", "boundary"),
               ref - 35*60)

  ref <- ymd_hms("2014-11-02 00:00:00", tz = "America/Los_Angeles")
  tt <- ref + 95*60
  expect_equal(
    time_force_tz(time_force_tz(tt, "America/Chicago", roll_dst = "post"),
                  "America/Los_Angeles", roll_dst = "post"),
    ref + 95*60)
  expect_equal(
    time_force_tz(ymd_hms("2014-11-02 01:35:00"), "America/Los_Angeles", roll_dst = "post"),
    ref + 155*60)
  expect_equal(
    time_force_tz(ymd_hms("2014-11-02 01:35:00"), "America/Los_Angeles", roll_dst = "pre"),
    ref + 95*60)
})


test_that("If original post/pre is known, ignore roll_DST", {
  tt <- ymd_hms("2014-11-02 01:35:00")
  chpst <- time_force_tz(tt, "America/Chicago", roll_dst = "post")
  chpdt <- time_force_tz(tt, "America/Chicago", roll_dst = "pre")
  lapst <- time_force_tz(tt, "America/Los_Angeles", roll_dst = "post")
  lapdt <- time_force_tz(tt, "America/Los_Angeles", roll_dst = "pre")

  expect_equal(lapst, time_force_tz(chpst, "America/Los_Angeles", roll_dst = "pre"))
  expect_equal(lapst, time_force_tz(chpst, "America/Los_Angeles", roll_dst = "post"))
  expect_equal(lapst, time_force_tz(chpst, "America/Los_Angeles", roll_dst = "boundary"))

  expect_equal(lapdt, time_force_tz(chpdt, "America/Los_Angeles", roll_dst = "pre"))
  expect_equal(lapdt, time_force_tz(chpdt, "America/Los_Angeles", roll_dst = "post"))
  expect_equal(lapdt, time_force_tz(chpdt, "America/Los_Angeles", roll_dst = "boundary"))

  ## expect_equal(lapst, time_force_tzs(chpst, "America/Los_Angeles", roll_dst = "pre"))
  ## expect_equal(lapst, time_force_tzs(chpst, "America/Los_Angeles", roll_dst = "post"))
  ## expect_equal(lapst, time_force_tzs(chpst, "America/Los_Angeles", roll_dst = "boundary"))
  ## expect_equal(lapst, time_force_tzs(chpst, "America/Los_Angeles", roll_dst = "pre"))
  ## expect_equal(lapst, time_force_tzs(chpst, "America/Los_Angeles", roll_dst = "post"))
  ## expect_equal(lapst, time_force_tzs(chpst, "America/Los_Angeles", roll_dst = "boundary"))

  expect_equal(time_at_tz(c(tt, chpdt, lapdt), "America/Chicago"),
               time_force_tz(tt,  #c(tt, chpdt, lapdt),
                             c("UTC", "America/Chicago", "America/Los_Angeles"),
                             tzout = "America/Chicago",
                             roll_dst = "pre"))

  expect_equal(time_at_tz(c(tt, chpst, lapst), "America/Chicago"),
               time_force_tz(tt,  #c(tt, chpdt, lapdt),
                             c("UTC", "America/Chicago", "America/Los_Angeles"),
                             tzout = "America/Chicago",
                             roll_dst = "post"))

  expect_equal(time_at_tz(c(tt, chpdt, lapdt), "America/Chicago"),
               time_force_tz(chpdt,
                             c("UTC", "America/Chicago", "America/Los_Angeles"),
                             tzout = "America/Chicago",
                             roll_dst = "pre"))

  expect_equal(time_at_tz(c(tt, chpdt, lapdt), "America/Chicago"),
               time_force_tz(chpdt,
                             c("UTC", "America/Chicago", "America/Los_Angeles"),
                             tzout = "America/Chicago",
                             roll_dst = "post"))

  expect_equal(time_at_tz(c(tt, chpst, lapst), "America/Chicago"),
               time_force_tz(chpst,
                             c("UTC", "America/Chicago", "America/Los_Angeles"),
                             tzout = "America/Chicago",
                             roll_dst = "post"))

  expect_equal(time_at_tz(c(tt, chpst + 100, chpst + 3600), "America/Chicago"),
               time_force_tz(time_at_tz(c(chpst, chpst + 100, chpdt), "America/Chicago"),
                             c("UTC", "America/Chicago", "America/Los_Angeles"),
                             tzout = "America/Chicago",
                             roll_dst = "post"))
})

test_that("force_tz works within repeated DST", {
  ## "2014-11-02 00:00:00 CST"
  ch <- ymd_hms("2014-11-02 00:00:00", tz = "America/Chicago")
  eu <- ymd_hms("2014-11-02 00:00:00", tz = "Europe/Berlin")
  expect_equal(
    time_force_tz(eu + (95*60), "America/Chicago", roll_dst = "post"),
    ch + (155*60))
  expect_equal(
    time_force_tz(eu + (95*60), "America/Chicago", roll_dst = "pre"),
    ch + (95*60))
  expect_equal(
    time_force_tz(eu + (95*60), "America/Chicago", roll_dst = "boundary"),
    ch + (120*60))


  expect_equal(
    time_force_tz(ch + (95*60), "Europe/Berlin", roll_dst = "post"),
    eu + (95*60))
  expect_equal(
    time_force_tz(ch + (95*60), "Europe/Berlin", roll_dst = "pre"),
    eu + (95*60))
  expect_equal(
    time_force_tz(ch + (95*60), "Europe/Berlin", roll_dst = "boundary"),
    eu + (95*60))
  expect_equal(
    time_force_tz(ch + (155*60), "Europe/Berlin", roll_dst = "post"),
    eu + (95*60))
})
