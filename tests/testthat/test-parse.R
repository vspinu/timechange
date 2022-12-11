context("Parser")

test_that("parse_unit works as expected", {

  expect_identical(
    parse_unit(c("1.2s", "1.2 s", "1.2S",  "1.2 secs", " 1.2  seco", " 1.2 seconds ")),
    list(n = rep.int(1.2, 6),
         unit = rep.int("second", 6)))

  expect_identical(
    parse_unit(c("1M", "1 mi", "mi",  "1 mins", " 1  minu", " minutes ")),
    list(n = rep.int(1, 6),
         unit = rep.int("minute", 6)))

  expect_identical(
    parse_unit(c("-1M", "-.1 d", "-1000.0000001y", "-1000.0000001years")),
    list(n = c(-1, -.1, -1000.0000001, -1000.0000001),
         unit = c("minute", "day", "year", "year")))

  expect_identical(
    parse_unit(c("-1sea", "-.1 seaso", "-1000.0000001seasons ")),
    list(n = c(-1, -.1, -1000.0000001),
         unit = c("season", "season", "season")))

  expect_identical(
    parse_unit(c("-1h", "-.1ha", "-1000.0000001se", "-1000.0000001sea")),
    list(n = c(-1, -.1, -1000.0000001, -1000.0000001),
         unit = c("hour", "halfyear", "second", "season")))

  expect_identical(parse_unit("asecs"), list(n = 1, unit = "asecond"))

})

test_that("parse_unit errors on invalid unit", {
  expect_error(parse_unit("1 blabla"), "Invalid unit.*blabla")
})
