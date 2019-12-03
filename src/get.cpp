
#include "common.h"

// CIVIL TIME:
// https://github.com/google/cctz/blob/master/include/cctz/civil_time.h
// https://github.com/devjgm/papers/blob/master/d0215r1.md

// TIME ZONES:
// https://github.com/google/cctz/blob/master/include/cctz/time_zone.h
// https://github.com/devjgm/papers/blob/master/d0216r1.md
// https://raw.githubusercontent.com/devjgm/papers/master/resources/struct-civil_lookup.png

// R's timezone registry:
// https://github.com/SurajGupta/r-source/blob/master/src/extra/tzone/registryTZ.c

// C++20 date/calendar proposal: https://github.com/HowardHinnant/date

bool charvec_contains(const CharacterVector vec, const std::string& elt) {
  return std::find(vec.begin(), vec.end(), elt) != vec.end();
}

// [[Rcpp::export]]
Rcpp::List C_time_get(const NumericVector& dt,
                      const CharacterVector& components,
                      const int week_start = 1) {

  std::vector<std::string> comps = Rcpp::as<std::vector<std::string>>(components);

  bool
    do_year = false, do_month = false, do_yday = false,
    do_mday = false, do_wday = false, do_hour = false,
    do_minute = false, do_second = false;

  R_xlen_t N = dt.size();

  for (std::string comp: comps) {
    if (comp == "year") { do_year = true; continue; };
    if (comp == "month") { do_month = true; continue; };
    if (comp == "yday") { do_yday = true; continue; };
    if (comp == "day" || comp == "mday") { do_mday = true; continue; };
    if (comp == "wday") { do_wday = true; continue; };
    if (comp == "hour") { do_hour = true; continue; };
    if (comp == "minute") { do_minute = true; continue; };
    if (comp == "second") { do_second = true; continue; };
    Rf_error("Invalid datetime component '%s'", comp.c_str());
  }

  IntegerVector year(do_year ? N : 0);
  IntegerVector month(do_month ? N :0);
  IntegerVector yday(do_yday ? N : 0);
  IntegerVector mday(do_mday ? N : 0);
  IntegerVector wday(do_wday ? N : 0);
  IntegerVector hour(do_hour ? N : 0);
  IntegerVector minute(do_minute ? N : 0);
  NumericVector second(do_second ? N : 0);

  std::string tz = tz_from_tzone_attr(dt);
  cctz::time_zone tzone;
  load_tz_or_fail(tz, tzone, "CCTZ: Invalid timezone of the input vector: \"%s\"");

  for (R_xlen_t i = 0; i < N; i++)
    {
      double dti = dt[i];
      int_fast64_t secs = floor_to_int64(dti);
      double rem = dti - secs;

      if (ISNAN(dti) || secs == NA_INT64) {
        if (do_year) year[i] = NA_INTEGER;
        if (do_month) month[i] = NA_INTEGER;
        if (do_yday) yday[i] = NA_INTEGER;
        if (do_mday) mday[i] = NA_INTEGER;
        if (do_wday) wday[i] = NA_INTEGER;
        if (do_hour) hour[i] = NA_INTEGER;
        if (do_minute) minute[i] = NA_INTEGER;
        if (do_second) second[i] = NA_REAL;
        continue;
      }

      sys_seconds ss(secs);
      time_point tp(ss);
      cctz::civil_second ct = cctz::convert(tp, tzone);

      if (do_year) year[i] = ct.year();
      if (do_month) month[i] = ct.month();
      if (do_yday) yday[i] = cctz::get_yearday(cctz::civil_day(ct));
      if (do_mday) mday[i] = ct.day();
      if (do_wday)  {
        // wday is 1 based and starts on week_start
        int cur_wday = (static_cast<int>(cctz::get_weekday(cctz::civil_day(ct))) + 8 - week_start) % 7;
        wday[i] = cur_wday;
      }
      if (do_hour) hour[i] = ct.hour();
      if (do_minute) minute[i] = ct.minute();
      if (do_second) second[i] = ct.second() + rem;

    }

  List out = DataFrame::create();
  CharacterVector names;

  for (std::string comp: comps) {
    if (comp == "year") { out.push_back(year); names.push_back("year"); continue; };
    if (comp == "month") { out.push_back(month); names.push_back("month"); continue; };
    if (comp == "yday") { out.push_back(yday); names.push_back("yday"); continue; };
    if (comp == "day") { out.push_back(mday); names.push_back("day"); continue; };
    if (comp == "mday") { out.push_back(mday); names.push_back("mday"); continue; };
    if (comp == "wday") { out.push_back(wday); names.push_back("wday"); continue; };
    if (comp == "hour") { out.push_back(hour); names.push_back("hour"); continue; };
    if (comp == "minute") { out.push_back(minute); names.push_back("minute"); continue; };
    if (comp == "second") { out.push_back(second); names.push_back("second"); continue; };
  }

  out.attr("names") = names;
  return out;
}
