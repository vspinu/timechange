
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
Rcpp::DataFrame C_time_get(const NumericVector& dt,
                           const CharacterVector& components,
                           const int week_start = 1) {

  std::vector<std::string> comps = Rcpp::as<std::vector<std::string>>(components);

  bool
    do_year = false, do_month = false, do_yday = false,
    do_mday = false, do_wday = false, do_hour = false,
    do_minute = false, do_second = false;

  R_xlen_t N = dt.size(), N_comps = components.size();

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

  List out(N_comps);
  CharacterVector names(N_comps);

  R_len_t pos = 0;

  for (std::string comp: comps) {
    if (comp == "year") { out[pos] = year; names[pos] = "year"; pos++; };
    if (comp == "month") { out[pos] = month; names[pos] = "month"; pos++; };
    if (comp == "yday") { out[pos] = yday; names[pos] = "yday"; pos++; };
    if (comp == "day") { out[pos] = mday; names[pos] = "day"; pos++; };
    if (comp == "mday") { out[pos] = mday; names[pos] = "mday"; pos++; }
    if (comp == "wday") { out[pos] = wday; names[pos] = "wday"; pos++; };
    if (comp == "hour") { out[pos] = hour; names[pos] = "hour"; pos++; };
    if (comp == "minute") { out[pos] = minute; names[pos] = "minute"; pos++; };
    if (comp == "second") { out[pos] = second; names[pos] = "second"; pos++; };
  }

  out.attr("names") = names;
  return out;
}
