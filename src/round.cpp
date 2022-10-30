
#include "common.h"

#define FLOOR_MULTI_UNIT(x, n) ((x/n) * n)
#define FLOOR_MULTI_UNIT1(x, n) (((x - 1)/n) * n)
inline double floor_multi_unit(double x, double n) noexcept {
  return std::floor(x/n) * n;
}

#define CEIL_MULTI_UNIT(x, n) ((x / n) * n + n)
#define CEIL_MULTI_UNIT1(x, n) (((x - 1) / n) * n + n)
inline double ceil_multi_unit(double x, double n) noexcept {
  return std::floor(x/n) * n + n;
}

enum class Unit {YEAR, HALFYEAR, QUARTER, SEASON, BIMONTH, MONTH,
  WEEK, DAY, HOUR, MINUTE, SECOND, ASECOND};

Unit name2unit(std::string unit_name) {
  if (unit_name == "asecond") return Unit::ASECOND;
  if (unit_name == "second") return Unit::SECOND;
  if (unit_name == "minute") return Unit::MINUTE;
  if (unit_name == "hour") return Unit::HOUR;
  if (unit_name == "day") return Unit::DAY;
  if (unit_name == "month") return Unit::MONTH;
  if (unit_name == "bimonth") return Unit::BIMONTH;
  if (unit_name == "season") return Unit::SEASON;
  if (unit_name == "quarter") return Unit::QUARTER;
  if (unit_name == "halfyear") return Unit::HALFYEAR;
  if (unit_name == "year") return Unit::YEAR;
  if (unit_name == "week") return Unit::WEEK;
  Rf_error("Invalid unit_name (%s)", unit_name.c_str());
}

// TOTHINK: roll_dst is hard-coded. Should this be generalized?

// used in time_floor exclusively
template<typename T>
inline double ct2posix4floor(const T& ct,
                             const cctz::time_zone& tz,
                             const time_point& tp_orig,
                             const cctz::civil_second& cs_orig) noexcept {
  cctz::time_zone::civil_lookup cl = tz.lookup(ct);
  return civil_lookup_to_posix(
    cl, tz, tp_orig, cs_orig, DST(RollDST::BOUNDARY, RollDST::POST), 0.0);
}

// used for time_ceiling exclusively
template<typename T>
inline double ct2posix4ceiling(const T& ct,
                               const cctz::time_zone& tz,
                               const time_point& tp_orig,
                               const cctz::civil_second& cs_orig,
                               const int N,
                               const bool check_boundary) noexcept {
  if (check_boundary && cs_orig == ct - N) {
    time_point tpnew = cctz::convert(cs_orig, tz);
    return tpnew.time_since_epoch().count();
  } else {
    cctz::time_zone::civil_lookup cl = tz.lookup(ct);
    return civil_lookup_to_posix(
      cl, tz, tp_orig, cs_orig, DST(RollDST::BOUNDARY, RollDST::PRE), 0.0);
  }
}

// [[Rcpp::export]]
Rcpp::newDatetimeVector C_time_ceiling(const NumericVector dt,
                                       const std::string unit_name,
                                       const double nunits,
                                       const int week_start,
                                       const bool change_on_boundary) {

  Unit unit = name2unit(unit_name);
  std::string tz_name = tz_from_tzone_attr(dt);
  cctz::time_zone tz;
  load_tz_or_fail(tz_name, tz, "CCTZ: Invalid timezone of the input vector: \"%s\"");

  size_t n = dt.size();
  NumericVector out(n);
  int N = static_cast<int>(nunits);
  if (unit != Unit::ASECOND && N == 0)
    Rf_error("Unit is 0 or fractional. Use 'aseconds' for fractional rounding.");
  if (unit == Unit::HALFYEAR) N *= 6;
  else if (unit == Unit::QUARTER) N *= 3;
  else if (unit == Unit::BIMONTH) N *= 2;
  else if (unit == Unit::SEASON) N *= 3;
  cctz::weekday wday = static_cast<cctz::weekday>(week_start - 1);

  for (size_t i = 0; i < n; i++) {
    double dsecs = dt[i];
    int_fast64_t secs = floor_to_int64(dsecs);
    double rem = dsecs - secs;
    bool check_boundary = rem == 0 && !change_on_boundary;
    if (secs == NA_INT64) { out[i] = NA_REAL; continue; }
    sys_seconds ss(secs);
    time_point tp(ss);
    cctz::civil_second cs = cctz::convert(tp, tz);
    switch(unit) {
     case Unit::ASECOND : {
       // aseconds are duration in seconds: fractional nunits and nunits > 60 are supported
       double posix = ceil_multi_unit(dsecs, nunits);
       /* Rprintf("dsecs:%f posix:%f\n", dsecs, posix); */
       if (check_boundary && (posix - nunits) == dsecs)
         out[i] = dsecs;
       else
         out[i] = posix;
       break;
     }
     case Unit::SECOND : {
       cctz::civil_second ct = cctz::civil_second(cctz::civil_minute(cs)) + CEIL_MULTI_UNIT(cs.second(), N);
       out[i] = ct2posix4ceiling(ct, tz, tp, cs, N, check_boundary); break;
     }
     case Unit::MINUTE : {
       cctz::civil_minute ct = cctz::civil_minute(cctz::civil_hour(cs)) + CEIL_MULTI_UNIT(cs.minute(), N);
       out[i] = ct2posix4ceiling(ct, tz, tp, cs, N, check_boundary); break;
     }
     case Unit::HOUR : {
       cctz::civil_hour ct = cctz::civil_hour(cctz::civil_day(cs)) + CEIL_MULTI_UNIT(cs.hour(), N);
       out[i] = ct2posix4ceiling(ct, tz, tp, cs, N, check_boundary); break;
     }
     case Unit::DAY : {
       /* Rprintf("day:%d ceil:%d\n", cs.day(), CEIL_MULTI_UNIT1(cs.day(), N)); */
       cctz::civil_day ct = cctz::civil_day(cctz::civil_month(cs)) + CEIL_MULTI_UNIT1(cs.day(), N);
       out[i] = ct2posix4ceiling(ct, tz, tp, cs, N, check_boundary); break;
     }
     case Unit::WEEK : {
       if (N != 1)
         Rf_warning("Multi-unit week ceiling is not supported; ignoring");
       cctz::civil_day ct = cctz::next_weekday(cctz::civil_day(cs), wday);
       out[i] = ct2posix4ceiling(ct, tz, tp, cs, 7, check_boundary); break;
     }
     case Unit::SEASON : {
       cctz::civil_month ct = cctz::civil_month(cctz::civil_year(cs)) + CEIL_MULTI_UNIT(cs.month(), N) - 1;
       out[i] = ct2posix4ceiling(ct, tz, tp, cs, N, check_boundary); break;
     }
     case Unit::HALFYEAR :
     case Unit::QUARTER :
     case Unit::BIMONTH :
     case Unit::MONTH : {
       cctz::civil_month ct = cctz::civil_month(cctz::civil_year(cs)) + CEIL_MULTI_UNIT1(cs.month(), N);
       out[i] = ct2posix4ceiling(ct, tz, tp, cs, N, check_boundary); break;
     }
     case Unit::YEAR : {
       cctz::civil_year ct = cctz::civil_year(CEIL_MULTI_UNIT(cs.year(), N));
       out[i] = ct2posix4ceiling(ct, tz, tp, cs, N, check_boundary); break;
     }
    }
  }

  return newDatetimeVector(out, tz_name.c_str());
}


// [[Rcpp::export]]
Rcpp::newDatetimeVector C_time_floor(const NumericVector dt,
                                     const std::string unit_name,
                                     const double nunits,
                                     const int week_start) {

  Unit unit = name2unit(unit_name);
  std::string tz_name = tz_from_tzone_attr(dt);
  cctz::time_zone tz;
  load_tz_or_fail(tz_name, tz, "CCTZ: Invalid timezone of the input vector: \"%s\"");

  size_t n = dt.size();
  NumericVector out(n);
  int N = static_cast<int>(nunits);
  if (unit != Unit::ASECOND && N == 0)
    Rf_error("Unit is 0 or fractional. Use 'aseconds' for fractional rounding.");
  if (unit == Unit::HALFYEAR) N *= 6;
  else if (unit == Unit::QUARTER) N *= 3;
  else if (unit == Unit::BIMONTH) N *= 2;
  else if (unit == Unit::SEASON) N *= 3;
  cctz::weekday wday = static_cast<cctz::weekday>(week_start - 1);

  for (size_t i = 0; i < n; i++) {
    double dsecs = dt[i];
    int_fast64_t secs = floor_to_int64(dsecs);
    if (secs == NA_INT64) { out[i] = NA_REAL; continue; }
    sys_seconds ss(secs);
    time_point tp(ss);
    cctz::civil_second cs = cctz::convert(tp, tz);
    switch(unit) {
     case Unit::ASECOND : {
       // seconds are special: fractional nunits and nunits > 60 are supported
       out[i] = floor_multi_unit(dsecs, nunits); break;
     }
     case Unit::SECOND : {
       cctz::civil_second ct = cctz::civil_second(cctz::civil_minute(cs)) + FLOOR_MULTI_UNIT(cs.second(), N);
       out[i] = ct2posix4floor(ct, tz, tp, cs); break;
     }
     case Unit::MINUTE : {
       cctz::civil_minute ct = cctz::civil_minute(cctz::civil_hour(cs)) + FLOOR_MULTI_UNIT(cs.minute(), N);
       out[i] = ct2posix4floor(ct, tz, tp, cs); break;
     }
     case Unit::HOUR : {
       cctz::civil_hour ct = cctz::civil_hour(cctz::civil_day(cs)) + FLOOR_MULTI_UNIT(cs.hour(), N);
       out[i] = ct2posix4floor(ct, tz, tp, cs); break;
     }
     case Unit::DAY : {
       cctz::civil_day ct = cctz::civil_day(cctz::civil_month(cs)) + FLOOR_MULTI_UNIT1(cs.day(), N);
       out[i] = ct2posix4floor(ct, tz, tp, cs); break;
     }
     case Unit::WEEK : {
       if (N != 1)
         Rf_warning("Multi-unit week rounding is not supported; ignoring");
       // get the previous `wday` if `cs` is not already on `wday`
       cctz::civil_day ct = cctz::next_weekday(cctz::civil_day(cs), wday) - 7;
       out[i] = ct2posix4floor(ct, tz, tp, cs); break;
     }
     case Unit::SEASON : {
       cctz::civil_month ct = cctz::civil_month(cctz::civil_year(cs)) + FLOOR_MULTI_UNIT(cs.month(), N) - 1;
       out[i] = ct2posix4floor(ct, tz, tp, cs); break;
     }
     case Unit::HALFYEAR :
     case Unit::QUARTER :
     case Unit::BIMONTH :
     case Unit::MONTH : {
       cctz::civil_month ct = cctz::civil_month(cctz::civil_year(cs)) + FLOOR_MULTI_UNIT1(cs.month(), N);
       out[i] = ct2posix4floor(ct, tz, tp, cs); break;
     }
     case Unit::YEAR : {
       cctz::civil_year ct = cctz::civil_year(FLOOR_MULTI_UNIT(cs.year(), N));
       out[i] = ct2posix4floor(ct, tz, tp, cs); break;
     }
    }
  }

  return newDatetimeVector(out, tz_name.c_str());
}
