
#include "R_ext/Print.h"
#include "common.h"
#include "cpp11/doubles.hpp"
#include <utility>

#define INT_FLOOR_MULTI_UNIT(x, n) ((x/n) * n)
#define INT_FLOOR_MULTI_UNIT1(x, n) (((x - 1)/n) * n)
inline double floor_multi_unit(double x, double n) noexcept {
  return std::floor(x/n) * n;
}
inline double floor_multi_unit1(double x, double n) noexcept {
  return std::floor((x-1)/n) * n;
}

#define INT_CEIL_MULTI_UNIT(x, n) ((x / n) * n + n)
#define INT_CEIL_MULTI_UNIT1(x, n) (((x - 1) / n) * n + n)
inline double ceil_multi_unit(double x, double n) noexcept {
  return std::floor(x/n) * n + n;
}
inline double ceil_multi_unit1(double x, double n) noexcept {
  return std::floor((x - 1)/n) * n + n;
}

enum class Unit {YEAR, HALFYEAR, QUARTER, SEASON, BIMONTH, MONTH,
  WEEK, DAY, HOUR, MINUTE, SECOND, ASECOND};

void check_fractional_unit(const double N, const char * unit_str) {
  int_fast64_t fN = floor_to_int64(N);
  if (N < 1 || N - fN > 1e-14)
    Rf_error("Rounding with %s units (%.2f) is not supported", unit_str, N);
}

inline double month_unit(const Unit unit, double N) {
  if (unit == Unit::HALFYEAR) N *= 6;
  else if (unit == Unit::BIMONTH) N *= 2;
  else if (unit == Unit::SEASON) N *= 3;
  else if (unit == Unit::QUARTER) N *= 3;
  return N;
}

std::pair<Unit,double> adjust_rounding_unit(const Unit unit, double N) {
  switch (unit) {
   case Unit::ASECOND:
     return std::make_pair(Unit::ASECOND, N);
   case Unit::SECOND:
     if (N > 60) Rf_error("Rounding unit for seconds larger than 60");
     return std::make_pair(Unit::SECOND, N);
   case Unit::MINUTE:
     if (N < 1) return std::make_pair(Unit::SECOND, N * 60);
     if (N > 60) Rf_error("Rounding unit for minutes larger than 60");
     check_fractional_unit(N, "fractional multi-minute");
     return std::make_pair(Unit::MINUTE, N);
   case Unit::HOUR:
     if (N < 1) return std::make_pair(Unit::MINUTE, N * 60);
     if (N > 24) Rf_error("Rounding unit for hours larger than 24");
     check_fractional_unit(N, "fractional multi-hour");
     return std::make_pair(Unit::HOUR, N);
   case Unit::DAY:
     if (N < 1) return std::make_pair(Unit::HOUR, N * 24);
     if (N > 31) Rf_error("Rounding unit for days larger than 31");
     check_fractional_unit(N, "fractional multi-day");
     return std::make_pair(Unit::DAY, N);
   case Unit::SEASON:
     if (N != 1)
       Rf_error("Rounding with fractional or multi-unit seasons not supporeted");
   case Unit::HALFYEAR:
   case Unit::BIMONTH:
   case Unit::QUARTER:
   case Unit::MONTH:
     N = month_unit(unit, N);
     check_fractional_unit(N, "fractional months");
     if (N > 12)
       Rf_error("Resulting rounding number of months (%.2f) larger than 12", N);
     break;
   case Unit::YEAR:
     check_fractional_unit(N, "fractional years");
     break;
   case Unit::WEEK:
     if (N != 1)
       Rf_error("Rounding with multi-week or fractional weeks is not supported");
     break;
  }

  return std::make_pair(unit, N);
}

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
                             const cctz::civil_second& cs_orig,
                             const double rem = 0.0) noexcept {
  cctz::time_zone::civil_lookup cl = tz.lookup(ct);
  return civil_lookup_to_posix(
    cl, tz, tp_orig, cs_orig, DST(RollDST::BOUNDARY, RollDST::POST), rem);
}

// used for time_ceiling exclusively
template<typename T>
inline double ct2posix4ceiling(const T& ct,
                               const cctz::time_zone& tz,
                               const time_point& tp_orig,
                               const cctz::civil_second& cs_orig,
                               const int_fast64_t N,
                               const bool check_boundary,
                               const double rem = 0.0) noexcept {
  if (check_boundary && rem == 0 && cs_orig == ct - N) {
    time_point tpnew = cctz::convert(cs_orig, tz);
    return tpnew.time_since_epoch().count();
  } else {
    cctz::time_zone::civil_lookup cl = tz.lookup(ct);
    return civil_lookup_to_posix(
      cl, tz, tp_orig, cs_orig, DST(RollDST::BOUNDARY, RollDST::PRE), rem);
  }
}

[[cpp11::register]]
cpp11::writable::doubles C_time_ceiling(const cpp11::doubles dt,
                                        const std::string unit_name,
                                        const double nunits,
                                        const int week_start,
                                        const bool change_on_boundary) {

  Unit unit = name2unit(unit_name);
  std::string tz_name = tz_from_tzone_attr(dt);
  cctz::time_zone tz;
  load_tz_or_fail(tz_name, tz, "CCTZ: Invalid timezone of the input vector: \"%s\"");

  size_t n = dt.size();
  cpp11::writable::doubles out(n);
  init_posixct(out, tz_name.c_str());

  auto UN = adjust_rounding_unit(unit, nunits);
  double N = UN.second;
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

    switch(UN.first) {
     case Unit::ASECOND : {
       // aseconds are duration in seconds: fractional nunits and nunits > 60 are supported
       double posix = ceil_multi_unit(dsecs, nunits);
       if (check_boundary && (posix - nunits) == dsecs)
         out[i] = dsecs;
       else
         out[i] = posix;
       break;
     }
     case Unit::SECOND : {
       double ds = ceil_multi_unit(cs.second() + rem, N); // double sec
       if (ds > 60) {
         int_fast64_t full = INT_FLOOR_MULTI_UNIT(static_cast<int_fast64_t>(ds), 60);
         ds = ceil_multi_unit(ds - full, N);
         cs = cs + full;
       }
       int_fast64_t is = static_cast<int_fast64_t>(ds); // int sec
       rem = ds - is;
       cctz::civil_second ct = cctz::civil_second(cctz::civil_minute(cs)) + is;
       out[i] = ct2posix4ceiling(ct, tz, tp, cs, N, check_boundary, rem);
       break;
     }
     case Unit::MINUTE : {
       double dm = ceil_multi_unit(cs.minute(), N);
       if (dm > 60) {
         int_fast64_t full = INT_FLOOR_MULTI_UNIT(static_cast<int_fast64_t>(dm), 60);
         dm = ceil_multi_unit(dm - full, N);
         cs = cs + full*60;
       }
       cctz::civil_minute ct = cctz::civil_minute(cctz::civil_hour(cs)) + dm;
       out[i] = ct2posix4ceiling(ct, tz, tp, cs, N, check_boundary);
       break;
     }
     case Unit::HOUR : {
       cctz::civil_hour ct = cctz::civil_hour(cctz::civil_day(cs)) + ceil_multi_unit(cs.hour(), N);
       if (ct.hour() > 0 &&
           (ct.day() != cs.day() || ct.month() != cs.month() || ct.year() != cs.year())) {
         ct = ct + (ceil_multi_unit(ct.hour(), N) - ct.hour());
       }
       out[i] = ct2posix4ceiling(ct, tz, tp, cs, N, check_boundary);
       break;
     }
     case Unit::DAY : {
       cctz::civil_day ct = cctz::civil_day(cctz::civil_month(cs)) + ceil_multi_unit1(cs.day(), N);
       if (ct.day() > 1 && (ct.month() != cs.month() || ct.year() != cs.year())) {
         ct = ct + (ceil_multi_unit(ct.day(), N) - ct.day() + 1);
       }
       out[i] = ct2posix4ceiling(ct, tz, tp, cs, N, check_boundary);
       break;
     }
     case Unit::WEEK : {
       cctz::civil_day ct = cctz::next_weekday(cctz::civil_day(cs), wday);
       out[i] = ct2posix4ceiling(ct, tz, tp, cs, 7, check_boundary);
       break;
     }
     case Unit::SEASON : {
       cctz::civil_month ct = cctz::civil_month(cctz::civil_year(cs)) + (ceil_multi_unit(cs.month(), N) - 1);
       out[i] = ct2posix4ceiling(ct, tz, tp, cs, N, check_boundary); break;
     }
     case Unit::HALFYEAR :
     case Unit::QUARTER :
     case Unit::BIMONTH :
     case Unit::MONTH : {
       cctz::civil_month ct = cctz::civil_month(cctz::civil_year(cs)) + ceil_multi_unit1(cs.month(), N);
       if (ct.month() > 1 && ct.year() != cs.year()) {
         ct = ct + (ceil_multi_unit1(ct.month() , N) - ct.month() + 1);
       }
       out[i] = ct2posix4ceiling(ct, tz, tp, cs, N, check_boundary); break;
     }
     case Unit::YEAR : {
       cctz::civil_year ct = cctz::civil_year(ceil_multi_unit(cs.year(), N));
       out[i] = ct2posix4ceiling(ct, tz, tp, cs, N, check_boundary); break;
     }
    }
  }

  return out;
}


[[cpp11::register]]
cpp11::writable::doubles C_time_floor(const cpp11::doubles dt,
                                      const std::string unit_name,
                                      const double nunits,
                                      const int week_start) {

  Unit unit = name2unit(unit_name);
  std::string tz_name = tz_from_tzone_attr(dt);
  cctz::time_zone tz;
  load_tz_or_fail(tz_name, tz, "CCTZ: Invalid timezone of the input vector: \"%s\"");

  size_t n = dt.size();
  cpp11::writable::doubles out(n);
  init_posixct(out, tz_name.c_str());

  auto UN = adjust_rounding_unit(unit, nunits);
  double N = UN.second;
  cctz::weekday wday = static_cast<cctz::weekday>(week_start - 1);

  for (size_t i = 0; i < n; i++) {

    double dsecs = dt[i];
    int_fast64_t isecs = floor_to_int64(dsecs);
    if (isecs == NA_INT64) { out[i] = NA_REAL; continue; }
    sys_seconds ss(isecs);
    time_point tp(ss);
    cctz::civil_second cs = cctz::convert(tp, tz);

    switch(UN.first) {
     case Unit::ASECOND : {
       // absolute seconds: rounding from origin. No restrictions on units.
       out[i] = floor_multi_unit(dsecs, nunits);
       break;
     }
     case Unit::SECOND : {
       double rem = dsecs - isecs;
       double ds = floor_multi_unit(cs.second() + rem, N);
       int_fast64_t is = static_cast<int_fast64_t>(ds);
       rem = ds - is;
       /* Rprintf("dsec:%f isec%f rem:%f ds:%f is:%ld\n", dsecs, isecs, ds, is); */
       cctz::civil_second ct = cctz::civil_second(cctz::civil_minute(cs)) + is;
       out[i] = ct2posix4floor(ct, tz, tp, cs, rem);
       break;
     }
     case Unit::MINUTE : {
       cctz::civil_minute ct = cctz::civil_minute(cctz::civil_hour(cs)) + floor_multi_unit(cs.minute(), N);
       out[i] = ct2posix4floor(ct, tz, tp, cs); break;
     }
     case Unit::HOUR : {
       cctz::civil_hour ct = cctz::civil_hour(cctz::civil_day(cs)) + floor_multi_unit(cs.hour(), N);
       out[i] = ct2posix4floor(ct, tz, tp, cs); break;
     }
     case Unit::DAY : {
       cctz::civil_day ct = cctz::civil_day(cctz::civil_month(cs)) + floor_multi_unit1(cs.day(), N);
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
       cctz::civil_month ct = cctz::civil_month(cctz::civil_year(cs)) + floor_multi_unit(cs.month(), N) - 1;
       out[i] = ct2posix4floor(ct, tz, tp, cs); break;
     }
     case Unit::HALFYEAR :
     case Unit::QUARTER :
     case Unit::BIMONTH :
     case Unit::MONTH : {
       cctz::civil_month ct = cctz::civil_month(cctz::civil_year(cs)) + floor_multi_unit1(cs.month(), N);
       out[i] = ct2posix4floor(ct, tz, tp, cs); break;
     }
     case Unit::YEAR : {
       cctz::civil_year ct = cctz::civil_year(floor_multi_unit(cs.year(), N));
       out[i] = ct2posix4floor(ct, tz, tp, cs); break;
     }
    }
  }

  return out;
}
