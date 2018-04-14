#include <cstdint>
#include <limits>
#include "civil_time.h"
#include "tzone.h"
#include <Rcpp.h>
#include <string>

using namespace Rcpp;
using namespace std;

// CIVIL TIME:
// https://github.com/google/cctz/blob/master/include/cctz/civil_time.h
// https://github.com/devjgm/papers/blob/master/d0215r1.md

// TIME ZONES:
// https://github.com/google/cctz/blob/master/include/cctz/time_zone.h
// https://github.com/devjgm/papers/blob/master/d0216r1.md
// https://raw.githubusercontent.com/devjgm/papers/master/resources/struct-civil_lookup.png

// R's timezone registry:
// https://github.com/SurajGupta/r-source/blob/master/src/extra/tzone/registryTZ.c


/// floor seconds

int_fast64_t NA_INT32 = static_cast<int_fast64_t>(NA_INTEGER);
int_fast64_t NA_INT64 = std::numeric_limits<int_fast64_t>::min();
double fINT64_MAX = static_cast<double>(std::numeric_limits<int_fast64_t>::max());
double fINT64_MIN = static_cast<double>(std::numeric_limits<int_fast64_t>::min());

int_fast64_t floor_to_int64(double x) {
  // maybe fixme: no warning yet on integer overflow
  if (ISNAN(x))
    return NA_INT64;
  x = std::floor(x);
  if (x > fINT64_MAX || x <= fINT64_MIN) {
    return NA_INT64;
  }
  return static_cast<int_fast64_t>(x);
}

// Helper for conversion functions. Get seconds from civil_lookup, but relies on
// use original time pre/post time if cl_new falls in repeated interval.
double get_secs_from_civil_lookup(const cctz::time_zone::civil_lookup& cl_new, // new lookup
                                  const cctz::time_zone& tz_orig,              // original time zone
                                  const time_point& tp_orig,                   // original time point
                                  const cctz::civil_second& cs_orig,           // original time in secs
                                  bool roll, double remainder = 0.0) {

  time_point tp_new;

  if (cl_new.kind == cctz::time_zone::civil_lookup::UNIQUE) {
    // UNIQUE
    tp_new = cl_new.pre;
  } else if (cl_new.kind == cctz::time_zone::civil_lookup::SKIPPED) {
    // SKIPPED
    if (roll)
      tp_new = cl_new.trans;
    else {
      return NA_REAL;
    }
  } else {
    // REPEATED
    // match pre or post time of original time
    const cctz::time_zone::civil_lookup cl_old = tz_orig.lookup(cs_orig);
    if (tp_orig >= cl_old.trans){
      tp_new = cl_new.post;
    } else {
      tp_new = cl_new.pre;
    }
    /* Rcpp::Rcout << cctz::format("tp:%Y-%m-%d %H:%M:%S %z", tp1, tz1) << std::endl; */
    /* Rcpp::Rcout << cctz::format("pre:%Y-%m-%d %H:%M:%S %z", cl1.pre, tz1) << std::endl; */
    /* Rcpp::Rcout << cctz::format("trans:%Y-%m-%d %H:%M:%S %z", cl1.trans, tz1) << std::endl; */
    /* Rcpp::Rcout << cctz::format("post:%Y-%m-%d %H:%M:%S %z", cl1.post, tz1) << std::endl; */
  }

  return tp_new.time_since_epoch().count() + remainder;
}

// [[Rcpp::export]]
Rcpp::newDatetimeVector C_time_update(const Rcpp::NumericVector& dt,
                                      const Rcpp::List& updates,
                                      const SEXP tz = R_NilValue,
                                      const bool roll = false,
                                      const int week_start = 7) {

  bool do_year = !Rf_isNull(updates["year"]), do_month = !Rf_isNull(updates["month"]),
    do_yday = !Rf_isNull(updates["yday"]), do_mday = !Rf_isNull(updates["mday"]),
    do_wday = !Rf_isNull(updates["wday"]), do_hour = !Rf_isNull(updates["hour"]),
    do_minute = !Rf_isNull(updates["minute"]), do_second = !Rf_isNull(updates["second"]);

  const IntegerVector& year = do_year ? updates["year"] : IntegerVector::create(0);
  const IntegerVector& month = do_month ? updates["month"] : IntegerVector::create(0);
  const IntegerVector& yday = do_yday ? updates["yday"] : IntegerVector::create(0);
  const IntegerVector& mday = do_mday ? updates["mday"] : IntegerVector::create(0);
  const IntegerVector& wday = do_wday ? updates["wday"] : IntegerVector::create(0);
  const IntegerVector& hour = do_hour ? updates["hour"] : IntegerVector::create(0);
  const IntegerVector& minute = do_minute ? updates["minute"] : IntegerVector::create(0);
  const NumericVector& second = do_second ? updates["second"] : NumericVector::create(0);

  if (dt.size() == 0) return(newDatetimeVector(dt));

  std::vector<R_xlen_t> sizes {
    year.size(), month.size(), yday.size(), mday.size(),
    wday.size(), hour.size(), minute.size(), second.size()
  };

  // tz is always there, so the output is at least length 1
  R_xlen_t N = std::max(*std::max_element(sizes.begin(), sizes.end()), dt.size());

  bool loop_year = sizes[0] == N, loop_month = sizes[1] == N,
    loop_yday = sizes[2] == N, loop_mday = sizes[3] == N, loop_wday = sizes[4] == N,
    loop_hour = sizes[5] == N, loop_minute = sizes[6] == N, loop_second = sizes[7] == N,
    loop_dt = dt.size() == N;

  if (sizes[0] > 1 && !loop_year) stop("C_update_dt: Invalid size of 'year' vector");
  if (sizes[1] > 1 && !loop_month) stop("C_update_dt: Invalid size of 'month' vector");
  if (sizes[2] > 1 && !loop_yday) stop("C_update_dt: Invalid size of 'yday' vector");
  if (sizes[3] > 1 && !loop_mday) stop("C_update_dt: Invalid size of 'mday' vector");
  if (sizes[4] > 1 && !loop_wday) stop("C_update_dt: Invalid size of 'wday' vector");
  if (sizes[5] > 1 && !loop_hour) stop("C_update_dt: Invalid size of 'hour' vector");
  if (sizes[6] > 1 && !loop_minute) stop("C_update_dt: Invalid size of 'minute' vector");
  if (sizes[7] > 1 && !loop_second) stop("C_update_dt: Invalid size of 'second' vector");

  if (dt.size() > 1 && !loop_dt)
    stop("C_update_dt: length of dt vector must be 1 or match the length of updating vectors");

  if (do_yday + do_mday + do_wday > 1)
    stop("Conflicting days input, only one of yday, mday and wday must be supplied");

  std::string tzfrom = tz_from_tzone_attr(dt);
  cctz::time_zone tzone1;
  load_tz_or_fail(tzfrom, tzone1, "CCTZ: Invalid timezone of the input vector: \"%s\"");

  std::string tzto;
  cctz::time_zone tzone2;
  if (Rf_isNull(tz)) {
    tzto = tzfrom;
  } else {
    tzto = tz_from_R_tzone(tz);
  }
  load_tz_or_fail(tzto, tzone2, "CCTZ: Unrecognized tzone: \"%s\"");

  NumericVector out(N);

  // all vectors are either size N or 1
  for (R_xlen_t i = 0; i < N; i++)
    {
      double dti = loop_dt ? dt[i] : dt[0];
      int_fast64_t secs = floor_to_int64(dti);

      if (ISNAN(dti) || secs == NA_INT64) {
        out[i] = NA_REAL;
        continue;
      }

      double rem = 0.0;
      sys_seconds ss(secs);
      time_point tp1(ss);
      cctz::civil_second ct1 = cctz::convert(tp1, tzone1);

      int_fast64_t
        y = ct1.year(), m = ct1.month(), d = ct1.day(),
        H = ct1.hour(), M = ct1.minute(), S = ct1.second();

      if (do_year) {
        y = loop_year ? year[i] : year[0];
        if (y == NA_INT32) {out[i] = NA_REAL; continue; }
      }
      if (do_month) {
        m = loop_month ? month[i] : month[0];
        if (m == NA_INT32) {out[i] = NA_REAL; continue; }
      }
      if (do_mday) {
        d = loop_mday ? mday[i] : mday[0];
        if (d == NA_INT32) {out[i] = NA_REAL; continue; }
      }
      if (do_hour) {
        H = loop_hour ? hour[i] : hour[0];
        if (H == NA_INT32) {out[i] = NA_REAL; continue; }
      }
      if (do_minute) {
        M = loop_minute ? minute[i] : minute[0];
        if (M == NA_INT32) {out[i] = NA_REAL; continue; }
      }
      if (do_second) {
        if (loop_second) {
          S = floor_to_int64(second[i]);
          rem = second[i] - S;
        } else {
          S = floor_to_int64(second[0]);
          rem = second[0] - S;
        }
        if (S == NA_INT64) {out[i] = NA_REAL; continue; }
      }

      if (do_yday) {
        // yday and d are 1 based
        d = d - cctz::get_yearday(cctz::civil_day(ct1));
        if (loop_yday) d += yday[i]; else d += yday[0];
      }

      if (do_wday) {
        // wday is 1 based and starts on week_start
        int cur_wday = (static_cast<int>(cctz::get_weekday(cctz::civil_day(ct1))) + 8 - week_start) % 7;
        d = d - cur_wday - 1;
        if (loop_wday) d += wday[i]; else d += wday[0];
      }

      const cctz::civil_second cs2(y, m, d, H, M, S);
      const cctz::time_zone::civil_lookup cl2 = tzone2.lookup(cs2);

      out[i] = get_secs_from_civil_lookup(cl2, tzone1, tp1, ct1, roll, rem);

    }

  return newDatetimeVector(out, tzto.c_str());
}

// [[Rcpp::export]]
newDatetimeVector C_force_tz(const NumericVector dt,
                                   const CharacterVector tz,
                                   const bool roll = false) {
  // roll: logical, if `true`, and `time` falls into the DST-break, assume the
  // next valid civil time, otherwise return NA

  if (tz.size() != 1)
    stop("`tz` argument must be a single character string");

  std::string tzfrom_name = tz_from_tzone_attr(dt);
  std::string tzto_name(tz[0]);
  cctz::time_zone tzfrom, tzto;
  load_tz_or_fail(tzfrom_name, tzfrom, "CCTZ: Unrecognized timezone of the input vector: \"%s\"");
  load_tz_or_fail(tzto_name, tzto, "CCTZ: Unrecognized output timezone: \"%s\"");

  /* std::cout << "TZ from:" << tzfrom.name() << std::endl; */
  /* std::cout << "TZ to:" << tzto.name() << std::endl; */

  size_t n = dt.size();
  NumericVector out(n);

  for (size_t i = 0; i < n; i++)
    {
      int_fast64_t secs = floor_to_int64(dt[i]);
      /* printf("na: %i na64: %+" PRIiFAST64 "  secs: %+" PRIiFAST64 "  dt: %f\n", NA_INTEGER, INT_FAST64_MIN, secs, dt[i]); */
      if (secs == NA_INT64) {out[i] = NA_REAL; continue; }
      double rem = dt[i] - secs;
      sys_seconds d1(secs);
      time_point tp1(d1);
      cctz::civil_second ct1 = cctz::convert(tp1, tzfrom);
      const cctz::time_zone::civil_lookup cl2 = tzto.lookup(ct1);
      out[i] = get_secs_from_civil_lookup(cl2, tzfrom, tp1, ct1, roll, rem);
    }

  return newDatetimeVector(out, tzto_name.c_str());
}


// [[Rcpp::export]]
newDatetimeVector C_force_tzs(const NumericVector dt,
                                    const CharacterVector tzs,
                                    const CharacterVector tz_out,
                                    const bool roll = false) {
  // roll: logical, if `true`, and `time` falls into the DST-break, assume the
  // next valid civil time, otherwise return NA

  if (tz_out.size() != 1)
    stop("In 'tzout' argument must be of length 1");

  if (tzs.size() != dt.size())
    stop("In 'C_force_tzs' tzs and dt arguments must be of the same length");

  std::string tzfrom_name = tz_from_tzone_attr(dt);
  std::string tzout_name(tz_out[0]);

  cctz::time_zone tzfrom, tzto, tzout;
  load_tz_or_fail(tzfrom_name, tzfrom, "CCTZ: Unrecognized timezone of input vector: \"%s\"");
  load_tz_or_fail(tzout_name, tzout, "CCTZ: Unrecognized timezone: \"%s\"");

  std::string tzto_old_name("not-a-tz");
  size_t n = dt.size();
  NumericVector out(n);

  for (size_t i = 0; i < n; i++)
    {
      std::string tzto_name(tzs[i]);
      if (tzto_name != tzto_old_name) {
        load_tz_or_fail(tzto_name, tzto, "CCTZ: Unrecognized timezone: \"%s\"");
        tzto_old_name = tzto_name;
      }

      int_fast64_t secs = floor_to_int64(dt[i]);
      if (secs == NA_INT64) { out[i] = NA_REAL; continue; }
      double rem = dt[i] - secs;
      sys_seconds secsfrom(secs);
      time_point tpfrom(secsfrom);
      cctz::civil_second csfrom = cctz::convert(tpfrom, tzfrom);

      const cctz::time_zone::civil_lookup clto = tzto.lookup(csfrom);
      out[i] = get_secs_from_civil_lookup(clto, tzfrom, tpfrom, csfrom, roll, rem);

    }

  return newDatetimeVector(out, tzout_name.c_str());
}

// [[Rcpp::export]]
NumericVector C_local_clock(const NumericVector dt,
                            const CharacterVector tzs) {

  if (tzs.size() != dt.size())
    stop("`tzs` and `dt` arguments must be of the same length");

  std::string tzfrom_name = tz_from_tzone_attr(dt);
  std::string tzto_old_name("not-a-tz");
  cctz::time_zone tzto;

  size_t n = dt.size();
  NumericVector out(n);

  for (size_t i = 0; i < n; i++)
    {
      std::string tzto_name(tzs[i]);
      if (tzto_name != tzto_old_name) {
        load_tz_or_fail(tzto_name, tzto, "CCTZ: Unrecognized timezone: \"%s\"");
        tzto_old_name = tzto_name;
      }

      int_fast64_t secs = floor_to_int64(dt[i]);
      if (secs == NA_INT64) { out[i] = NA_REAL; continue; }
      double rem = dt[i] - secs;

      sys_seconds secsfrom(secs);
      time_point tpfrom(secsfrom);
      cctz::civil_second cs = cctz::convert(tpfrom, tzto);
      cctz::civil_second cs_floor = cctz::civil_second(cctz::civil_day(cs));
      out[i] = cs - cs_floor + rem;
    }

  return out;
}





#define FLOOR(x, n) ((x/n) * n)
#define FLOOR1(x, n) (((x - 1)/n) * n)
inline double floor_multi_unit(double x, double n) noexcept {
  return std::floor(x/n) * n;
}

#define CEIL_MULTI_UNIT(x, n) ((x / n) * n + n)
#define CEIL_MULTI_UNIT1(x, n) (((x - 1) / n) * n + n)
inline double ceil_multi_unit(double x, double n) noexcept {
  return std::floor(x/n) * n + n;
}

enum class Unit {YEAR, HALFYEAR, QUARTER, SEASON, BIMONTH, MONTH,
                 WEEK, DAY, HOUR, MINUTE, SECOND};

Unit name2unit(std::string unit_name) {
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
  Rf_error("Invalid unit_name (%s)", unit_name);
}

template<typename T>
inline double civil_time_to_posix(T ct, cctz::time_zone tz) noexcept {
  time_point tpnew = cctz::convert(ct, tz);
  return tpnew.time_since_epoch().count();
}

template<typename T>
inline double civil_time_to_posix(T ct, cctz::civil_second cs, cctz::time_zone tz, int N, bool check_boundary) noexcept {
  if (check_boundary && cs == ct - N) {
    time_point tpnew = cctz::convert(cs, tz);
    return tpnew.time_since_epoch().count();
  } else {
    time_point tpnew = cctz::convert(ct, tz);
    return tpnew.time_since_epoch().count();
  }
}

// [[Rcpp::export]]
newDatetimeVector C_time_ceiling(const NumericVector dt,
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
     case Unit::SECOND : {
       // seconds are special: fractional nunits and nunits > 60 are supported
       double posix = ceil_multi_unit(dsecs, nunits);
       /* Rprintf("dsecs:%f posix:%f\n", dsecs, posix); */
       if (check_boundary && (posix - nunits) == dsecs)
         out[i] = dsecs;
       else
         out[i] = posix;
       break;
     }
     case Unit::MINUTE : {
       cctz::civil_minute ct = cctz::civil_minute(cctz::civil_hour(cs)) + CEIL_MULTI_UNIT(cs.minute(), N);
       out[i] = civil_time_to_posix(ct, cs, tz, N, check_boundary); break;
     }
     case Unit::HOUR : {
       cctz::civil_hour ct = cctz::civil_hour(cctz::civil_day(cs)) + CEIL_MULTI_UNIT(cs.hour(), N);
       out[i] = civil_time_to_posix(ct, cs, tz, N, check_boundary); break;
     }
     case Unit::DAY : {
       /* Rprintf("day:%d ceil:%d\n", cs.day(), CEIL_MULTI_UNIT1(cs.day(), N)); */
       cctz::civil_day ct = cctz::civil_day(cctz::civil_month(cs)) + CEIL_MULTI_UNIT1(cs.day(), N);
       out[i] = civil_time_to_posix(ct, cs, tz, N, check_boundary); break;
     }
     case Unit::WEEK : {
       if (N != 1)
         Rf_warning("Multi-unit week ceiling is not supported; ignoring");
       cctz::civil_day ct = cctz::next_weekday(cctz::civil_day(cs), wday);
       out[i] = civil_time_to_posix(ct, cs, tz, 7, check_boundary); break;
     }
     case Unit::SEASON : {
       cctz::civil_month ct = cctz::civil_month(cctz::civil_year(cs)) + CEIL_MULTI_UNIT(cs.month(), N) - 1;
       out[i] = civil_time_to_posix(ct, cs, tz, N, check_boundary); break;
     }
     case Unit::HALFYEAR :
     case Unit::QUARTER :
     case Unit::BIMONTH :
     case Unit::MONTH : {
       cctz::civil_month ct = cctz::civil_month(cctz::civil_year(cs)) + CEIL_MULTI_UNIT1(cs.month(), N);
       out[i] = civil_time_to_posix(ct, cs, tz, N, check_boundary); break;
     }
     case Unit::YEAR : {
       cctz::civil_year ct = cctz::civil_year(CEIL_MULTI_UNIT(cs.year(), N));
       out[i] = civil_time_to_posix(ct, cs, tz, N, check_boundary); break;
     }
    }
  }

  return newDatetimeVector(out, tz_name.c_str());
}


// [[Rcpp::export]]
newDatetimeVector C_time_floor(const NumericVector dt,
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
     case Unit::SECOND : {
       // seconds are special: fractional nunits and nunits > 60 are supported
       out[i] = floor_multi_unit(dsecs, nunits); break;
     }
     case Unit::MINUTE : {
       cctz::civil_minute ct = cctz::civil_minute(cctz::civil_hour(cs)) + FLOOR(cs.minute(), N);
       out[i] = civil_time_to_posix(ct, tz); break;
     }
     case Unit::HOUR : {
       cctz::civil_hour ct = cctz::civil_hour(cctz::civil_day(cs)) + FLOOR(cs.hour(), N);
       out[i] = civil_time_to_posix(ct, tz); break;
     }
     case Unit::DAY : {
       cctz::civil_day ct = cctz::civil_day(cctz::civil_month(cs)) + FLOOR1(cs.day(), N);
       out[i] = civil_time_to_posix(ct, tz); break;
     }
     case Unit::WEEK : {
       if (N != 1)
         Rf_warning("Multi-unit week rounding is not supported; ignoring");
       // get the previous `wday` if `cs` is not already on `wday`
       cctz::civil_day ct = cctz::next_weekday(cctz::civil_day(cs), wday) - 7;
       out[i] = civil_time_to_posix(ct, tz); break;
     }
     case Unit::SEASON : {
       cctz::civil_month ct = cctz::civil_month(cctz::civil_year(cs)) + FLOOR(cs.month(), N) - 1;
       out[i] = civil_time_to_posix(ct, tz); break;
     }
     case Unit::HALFYEAR :
     case Unit::QUARTER :
     case Unit::BIMONTH :
     case Unit::MONTH : {
       cctz::civil_month ct = cctz::civil_month(cctz::civil_year(cs)) + FLOOR1(cs.month(), N);
       out[i] = civil_time_to_posix(ct, tz); break;
     }
     case Unit::YEAR : {
       cctz::civil_year ct = cctz::civil_year(FLOOR(cs.year(), N));
       out[i] = civil_time_to_posix(ct, tz); break;
     }
    }
  }

  return newDatetimeVector(out, tz_name.c_str());
}
