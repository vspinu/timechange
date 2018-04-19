
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


// [[Rcpp::export]]
Rcpp::newDatetimeVector C_time_update(const Rcpp::NumericVector& dt,
                                      const Rcpp::List& updates,
                                      const SEXP tz,
                                      const std::string roll_month,
                                      const std::string roll_dst,
                                      const int week_start = 1) {

  Roll rmonth = roll_type(roll_month);
  Roll rdst = roll_type(roll_dst);

  bool do_year = updates.containsElementNamed("year"),
    do_month = updates.containsElementNamed("month"),
    do_yday = updates.containsElementNamed("yday"),
    do_mday = updates.containsElementNamed("mday"),
    do_wday = updates.containsElementNamed("wday"),
    do_hour = updates.containsElementNamed("hour"),
    do_minute = updates.containsElementNamed("minute"),
    do_second = updates.containsElementNamed("second");

  /* bool do_year = !Rf_isNull(updates["year"]), do_month = !Rf_isNull(updates["month"]), */
  /*   do_yday = !Rf_isNull(updates["yday"]), do_mday = !Rf_isNull(updates["mday"]), */
  /*   do_wday = !Rf_isNull(updates["wday"]), do_hour = !Rf_isNull(updates["hour"]), */
  /*   do_minute = !Rf_isNull(updates["minute"]), do_second = !Rf_isNull(updates["second"]); */

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

      double rem = dti - secs;
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

      if (do_mday) {
        d = loop_mday ? mday[i] : mday[0];
        if (d == NA_INT32) {out[i] = NA_REAL; continue; }
      }

      cctz::civil_day cd = cctz::civil_day(y, m, d);
      if (cd.day() != d) {
        // month rolling kicks in
        switch(rmonth) {
         case Roll::SKIP: break;
         case Roll::BOUNDARY:
           cd = cctz::civil_day(cctz::civil_month(cd));
           H = 0; M = 0; S = 0; rem = 0.0;
           break;
         case Roll::FIRST:
           cd = cctz::civil_day(cctz::civil_month(cd));
           break;
         case Roll::LAST:
           cd = cctz::civil_day(cctz::civil_month(cd)) - 1;
           break;
         case Roll::NA:
           out[i] = NA_REAL;
           continue;
        }
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
        double s = loop_second ? second[i] : second[0];
        if (ISNAN(s)) { out[i] = NA_REAL; continue; }
        S = floor_to_int64(s);
        if (S == NA_INT64) { out[i] = NA_REAL; continue; }
        rem = s - S;
      }

      const cctz::civil_second cs2(y, m, d, H, M, S);
      const cctz::time_zone::civil_lookup cl2 = tzone2.lookup(cs2);

      out[i] = civil_lookup_to_posix(cl2, tzone1, tp1, ct1, rdst, rem);

    }

  return newDatetimeVector(out, tzto.c_str());
}

// [[Rcpp::export]]
Rcpp::newDatetimeVector C_time_add(const Rcpp::NumericVector& dt,
                                   const Rcpp::List& periods,
                                   const std::string roll_month,
                                   const std::string roll_dst) {

  Roll rmonth = roll_type(roll_month);
  Roll rdst = roll_type(roll_dst);

  bool do_year = periods.containsElementNamed("years"),
    do_month = periods.containsElementNamed("months"),
    do_day = periods.containsElementNamed("days"),
    do_week = periods.containsElementNamed("weeks"),
    do_hour = periods.containsElementNamed("hours"),
    do_minute = periods.containsElementNamed("minutes"),
    do_second = periods.containsElementNamed("seconds");

  const IntegerVector& year = do_year ? periods["years"] : IntegerVector::create(0);
  const IntegerVector& month = do_month ? periods["months"] : IntegerVector::create(0);
  const IntegerVector& week = do_week ? periods["weeks"] : IntegerVector::create(0);
  const IntegerVector& day = do_day ? periods["days"] : IntegerVector::create(0);
  const IntegerVector& hour = do_hour ? periods["hours"] : IntegerVector::create(0);
  const IntegerVector& minute = do_minute ? periods["minutes"] : IntegerVector::create(0);
  const NumericVector& second = do_second ? periods["seconds"] : NumericVector::create(0);

  if (dt.size() == 0) return(newDatetimeVector(dt));

  std::vector<R_xlen_t> sizes {
    year.size(), month.size(), week.size(), day.size(),
    hour.size(), minute.size(), second.size()
  };

  // tz is always there, so the output is at least length 1
  R_xlen_t N = std::max(*std::max_element(sizes.begin(), sizes.end()), dt.size());

  bool loop_year = year.size() == N, loop_month = month.size() == N,
    loop_week = week.size() == N, loop_day = day.size() == N,
    loop_hour = hour.size() == N, loop_minute = minute.size() == N,
    loop_second = second.size() == N, loop_dt = dt.size() == N;

  if (year.size() > 1 && !loop_year) stop("C_update_dt: Invalid size of 'year' vector");
  if (month.size() > 1 && !loop_month) stop("C_update_dt: Invalid size of 'month' vector");
  if (week.size() > 1 && !loop_week) stop("C_update_dt: Invalid size of 'week' vector");
  if (day.size() > 1 && !loop_day) stop("C_update_dt: Invalid size of 'day' vector");
  if (hour.size() > 1 && !loop_hour) stop("C_update_dt: Invalid size of 'hour' vector");
  if (minute.size() > 1 && !loop_minute) stop("C_update_dt: Invalid size of 'minute' vector");
  if (second.size() > 1 && !loop_second) stop("C_update_dt: Invalid size of 'second' vector");

  if (dt.size() > 1 && !loop_dt)
    stop("C_update_dt: length of dt vector must be 1 or match the length of updating vectors");

  std::string tz_name = tz_from_tzone_attr(dt);
  cctz::time_zone tz;
  load_tz_or_fail(tz_name, tz, "CCTZ: Invalid timezone of the input vector: \"%s\"");

  NumericVector out(N);

  int y = 0, m = 0, w = 0, d = 0, H = 0, M = 0;
  double s = 0.0;
  int_fast64_t S = 0;

  cctz::civil_year cy;
  cctz::civil_month cm;
  cctz::civil_day cd;
  cctz::civil_hour cH;
  cctz::civil_minute cM;
  cctz::civil_second cS;


  // all vectors are either size N or 1
  for (R_xlen_t i = 0; i < N; i++)
    {
      double dti = loop_dt ? dt[i] : dt[0];
      int_fast64_t secs = floor_to_int64(dti);

      if (ISNAN(dti) || secs == NA_INT64) {
        out[i] = NA_REAL;
        continue;
      }

      bool add_my_hms = true;
      double rem = dti - secs;
      sys_seconds ss(secs);
      time_point tp(ss);
      cctz::civil_second cs = cctz::convert(tp, tz);

      int_fast64_t
        ty = cs.year(), tm = cs.month(), td = cs.day(),
        tH = cs.hour(), tM = cs.minute(), tS = cs.second();

      cy = cctz::civil_year(ty);

      if (do_year) {
        y = loop_year ? year[i] : year[0];
        if (y == NA_INT32) { out[i] = NA_REAL; continue; }
        cy += y;
      }
      cm = cctz::civil_month(cy) + (tm -1);
      if (do_month) {
        m = loop_month ? month[i] : month[0];
        if (m == NA_INT32) { out[i] = NA_REAL; continue; }
        cm += m;
      }
      cd = cctz::civil_day(cm) + (td - 1);
      if (cd.day() != td) {
        // month rolling kicks in
        switch(rmonth) {
         case Roll::SKIP: break;
         case Roll::BOUNDARY:
           cd = cctz::civil_day(cctz::civil_month(cd));
           add_my_hms = false;
           break;
         case Roll::FIRST:
           cd = cctz::civil_day(cctz::civil_month(cd));
           break;
         case Roll::LAST:
           cd = cctz::civil_day(cctz::civil_month(cd)) - 1;
           break;
         case Roll::NA:
           out[i] = NA_REAL;
           continue;
        }
      }
      if (do_week) {
        w = loop_week ? week[i] : week[0];
        if (w == NA_INT32) { out[i] = NA_REAL; continue; }
        cd += w * 7;
      }
      if (do_day) {
        d = loop_day ? day[i] : day[0];
        if (d == NA_INT32) { out[i] = NA_REAL; continue; }
        /* Rprintf("tm:%d m:%d cd.month():%d cd.day():%d td:%d d:%d\n", tm, m, cd.month(), cd.day(), td, d); */
        cd += d;
      }
      cH = cctz::civil_hour(cd);
      if (add_my_hms) cH += tH;
      if (do_hour) {
        H = loop_hour ? hour[i] : hour[0];
        if (H == NA_INT32) { out[i] = NA_REAL; continue; }
        cH += H;
      }
      cM = cctz::civil_minute(cH);
      if (add_my_hms) cM += tM;
      if (do_minute) {
        M = loop_minute ? minute[i] : minute[0];
        if (M == NA_INT32) { out[i] = NA_REAL; continue; }
        cM += M;
      }
      cS = cctz::civil_second(cM);
      if (add_my_hms) cS += tS;
      if (do_second) {
        s = loop_second ? second[i] : second[0];
        if (ISNAN(s)) { out[i] = NA_REAL; continue; }
        S = floor_to_int64(s);
        if (S == NA_INT64) { out[i] = NA_REAL; continue; }
        rem += s - S;
        cS += S;
      }

      s = civil_lookup_to_posix(tz.lookup(cS), rdst);
      out[i] = s + rem;

    }

  return newDatetimeVector(out, tz_name.c_str());
}

// [[Rcpp::export]]
newDatetimeVector C_force_tz(const NumericVector dt,
                             const CharacterVector tz,
                             const std::string roll_dst) {
  // roll: logical, if `true`, and `time` falls into the DST-break, assume the
  // next valid civil time, otherwise return NA

  Roll rdst = roll_type(roll_dst);

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
      out[i] = civil_lookup_to_posix(cl2, tzfrom, tp1, ct1, rdst, rem);
    }

  return newDatetimeVector(out, tzto_name.c_str());
}


// [[Rcpp::export]]
newDatetimeVector C_force_tzs(const NumericVector dt,
                              const CharacterVector tzs,
                              const CharacterVector tz_out,
                              const std::string roll_dst) {
  // roll: logical, if `true`, and `time` falls into the DST-break, assume the
  // next valid civil time, otherwise return NA

  Roll rdst = roll_type(roll_dst);

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
      out[i] = civil_lookup_to_posix(clto, tzfrom, tpfrom, csfrom, rdst, rem);

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
