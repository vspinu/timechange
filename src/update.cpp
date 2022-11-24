
#include "common.h"

#include "cpp11/doubles.hpp"
#include "cpp11/function.hpp"
#include "cpp11/integers.hpp"
#include "cpp11/strings.hpp"
#include "tzone.h"
#include "R_ext/Print.h"

// CIVIL TIME:
// https://github.com/google/cctz/blob/master/include/cctz/civil_time.h
// https://github.com/devjgm/papers/blob/master/d0215r1.md

// TIME ZONES:
// https://github.com/google/cctz/blob/master/include/cctz/time_zone.h
// https://github.com/devjgm/papers/blob/master/d0216r1.md
// https://raw.githubusercontent.com/devjgm/papers/master/resources/struct-civil_lookup.png
// https://en.wikipedia.org/wiki/Tz_database
// https://github.com/eggert/tz/blob/2018d/etcetera#L36-L42

// R's timezone registry:
// https://github.com/SurajGupta/r-source/blob/master/src/extra/tzone/registryTZ.c

// C++20 date/calendar proposal: https://github.com/HowardHinnant/date

[[cpp11::register]]
cpp11::writable::doubles C_time_update(const cpp11::doubles dt,
                                       const cpp11::list updates,
                                       const SEXP tz,
                                       const std::string roll_month,
                                       const cpp11::strings roll_dst,
                                       const int week_start = 1,
                                       const bool exact = false) {

  if (dt.empty())
    // FIXME: ignored tz argument
    return(posixct(tz_from_tzone_attr(dt)));

  RollMonth rmonth = parse_month_roll(roll_month);
  DST rdst(roll_dst);

  cpp11::integers year, month, yday, mday, wday, hour, minute;
  cpp11::doubles second;

  bool do_year = updates.contains("year"),
    do_month = updates.contains("month"),
    do_yday = updates.contains("yday"),
    do_mday = updates.contains("mday"),
    do_wday = updates.contains("wday"),
    do_hour = updates.contains("hour"),
    do_minute = updates.contains("minute"),
    do_second = updates.contains("second");

  if (do_year) year = to_integers(updates["year"]);
  if (do_month) month = to_integers(updates["month"]);
  if (do_yday) yday = to_integers(updates["yday"]);
  if (do_mday) mday = to_integers(updates["mday"]);
  if (do_wday) wday = to_integers(updates["wday"]);
  if (do_hour) hour = to_integers(updates["hour"]);
  if (do_minute) minute = to_integers(updates["minute"]);
  if (do_second) second = to_doubles(updates["second"]);

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

  // fixme: more informative message
  if (do_year && sizes[0] != 1 && !loop_year) Rf_error("time_update: Incompatible size of 'year' vector");
  if (do_month && sizes[1] != 1 && !loop_month) Rf_error("time_update: Incompatible size of 'month' vector");
  if (do_yday && sizes[2] != 1 && !loop_yday) Rf_error("time_update: Incompatible size of 'yday' vector");
  if (do_mday && sizes[3] != 1 && !loop_mday) Rf_error("time_update: Incompatible size of 'mday' vector");
  if (do_wday && sizes[4] != 1 && !loop_wday) Rf_error("time_update: Incompatible size of 'wday' vector");
  if (do_hour && sizes[5] != 1 && !loop_hour) Rf_error("time_update: Incompatible size of 'hour' vector");
  if (do_minute && sizes[6] != 1 && !loop_minute) Rf_error("time_update: Incompatible size of 'minute' vector");
  if (do_second && sizes[7] != 1 && !loop_second) Rf_error("time_update: Incompatible size of 'second' vector");

  if (dt.size() > 1 && !loop_dt)
    Rf_error("time_update: Incompatible length of time and update vectors");

  if (do_yday + do_mday + do_wday > 1)
    Rf_error("Conflicting days input, only one of yday, mday and wday must be supplied");

  if (do_yday && (do_month || do_mday))
    Rf_error("Setting `yday` in combination with `month` or `mday` is not supported");

  if (do_wday && (do_year || do_month || do_mday))
    Rf_error("Setting `yday` in combination with `year`, `month` or `mday` is not supported");

  if ((do_yday && do_wday))
    Rf_error("Setting both `yday` and `wday` is not supported");

  std::string tzfrom = tz_from_tzone_attr(dt);
  cctz::time_zone otzone;
  load_tz_or_fail(tzfrom, otzone, "CCTZ: Invalid timezone of the input vector: \"%s\"");

  std::string tzto;
  cctz::time_zone ntzone;
  if (Rf_isNull(tz)) {
    tzto = tzfrom;
  } else {
    tzto = tz_from_R_tzone(tz);
  }
  load_tz_or_fail(tzto, ntzone, "CCTZ: Unrecognized tzone: \"%s\"");

  cpp11::writable::doubles out(N);
  init_posixct(out, tzto.c_str());

  // all vectors are either size N or 1
  for (R_xlen_t i = 0; i < N; i++)
    {
      double dti = loop_dt ? dt[i] : dt[0];
      int_fast64_t secs = floor_to_int64(dti);

      if (ISNAN(dti) || secs == NA_INT64) {
        if (dti == R_PosInf)
          out[i] = R_PosInf;
        else if (dti == R_NegInf)
          out[i] = R_NegInf;
        else
          out[i] = NA_REAL;
        continue;
      }

      double rem = dti - secs;
      sys_seconds ss(secs);
      time_point otp(ss);
      cctz::civil_second ocs = cctz::convert(otp, otzone);

      int_fast64_t
        y = ocs.year(), m = ocs.month(), d = ocs.day(),
        H = ocs.hour(), M = ocs.minute(), S = ocs.second();

      /* Rprintf("dti: %f sec:%ld H:%d M:%d S:%d\n", dti, secs, H, M, S); */

      if (do_year) {
        y = loop_year ? year[i] : year[0];
        if (y == NA_INT32) { out[i] = NA_REAL; continue; }
      }

      if (do_month) {
        m = loop_month ? month[i] : month[0];
        if (m == NA_INT32) { out[i] = NA_REAL; continue; }
      }

      if (do_mday) {
        d = loop_mday ? mday[i] : mday[0];
        if (d == NA_INT32) { out[i] = NA_REAL; continue; }
      }

      cctz::civil_month cm = cctz::civil_month(y, m);

      if (rmonth == RollMonth::NAym) {
        // lubridate historical case of returning NA when intermediate month+year result
        // in a invalid date
        if (d != cctz::civil_day(y, m, d).day()) { out[i] = NA_REAL; continue; }
      }

      if (do_yday || do_wday) {
        cctz::civil_day cd = cctz::civil_day(y, m, d);
        d = cd.day();
        // - yda and wday are recycled
        // - yday and wday apply after y,m,d computaiton and is always valid
        if (do_yday) {
          // yday and d are 1 based
          d = d - cctz::get_yearday(cd);
          if (loop_yday) d += yday[i]; else d += yday[0];
        }
        if (do_wday) {
          // wday is 1 based and starts on week_start
          int cur_wday = (static_cast<int>(cctz::get_weekday(cd)) + 8 - week_start) % 7;
          d = d - cur_wday - 1;
          if (loop_wday) d += wday[i]; else d += wday[0];
        }
      }

      if (do_hour) {
        H = loop_hour ? hour[i] : hour[0];
        if (H == NA_INT32) { out[i] = NA_REAL; continue; }
      }

      if (do_minute) {
        M = loop_minute ? minute[i] : minute[0];
        if (M == NA_INT32) { out[i] = NA_REAL; continue; }
      }

      if (do_second) {
        double s = loop_second ? second[i] : second[0];
        if (ISNAN(s)) { out[i] = NA_REAL; continue; }
        S = floor_to_int64(s);
        if (S == NA_INT64) { out[i] = NA_REAL; continue; }
        rem = s - S;
      }

      cctz::civil_second ncs(y, m, d, H, M, S);

      if (exact) {

        bool invalid = false;
        if (do_yday || do_wday) {
          if (do_yday) {
            invalid = invalid ||
              (cctz::get_yearday(ncs) != (loop_yday ? yday[i] : yday[0]));
          }
          // FIXME: fix this logic
          /* if (do_wday) { */
          /*   int new_wday = static_cast<int>(cctz::get_weekday(ncs)) + 1; */
          /*   invalid = invalid || */
          /*     new_wday != (loop_yday ? wday[i] : wday[0]); */
          /* } */
        } else {
          invalid =
            ncs.year() != y || ncs.month() != m || ncs.day() != d ||
            ncs.hour() != H || ncs.minute() != M || ncs.second() != S;
        }

        if (invalid) {
          out[i] = NA_REAL;
          continue;
        }

      } else {

        // Month recycling - yda and wday are incompatible with year,month,mday units
        // and month recycling does not make sense
        if (!(do_yday || do_wday)) {

          // If day is not expected we roll. Rolling can be triggered by recycling
          // d,H,M,S units or by falling into a non-existing day by virtue of setting
          // year or month.
          if (ncs.day() != d && ncs.month() != cm.month()) {
            switch(rmonth) {
             case RollMonth::FULL: break;
             case RollMonth::NA:
               out[i] = NA_REAL;
               continue;
             case RollMonth::NAym: break;
             case RollMonth::BOUNDARY: {
               cctz::civil_day cd = cctz::civil_day(cctz::civil_month(ncs));
               ncs = cctz::civil_second(cd.year(), cd.month(), cd.day(), 0, 0, 0);
               rem = 0.0;
               break;
             }
             case RollMonth::POSTDAY:
               ncs = cctz::civil_second(ncs.year(), ncs.month(), 1, ncs.hour(), ncs.minute(), ncs.second());
               break;
             case RollMonth::PREDAY: {
              cctz::civil_day cd = cctz::civil_day(cctz::civil_month(ncs)) - 1;
               ncs = cctz::civil_second(cd.year(), cd.month(), cd.day(), ncs.hour(), ncs.minute(), ncs.second());
               break;
             }
            }
          }

        }

      }

      const cctz::time_zone::civil_lookup ncl = ntzone.lookup(ncs);
      out[i] = civil_lookup_to_posix(ncl, otzone, otp, ocs, rdst, rem);
    }


  return out;
}

[[cpp11::register]]
cpp11::writable::doubles C_time_add(const cpp11::doubles& dt,
                                    const cpp11::list periods,
                                    const std::string roll_month,
                                    const cpp11::strings roll_dst) {
  if (dt.size() == 0)
    // FIXME: ignored tz argument
    return(posixct(tz_from_tzone_attr(dt)));

  RollMonth rmonth = parse_month_roll(roll_month);
  DST rdst(roll_dst);

  bool
    do_year = periods.contains("year"),
    do_month = periods.contains("month"),
    do_day = periods.contains("day"),
    do_week = periods.contains("week"),
    do_hour = periods.contains("hour"),
    do_minute = periods.contains("minute"),
    do_second = periods.contains("second");

  cpp11::integers year, month, week, day, hour, minute;
  cpp11::doubles second;

  if (do_year) year = to_integers(periods["year"]);
  if (do_month) month = to_integers(periods["month"]);
  if (do_week) week = to_integers(periods["week"]);
  if (do_day) day = to_integers(periods["day"]);
  if (do_hour) hour = to_integers(periods["hour"]);
  if (do_minute) minute = to_integers(periods["minute"]);
  if (do_second) second = to_doubles(periods["second"]);

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

  // fixme: provide vec size info in the message
  if (do_year && year.size() != 1 && !loop_year) Rf_error("time_add: Incompatible size of 'year' vector");
  if (do_month && month.size() != 1 && !loop_month) Rf_error("time_add: Incompatible size of 'month' vector");
  if (do_week && week.size() != 1 && !loop_week) Rf_error("time_add: Incompatible size of 'week' vector");
  if (do_day && day.size() != 1 && !loop_day) Rf_error("time_add: Incompatible size of 'day' or 'mday' vector");
  if (do_hour && hour.size() != 1 && !loop_hour) Rf_error("time_add: Incompatible size of 'hour' vector");
  if (do_minute && minute.size() != 1 && !loop_minute) Rf_error("time_add: Incompatible size of 'minute' vector");
  if (do_second && second.size() != 1 && !loop_second) Rf_error("time_add: Incompatible size of 'second' vector");

  if (dt.size() > 1 && !loop_dt)
    Rf_error("time_add: length of datetime vector must be 1 or match the length of updating vectors");

  std::string tz_name = tz_from_tzone_attr(dt);
  cctz::time_zone tz;
  load_tz_or_fail(tz_name, tz, "CCTZ: Invalid timezone of the input vector: \"%s\"");

  cpp11::writable::doubles out(N);
  init_posixct(out, tz_name.c_str());

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
        if (dti == R_PosInf)
          out[i] = R_PosInf;
        else if (dti == R_NegInf)
          out[i] = R_NegInf;
        else
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
         case RollMonth::FULL: break;
         case RollMonth::PREDAY:
           cd = cctz::civil_day(cctz::civil_month(cd)) - 1;
           break;
         case RollMonth::BOUNDARY:
           cd = cctz::civil_day(cctz::civil_month(cd));
           add_my_hms = false;
           break;
         case RollMonth::POSTDAY:
           cd = cctz::civil_day(cctz::civil_month(cd));
           break;
         case RollMonth::NA:
           out[i] = NA_REAL;
           continue;
         case RollMonth::NAym: break;
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

  return out;
}

[[cpp11::register]]
cpp11::writable::doubles C_force_tz(const cpp11::doubles dt,
                                    const cpp11::strings tz,
                                    const cpp11::strings roll_dst) {
  // roll: logical, if `true`, and `time` falls into the DST-break, assume the
  // next valid civil time, otherwise return NA

  DST rdst(roll_dst);

  if (tz.size() != 1)
    Rf_error("`tz` argument must be a single character string");

  std::string tzfrom_name = tz_from_tzone_attr(dt);
  std::string tzto_name(tz[0]);
  cctz::time_zone tzfrom, tzto;
  load_tz_or_fail(tzfrom_name, tzfrom, "CCTZ: Unrecognized timezone of the input vector: \"%s\"");
  load_tz_or_fail(tzto_name, tzto, "CCTZ: Unrecognized output timezone: \"%s\"");

  /* std::cout << "TZ from:" << tzfrom.name() << std::endl; */
  /* std::cout << "TZ to:" << tzto.name() << std::endl; */

  size_t n = dt.size();
  cpp11::writable::doubles out(n);
  init_posixct(out, tzto_name.c_str());

  for (size_t i = 0; i < n; i++)
    {
      int_fast64_t secs = floor_to_int64(dt[i]);
      /* printf("na: %i na64: %+" PRIiFAST64 "  secs: %+" PRIiFAST64 "  dt: %f\n", NA_INTEGER, INT_FAST64_MIN, secs, dt[i]); */
      if (secs == NA_INT64) {out[i] = NA_REAL; continue; }
      double rem = dt[i] - secs;
      sys_seconds secsfrom(secs);
      time_point tpfrom(secsfrom);
      cctz::civil_second ct1 = cctz::convert(tpfrom, tzfrom);
      const cctz::time_zone::civil_lookup cl2 = tzto.lookup(ct1);
      out[i] = civil_lookup_to_posix(cl2, tzfrom, tpfrom, ct1, rdst, rem);
    }

    return out;
}


[[cpp11::register]]
cpp11::writable::doubles C_force_tzs(const cpp11::doubles dt,
                                     const cpp11::strings tzs,
                                     const cpp11::strings tz_out,
                                     const cpp11::strings roll_dst) {
  // roll: logical, if `true`, and `time` falls into the DST-break, assume the
  // next valid civil time, otherwise return NA

  DST rdst(roll_dst);

  if (tz_out.size() != 1)
    Rf_error("In 'tzout' argument must be of length 1");

  if (tzs.size() != dt.size())
    Rf_error("In 'C_force_tzs' tzs and dt arguments must be of the same length");

  std::string tzfrom_name = tz_from_tzone_attr(dt);
  std::string tzout_name(tz_out[0]);

  cctz::time_zone tzfrom, tzto, tzout;
  load_tz_or_fail(tzfrom_name, tzfrom, "CCTZ: Unrecognized timezone of input vector: \"%s\"");
  load_tz_or_fail(tzout_name, tzout, "CCTZ: Unrecognized timezone: \"%s\"");

  std::string tzto_old_name("not-a-tz");
  size_t n = dt.size();
  cpp11::writable::doubles out(n);
  init_posixct(out, tzout_name.c_str());

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

  return out;
}

[[cpp11::register]]
cpp11::writable::doubles C_local_clock(const cpp11::doubles dt,
                                       const cpp11::strings tzs) {

  if (tzs.size() != dt.size())
    Rf_error("`tzs` and `dt` arguments must be of the same length");

  std::string tzfrom_name = tz_from_tzone_attr(dt);
  std::string tzto_old_name("not-a-tz");
  cctz::time_zone tzto;

  size_t n = dt.size();
  cpp11::writable::doubles out(n);

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
