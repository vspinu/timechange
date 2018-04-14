

#ifndef TIMECHANGE_TZONE_H
#define TIMECHANGE_TZONE_H

#include <unordered_map>
#include "time_zone.h"
#include <Rcpp.h>

namespace chrono = std::chrono;
using sys_seconds = chrono::duration<int_fast64_t>;
using time_point = chrono::time_point<std::chrono::system_clock, sys_seconds>;


const std::unordered_map<std::string, int> TZMAP {
  {"CEST", 2}, {"CET", 1}, {"EDT", -4}, {"EEST", 3}, {"EET", 2}, {"EST", -5},
  {"PDT", -7}, {"PST", -8}, {"WEST", 1}, {"WET", 0}
};

const char* tz_from_R_tzone(SEXP tz);
const char* tz_from_tzone_attr(SEXP x);
const char* system_tz();
const char* local_tz();
bool load_tz(std::string tzstr, cctz::time_zone& tz);
void load_tz_or_fail(std::string tzstr, cctz::time_zone& tz, std::string error_msg);

#endif // TIMECHANGE_TZONE_H
