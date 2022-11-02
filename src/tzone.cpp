
#include "tzone.h"
#include "cpp11/logicals.hpp"
namespace chrono = std::chrono;

const char* tz_from_R_tzone(SEXP tz) {
  if (Rf_isNull(tz)) {
    return "";
  } else {
    if (!Rf_isString(tz))
      Rf_error("'tz' is not a character vector");
    const char* tz0 = CHAR(STRING_ELT(tz, 0));
    if (strlen(tz0) == 0) {
      if (LENGTH(tz) > 1) {
        return CHAR(STRING_ELT(tz, 1));
      }
    }
    return tz0;
  }
}

const char* tz_from_tzone_attr(SEXP x){
  return tz_from_R_tzone(Rf_getAttrib(x, Rf_install("tzone")));
}

const char* system_tz() {
  auto sys_timezone = cpp11::package("base")["Sys.timezone"];
  SEXP sys_tz = STRING_ELT(sys_timezone(), 0);
  if (sys_tz == NA_STRING || strlen(CHAR(sys_tz)) == 0) {
    Rf_warning("System timezone name is unknown. Please set environment variable TZ. Using UTC.");
    return "UTC";
  } else {
    return CHAR(sys_tz);
  }
}

const char* local_tz() {
  // initialize once per session
  static const char* SYS_TZ = strdup(system_tz());
  const char* tz_env = std::getenv("TZ");
  if (tz_env == NULL) {
    return SYS_TZ;
  } else if (strlen(tz_env) == 0) {
    // If set but empty, R behaves in a system specific way and there is no way
    // to infer local time zone.
    Rf_warning("Environment variable TZ is set to \"\". Using system TZ.");
    return SYS_TZ;
  } else {
    return tz_env;
  }
}

bool load_tz(std::string tzstr, cctz::time_zone& tz) {
  // return `true` if loaded, else false
  if (tzstr.size() == 0) {
    // CCTZ doesn't work on windows https://github.com/google/cctz/issues/53
    /* std::cout << "Local TZ: " << local_tz() << std::endl; */
    return cctz::load_time_zone(local_tz(), &tz);
  } else {
    if (!cctz::load_time_zone(tzstr, &tz)) {
      auto el = TZMAP.find(tzstr);
      if (el != TZMAP.end()) {
        tz = cctz::fixed_time_zone(chrono::hours(el->second));
      } else {
        return false;
      }
    }
    return true;
  }
}

void load_tz_or_fail(std::string tzstr, cctz::time_zone& tz, std::string error_msg) {
  if (!load_tz(tzstr, tz)) {
    Rf_error(error_msg.c_str(), tzstr);
  }
}

[[cpp11::register]]
cpp11::strings C_local_tz() {
    return Rf_mkString(local_tz());
}

[[cpp11::register]]
bool C_valid_tz(const cpp11::strings tz_name) {
  cctz::time_zone tz;
  std::string tzstr(tz_name[0]);
  return load_tz(tzstr, tz);
}
