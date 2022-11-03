
// FIXME: all of the below is directly taken from lubridate (my own code) once
// parser package is out this should go.

#include <Rinternals.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdbool.h>

#define ALPHA(X) (((X) >= 'a' && (X) <= 'z') || ((X) >= 'A' && (X) <= 'Z'))
#define DIGIT(X) ((X) >= '0' && (X) <= '9')

static const int SECONDS_IN_ONE[] = {
    1, // second
    60, // minute
    3600, // hour
    86400, // day
    604800, // week
    31557600 // year
};

typedef struct {
  int  val;
  int unit;
} intUnit;

typedef struct {
  int  val;
  double fraction;
  int unit;
} fractionUnit;

static const char *EN_UNITS[] = {"S", "secs", "seconds",
                                 "M", "mins", "minutes",
                                 "H", "hours",  // 6
                                 "D", "days",   // 8
                                 "W", "weeks",  // 10
                                 "M", "months", // 12
                                 "Y", "years",  // 14
                                 // ISO period delimiters
                                 "M",           // 16
                                 "P",           // 17
                                 "T"            // 18
};
#define N_EN_UNITS 19

// S=0,  M=1, H=2, d=3, w=4, m=5, y=6
static const char *PERIOD_UNITS[] = {"seconds", "minutes", "hours",
                                     "days", "weeks", "months", "years"};
#define N_PERIOD_UNITS 7

// Find maximal partial match in `strings`.
//
// Increment *c and return index in 0..(length(strings)-1) if match was found,
// -1 if not. Matching starts from *c, with all non-alpha-numeric characters
// pre-skipped.
//
// - *c: pointer to a character in a C string (incremented by side effect)
// - *stings: pointer to an array of C strings to be matched to
// - strings_len: length of strings array
int parse_alphanum(const char **c, const char **strings, const int strings_len, const char ignore_case){

  // tracking array: all valid objects are marked with 1, invalid with 0
  int track[strings_len];
  for (int i = 0; i < strings_len; i++){
    track[i] = 1;
  }

  int j = 0, out = -1, good_tracks = strings_len;
  while (**c && !ALPHA(**c) && !DIGIT(**c)) (*c)++;

  while (**c && good_tracks) {
    // stop when all tracks have been exhausted
    for (int i = 0; i < strings_len; i++){

      // keep going while at least one valid track
      if (track[i]){

        if (strings[i][j]) {
          if (**c == strings[i][j] || (ignore_case && (tolower(**c) == strings[i][j]))) {
            out = i;
          } else { // invalidate track i if not matching
            track[i] = 0;
            good_tracks--;
          }
        } else { // reached to the end of string i; return it if the last track
          good_tracks--;
          out = i;
        }
      }
    }
    if (good_tracks) {
      (*c)++;
      j++;
    }
  }
  return out;
}

// parse fractional part
double parse_fractional(const char **c) {
  double out = 0.0, factor = 0.1;
  while (DIGIT(**c)) { out = out + (**c - '0')*factor; factor *= 0.1; (*c)++; }
  return out;
}

/* parse N digit characters from **c. Return parsed non-negative integer. If
   failed to pass N chars, return -1.*/
int parse_int (const char **c, const int N, const int strict) {
  int tN = N, X = 0;
  while (DIGIT(**c) && tN > 0) {
    X = X * 10 + (**c - '0');
    (*c)++;
    tN--;
  }
  if (strict && tN > 0) return -1; // not all numbers have been consumed
  else if (tN == N) return -1; // no parsing happened
  else return X;
}

fractionUnit parse_period_unit (const char **c) {
  // units: invalid=-1, S=0,  M=1, H=2, d=3, w=4, m=5, y=6
  // SKIP_NON_ALPHANUMS(*c);   //  why this macro doesn't work here?
  while(**c && !(ALPHA(**c) || DIGIT(**c) || **c == '.')) (*c)++;;

  fractionUnit out;
  out.unit = -1;
  out.val = parse_int(c, 100, FALSE);
  if (**c == '.') {
    (*c)++;
    // allow fractions without leading 0
    if (out.val == -1)
      out.val = 0;
    out.fraction = parse_fractional(c);
  } else {
    out.fraction = 0.0;
  }

  if(**c){
    out.unit = parse_alphanum(c, EN_UNITS, N_EN_UNITS, 0);
    if (out.unit < 0 || out.unit > 16) {
      return out;
    } else {
      // if only unit name supplied, default to 1 units
      if(out.val == -1)
        out.val = 1;
      if (out.unit < 3) out.unit = 0;      // seconds
      else if (out.unit < 6) out.unit = 1; // minutes
      else if (out.unit < 16) out.unit = (out.unit - 6)/2 + 2;
      return out;
    }
  } else {
    return out;
  }
}


void parse_period_1 (const char **c, double ret[N_PERIOD_UNITS], bool frac_to_second){
  int P = 0; // ISO period flag
  int parsed1 = 0;
  while (**c) {
    fractionUnit fu = parse_period_unit(c);
    /* Rprintf("P:%d UNIT:%d\n", P, fu.unit); */
    if (fu.unit >= 0) {
      if (fu.unit == 17) { // ISO P
        P = 1;
      } else if (fu.unit == 18) { // ISO T
        P = 0;
      } else {
        if (fu.unit == 16) { // month or minute
          fu.unit = P ? 5 : 1;
        }
        parsed1 = 1;
        ret[fu.unit] += fu.val;
        if (fu.fraction > 0) {
          if (frac_to_second) {
            if (fu.unit == 0) ret[fu.unit] += fu.fraction;
            else ret[0] += fu.fraction * SECONDS_IN_ONE[fu.unit];
          } else {
            ret[fu.unit] += fu.fraction;
          }
        }
      }
    } else {
      ret[0] = NA_REAL;
      break;
    }
  }
  if (!parsed1) {
    ret[0] = NA_REAL;
  }
}

SEXP period_names(void) {
  SEXP names = PROTECT(allocVector(STRSXP, N_PERIOD_UNITS));
  for (int i = 0; i < N_PERIOD_UNITS; i++) {
    SET_STRING_ELT(names, i, mkChar(PERIOD_UNITS[i]));
  }
  UNPROTECT(1);
  return names;
}

SEXP C_parse_period(SEXP str) {

  if (TYPEOF(str) != STRSXP) error("STR argument must be a character vector");

  int n = LENGTH(str);

  // store parsed units in a N_PERIOD_UNITS x n matrix
  SEXP out = PROTECT(allocMatrix(REALSXP, N_PERIOD_UNITS, n));
  double *data = REAL(out);

  for (int i = 0; i < n; i++) {
    const char *c = CHAR(STRING_ELT(str, i));
    double ret[N_PERIOD_UNITS] = {0};
    parse_period_1(&c, ret, false);
    int j = i * N_PERIOD_UNITS;
    for(int k = 0; k < N_PERIOD_UNITS; k++) {
      data[j + k] = ret[k];
    }
  }

  // Not adding names as mat[i, ] retains names when mat is a single column, thus
  // requiring additional pre-processing at R level

  /* SEXP dimnames = PROTECT(allocVector(VECSXP, 2)); */
  /* SET_VECTOR_ELT(dimnames, 0, period_names()); */
  /* SET_VECTOR_ELT(dimnames, 1, R_NilValue); */
  /* setAttrib(out, R_DimNamesSymbol, dimnames); */

  UNPROTECT(1);

  return out;
}
