#include "xmlrpc_config.h"

#include <time.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <stdio.h>
#if MSVCRT
#include <windows.h>
#endif

#include "bool.h"

#include "xmlrpc-c/c_util.h"
#include "xmlrpc-c/base.h"
#include "xmlrpc-c/base_int.h"
#include "xmlrpc-c/string_int.h"
#include "xmlrpc-c/time_int.h"


/* Future work: the XMLRPC_TYPE_DATETIME xmlrpc_value should store the
   datetime as something computation-friendly, not as a string.  The
   XML-RPC XML parser should parse the string value and reject the XML if
   it isn't valid.

   But this file should remain the authority on datetimes, so the XML
   parser and builder should call on routines in here to do that.

   time_t won't work because it can't represent times before 1970 or
   after 2038.  We need to figure out something better.
*/

#if HAVE_REGEX
#include "regex.h"
#endif

#if MSVCRT

static const __int64 SECS_BETWEEN_EPOCHS = 11644473600;
static const __int64 SECS_TO_100NS = 10000000; /* 10^7 */


void
UnixTimeToFileTime(time_t     const t,
                   LPFILETIME const pft) {

    int64_t const ll =
        Int32x32To64(t, SECS_TO_100NS) + SECS_BETWEEN_EPOCHS * SECS_TO_100NS;

    pft->dwLowDateTime  = (DWORD)ll;
    pft->dwHighDateTime = (DWORD)(ll >> 32);
}



void
UnixTimeToSystemTime(time_t const t,
                     LPSYSTEMTIME const pst) {
    FILETIME ft;

    UnixTimeToFileTime(t, &ft);
    FileTimeToSystemTime(&ft, pst);
}



static void
UnixTimeFromFileTime(xmlrpc_env *  const envP,
                     LPFILETIME    const pft,
                     time_t *      const timeValueP) { 

    int64_t const WinEpoch100Ns =
        ((int64_t)pft->dwHighDateTime << 32) + pft->dwLowDateTime;
    int64_t const unixEpoch100Ns =
        WinEpoch100Ns - (SECS_BETWEEN_EPOCHS * SECS_TO_100NS);
    int64_t const unixEpochSeconds =
        unixEpoch100Ns / SECS_TO_100NS; 

    if ((time_t)unixEpochSeconds != unixEpochSeconds) {
        /* Value is too big for a time_t; fail. */
        xmlrpc_faultf(envP, "Does not indicate a valid date");
        *timeValueP = (time_t)(-1);
    } else
        *timeValueP = (time_t)unixEpochSeconds;
}



static void
UnixTimeFromSystemTime(xmlrpc_env * const envP,
                       LPSYSTEMTIME const pst,
                       time_t *     const timeValueP) {
    FILETIME filetime;

    SystemTimeToFileTime(pst, &filetime); 
    UnixTimeFromFileTime(envP, &filetime, timeValueP); 
}

#endif  /* MSVCRT */



static const char * const iso8601Regex =
  "^([0-9]{4})([0-9]{2})([0-9]{2})T"
  "([0-9]{2}):?([0-9]{2}):?([0-9]{2})\\.?([0-9]+)?$";



static void
validateDatetimeType(xmlrpc_env *         const envP,
                     const xmlrpc_value * const valueP) {

    if (valueP->_type != XMLRPC_TYPE_DATETIME) {
        xmlrpc_env_set_fault_formatted(
            envP, XMLRPC_TYPE_ERROR, "Value of type %s supplied where "
            "type %s was expected.", 
            xmlrpc_type_name(valueP->_type), 
            xmlrpc_type_name(XMLRPC_TYPE_DATETIME));
    }
}



void
xmlrpc_read_datetime_str(xmlrpc_env *         const envP,
                         const xmlrpc_value * const valueP,
                         const char **        const stringValueP) {
    
    validateDatetimeType(envP, valueP);
    if (!envP->fault_occurred) {
        const char * const contents = 
            XMLRPC_MEMBLOCK_CONTENTS(char, &valueP->_block);
        *stringValueP = strdup(contents);
        if (*stringValueP == NULL)
            xmlrpc_env_set_fault_formatted(
                envP, XMLRPC_INTERNAL_ERROR, "Unable to allocate space "
                "for datetime string");
    }
}



void
xmlrpc_read_datetime_str_old(xmlrpc_env *         const envP,
                             const xmlrpc_value * const valueP,
                             const char **        const stringValueP) {
    
    validateDatetimeType(envP, valueP);
    if (!envP->fault_occurred) {
        *stringValueP = XMLRPC_MEMBLOCK_CONTENTS(char, &valueP->_block);
    }
}



#if HAVE_REGEX

static unsigned int
digitStringValue(const char * const string,
                 regmatch_t   const match) {
/*----------------------------------------------------------------------------
   Return the numerical value of the decimal whole number substring of
   'string' identified by 'match'.  E.g. if 'string' is 'abc34d' and
   'match' says start at 3 and end at 5, we return 34.
-----------------------------------------------------------------------------*/
    unsigned int i;
    unsigned int accum;

    assert(match.rm_so >= 0);
    assert(match.rm_eo >= 0);

    for (i = match.rm_so, accum = 0; i < (unsigned)match.rm_eo; ++i) {
        accum *= 10;
        assert(isdigit(string[i]));
        accum += string[i] - '0';
    }
    return accum;
}
#endif  /* HAVE_REGEX */



#if HAVE_REGEX

static unsigned int
digitStringMillionths(const char * const string,
                      regmatch_t   const match) {
/*----------------------------------------------------------------------------
   Return the number of millionths represented by the digits after the
   decimal point in a decimal string, where thse digits are the substring
   of 'string' identified by 'match'.  E.g. if the substring is
   34, we return 340,000.
-----------------------------------------------------------------------------*/
    unsigned int i;
    unsigned int accum;

    assert(match.rm_so >= 0);
    assert(match.rm_eo >= 0);

    for (i = match.rm_so, accum = 0; i < (unsigned)match.rm_so+6; ++i) {
        accum *= 10;
        if (i < (unsigned)match.rm_eo) {
            assert(isdigit(string[i]));
            accum += string[i] - '0';
        }
    }
    return accum;
}
#endif /* HAVE_REGEX */



#if HAVE_REGEX
static void
parseDateNumbersRegex(xmlrpc_env *   const envP,
                      const char *   const datetimeString,
                      unsigned int * const YP,
                      unsigned int * const MP,
                      unsigned int * const DP,
                      unsigned int * const hP,
                      unsigned int * const mP,
                      unsigned int * const sP,
                      unsigned int * const uP) {


    int status;
    char errBuf[1024];
    regex_t re;

    status = regcomp(&re, iso8601Regex, REG_ICASE | REG_EXTENDED);
    if (status == 0) {
        regmatch_t matches[1024];
        int status;

        status = regexec(&re, datetimeString, ARRAY_SIZE(matches), matches, 0);

        if (status == 0) {
            assert(matches[0].rm_so != -1);  /* Match of whole regex */
            
            *YP = digitStringValue(datetimeString, matches[1]);
            *MP = digitStringValue(datetimeString, matches[2]);
            *DP = digitStringValue(datetimeString, matches[3]);
            *hP = digitStringValue(datetimeString, matches[4]);
            *mP = digitStringValue(datetimeString, matches[5]);
            *sP = digitStringValue(datetimeString, matches[6]);

            if (matches[7].rm_so == -1)
                *uP = 0;
            else
                *uP = digitStringMillionths(datetimeString, matches[7]);
        } else {
            regerror(status, &re, errBuf, sizeof(errBuf));
            xmlrpc_env_set_fault(envP, XMLRPC_PARSE_ERROR, errBuf);
        }

    } else {
        regerror(status, &re, errBuf, sizeof(errBuf));
        xmlrpc_faultf(envP, "internal regex error at %s:%d: '%s'",
                      __FILE__, __LINE__, errBuf);
    }
    regfree(&re);
}
#endif  /* HAVE_REGEX */



static __inline__ void
parseDateNumbersNoRegex(xmlrpc_env *   const envP,
                        const char *   const datetimeString,
                        unsigned int * const YP,
                        unsigned int * const MP,
                        unsigned int * const DP,
                        unsigned int * const hP,
                        unsigned int * const mP,
                        unsigned int * const sP,
                        unsigned int * const uP) {

    unsigned int const dtStrlen = strlen(datetimeString);

    char year[4+1];
    char month[2+1];
    char day[2+1];
    char hour[2+1];
    char minute[2+1];
    char second[2+1];

    if (dtStrlen < 17 || dtStrlen == 18 || dtStrlen > 24)
        xmlrpc_faultf(envP, "could not parse date, size incompatible: '%d'",
                      dtStrlen);
    else {
        year[0]   = datetimeString[ 0];
        year[1]   = datetimeString[ 1];
        year[2]   = datetimeString[ 2];
        year[3]   = datetimeString[ 3];
        year[4]   = '\0';

        month[0]  = datetimeString[ 4];
        month[1]  = datetimeString[ 5];
        month[2]  = '\0';

        day[0]    = datetimeString[ 6];
        day[1]    = datetimeString[ 7];
        day[2]    = '\0';

        assert(datetimeString[ 8] == 'T');

        hour[0]   = datetimeString[ 9];
        hour[1]   = datetimeString[10];
        hour[2]   = '\0';

        assert(datetimeString[11] == ':');

        minute[0] = datetimeString[12];
        minute[1] = datetimeString[13];
        minute[2] = '\0';

        assert(datetimeString[14] == ':');

        second[0] = datetimeString[15];
        second[1] = datetimeString[16];
        second[2] = '\0';

        if (dtStrlen > 17) {
            unsigned int const pad = 24 - dtStrlen;
            unsigned int i;

            *uP = atoi(&datetimeString[18]);
            for (i = 0; i < pad; ++i)
                *uP *= 10;
        } else
            *uP = 0;

        *YP = atoi(year);
        *MP = atoi(month);
        *DP = atoi(day);
        *hP = atoi(hour);
        *mP = atoi(minute);
        *sP = atoi(second);
    }
}



static void
validateFirst17(xmlrpc_env * const envP,
                const char * const dt) {
/*----------------------------------------------------------------------------
   Assuming 'dt' is at least 17 characters long, validate that the first
   17 characters are a valid XML-RPC datetime, e.g.
   "20080628T16:35:02"
-----------------------------------------------------------------------------*/
    unsigned int i;

    for (i = 0; i < 8 && !envP->fault_occurred; ++i)
        if (!isdigit(dt[i]))
            xmlrpc_env_set_fault_formatted(
                envP, XMLRPC_PARSE_ERROR, "Not a digit: '%c'", dt[i]);

    if (dt[8] != 'T')
        xmlrpc_env_set_fault_formatted(
            envP, XMLRPC_PARSE_ERROR, "9th character is '%c', not 'T'",
            dt[8]);
    if (!isdigit(dt[9]))
        xmlrpc_env_set_fault_formatted(
            envP, XMLRPC_PARSE_ERROR, "Not a digit: '%c'", dt[9]);
    if (!isdigit(dt[10]))
        xmlrpc_env_set_fault_formatted(
            envP, XMLRPC_PARSE_ERROR, "Not a digit: '%c'", dt[10]);
    if (dt[11] != ':')
        xmlrpc_env_set_fault_formatted(
            envP, XMLRPC_PARSE_ERROR, "Not a colon: '%c'", dt[11]);
    if (!isdigit(dt[12]))
        xmlrpc_env_set_fault_formatted(
            envP, XMLRPC_PARSE_ERROR, "Not a digit: '%c'", dt[12]);
    if (!isdigit(dt[13]))
        xmlrpc_env_set_fault_formatted(
            envP, XMLRPC_PARSE_ERROR, "Not a digit: '%c'", dt[13]);
    if (dt[14] != ':')
        xmlrpc_env_set_fault_formatted(
            envP, XMLRPC_PARSE_ERROR, "Not a colon: '%c'", dt[14]);
    if (!isdigit(dt[15]))
        xmlrpc_env_set_fault_formatted(
            envP, XMLRPC_PARSE_ERROR, "Not a digit: '%c'", dt[15]);
    if (!isdigit(dt[16]))
        xmlrpc_env_set_fault_formatted(
            envP, XMLRPC_PARSE_ERROR, "Not a digit: '%c'", dt[16]);
}



static void
validateFractionalSeconds(xmlrpc_env * const envP,
                          const char * const dt) {
/*----------------------------------------------------------------------------
   Validate the fractional seconds part of the XML-RPC datetime string
   'dt', if any.  That's the decimal point and everything following
   it.
-----------------------------------------------------------------------------*/
    if (strlen(dt) > 17) {
        if (dt[17] != '.') {
            xmlrpc_env_set_fault_formatted(
                envP, XMLRPC_PARSE_ERROR,
                "'%c' where only a period is valid", dt[17]);
        } else {
            if (dt[18] == '\0')
                xmlrpc_env_set_fault_formatted(
                    envP, XMLRPC_PARSE_ERROR, "Nothing after decimal point");
            else {
                unsigned int i;
                for (i = 18; dt[i] != '\0' && !envP->fault_occurred; ++i) {
                    if (!isdigit(dt[i]))
                        xmlrpc_env_set_fault_formatted(
                            envP, XMLRPC_PARSE_ERROR,
                            "Non-digit in fractional seconds: '%c'", dt[i]);
                }
            }
        }
    }
}



static __inline__ void
validateFormat(xmlrpc_env * const envP,
               const char * const dt) {

    if (strlen(dt) < 17)
        xmlrpc_env_set_fault_formatted(
            envP, XMLRPC_PARSE_ERROR, 
            "Invalid length of %u of datetime.  "
            "Must be at least 17 characters",
            strlen(dt));
    else {
        validateFirst17(envP, dt);

        validateFractionalSeconds(envP, dt);
    }
}



static void
parseDateNumbers(xmlrpc_env *   const envP,
                 const char *   const datetimeString,
                 unsigned int * const YP,
                 unsigned int * const MP,
                 unsigned int * const DP,
                 unsigned int * const hP,
                 unsigned int * const mP,
                 unsigned int * const sP,
                 unsigned int * const uP) {

#if HAVE_REGEX
    parseDateNumbersRegex(envP, datetimeString, YP, MP, DP, hP, mP, sP, uP);
#else
    /* Note: validation is not as strong without regex */
    validateFormat(envP, datetimeString);
    if (!envP->fault_occurred)
        parseDateNumbersNoRegex(envP, datetimeString,
                                YP, MP, DP, hP, mP, sP, uP);
#endif
}



static void
parseDatetime(xmlrpc_env *   const envP,
              const char *   const datetimeString,
              time_t *       const timeValueP,
              unsigned int * const usecsP) {
/*----------------------------------------------------------------------------
   Parse a time in the format stored in an xmlrpc_value and return the
   time that it represents.

   datetimeString[] is the input time string.  We return the result as
   *timeValueP.

   Example of the format we parse: "19980717T14:08:55"
   Note that this is not quite ISO 8601.  It's a bizarre combination of
   two ISO 8601 formats.

   The input is capable of representing datetimes that cannot be expressed
   as a time_t.  In that case, we fail, with fault code
   XMLRPC_INTERNAL_ERROR.

   And of course the input may not validly represent a datetime at all.
   In that case too, we fail with fault code XMLRPC_PARSE_ERROR.
-----------------------------------------------------------------------------*/
    validateFormat(envP, datetimeString);

    if (!envP->fault_occurred) {
        unsigned int Y, M, D, h, m, s, u;
        
        parseDateNumbers(envP, datetimeString, &Y, &M, &D, &h, &m, &s, &u);

        if (!envP->fault_occurred) {
            if (Y < 1970)
                xmlrpc_env_set_fault_formatted(envP, XMLRPC_INTERNAL_ERROR,
                                     "Year is too early to represent as "
                                     "a standard Unix time");
            else {
                struct tm brokenTime;
                const char * error;
                
                brokenTime.tm_sec  = s;
                brokenTime.tm_min  = m;
                brokenTime.tm_hour = h;
                brokenTime.tm_mday = D;
                brokenTime.tm_mon  = M - 1;
                brokenTime.tm_year = Y - 1900;
                
                xmlrpc_timegm(&brokenTime, timeValueP, &error);

                if (error) {
                    xmlrpc_env_set_fault_formatted(
                        envP, XMLRPC_PARSE_ERROR, error);
                    xmlrpc_strfree(error);
                } else
                    *usecsP = u;
            }
        }
    }
}



void
xmlrpc_read_datetime_usec(xmlrpc_env *         const envP,
                          const xmlrpc_value * const valueP,
                          time_t *             const secsP,
                          unsigned int *       const usecsP) {
    
    validateDatetimeType(envP, valueP);

    if (!envP->fault_occurred)
        parseDatetime(envP,
                      XMLRPC_MEMBLOCK_CONTENTS(char, &valueP->_block),
                      secsP,
                      usecsP);
}



void
xmlrpc_read_datetime_sec(xmlrpc_env *         const envP,
                         const xmlrpc_value * const valueP,
                         time_t *             const timeValueP) {
    
    unsigned int usecs;

    xmlrpc_read_datetime_usec(envP, valueP, timeValueP, &usecs);
}



#if XMLRPC_HAVE_TIMEVAL

void
xmlrpc_read_datetime_timeval(xmlrpc_env *         const envP,
                             const xmlrpc_value * const valueP,
                             struct timeval *     const timeValueP) {
    
    time_t secs;
    unsigned int usecs;

    xmlrpc_read_datetime_usec(envP, valueP, &secs, &usecs);

    timeValueP->tv_sec  = secs;
    timeValueP->tv_usec = usecs;
}
#endif



#if XMLRPC_HAVE_TIMESPEC

void
xmlrpc_read_datetime_timespec(xmlrpc_env *         const envP,
                              const xmlrpc_value * const valueP,
                              struct timespec *    const timeValueP) {
    
    time_t secs;
    unsigned int usecs;

    xmlrpc_read_datetime_usec(envP, valueP, &secs, &usecs);

    timeValueP->tv_sec  = secs;
    timeValueP->tv_nsec = usecs * 1000;
}
#endif



xmlrpc_value *
xmlrpc_datetime_new_str(xmlrpc_env * const envP, 
                        const char * const value) {

    xmlrpc_value * valP;

    xmlrpc_createXmlrpcValue(envP, &valP);

    if (!envP->fault_occurred) {
        valP->_type = XMLRPC_TYPE_DATETIME;

        XMLRPC_TYPED_MEM_BLOCK_INIT(
            char, envP, &valP->_block, strlen(value) + 1);
        if (!envP->fault_occurred) {
            char * const contents =
                XMLRPC_TYPED_MEM_BLOCK_CONTENTS(char, &valP->_block);
            strcpy(contents, value);
        }
        if (envP->fault_occurred)
            free(valP);
    }
    return valP;
}



xmlrpc_value*
xmlrpc_datetime_new_usec(xmlrpc_env * const envP,
                         time_t       const secs,
                         unsigned int const usecs) {

    xmlrpc_value * valP;

    xmlrpc_createXmlrpcValue(envP, &valP);

    if (!envP->fault_occurred) {
        struct tm brokenTime;
        char timeString[64];

        xmlrpc_gmtime(secs, &brokenTime);

        /* Note that this format is NOT ISO 8601 -- it's a bizarre
           hybrid of two ISO 8601 formats.
        */
        strftime(timeString, sizeof(timeString), "%Y%m%dT%H:%M:%S", 
                 &brokenTime);

        if (usecs != 0) {
            char usecString[64];
            assert(usecs < 1000000);
            snprintf(usecString, sizeof(usecString), ".%06u", usecs);
            STRSCAT(timeString, usecString);
        }

        valP->_type = XMLRPC_TYPE_DATETIME;
        
        XMLRPC_TYPED_MEM_BLOCK_INIT(
            char, envP, &valP->_block, strlen(timeString) + 1);

        if (!envP->fault_occurred) {
            char * const contents =
                XMLRPC_TYPED_MEM_BLOCK_CONTENTS(char, &valP->_block);
        
            strcpy(contents, timeString);
        }
        if (envP->fault_occurred) {
            free(valP);
            valP = NULL;
        }
    }
    return valP;
}



xmlrpc_value *
xmlrpc_datetime_new_sec(xmlrpc_env * const envP, 
                        time_t       const value) {

    return xmlrpc_datetime_new_usec(envP, value, 0);
}



#if XMLRPC_HAVE_TIMEVAL

xmlrpc_value *
xmlrpc_datetime_new_timeval(xmlrpc_env *   const envP, 
                            struct timeval const value) {

    return xmlrpc_datetime_new_usec(envP, value.tv_sec, value.tv_usec);
}
#endif



#if XMLRPC_HAVE_TIMESPEC

xmlrpc_value *
xmlrpc_datetime_new_timespec(xmlrpc_env *    const envP, 
                             struct timespec const value) {

    return xmlrpc_datetime_new_usec(envP, value.tv_sec, value.tv_nsec/1000);
}
#endif
