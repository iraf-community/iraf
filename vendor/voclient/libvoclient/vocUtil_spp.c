/**
 *  VOCUTIL_SPP.C -- Utility routines for the SPP proceduers.
 *
 *  Michael Fitzpatrick, NOAO, Jul 2006
 *
 *  @file       vocUtil_spp.c
 *  @author     Michael Fitzpatrick
 *  @version    June 2006
 *
 *************************************************************************
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>

#define _VOCLIENT_LIB_
#include "VOClient.h"


/**
 *  SPP Name mapping macros.  SPP procedure names are mappad as the first-5
 *  plus the last character of a name minus any underscores.  This should
 *  be done such that a unique 6-character name is produced for each SPP
 *  symbol.  In these definitions the SPP code may use the long form of the
 *  name in the code, the mapping is done automatically and so we need the
 *  macros here so the symbol entered in the library is actually the short
 *  name.
 */

/**
 *  SPP Type definitions.
 */
#define XCHAR		short
#define PKCHAR		char
#define XINT		int
#define XEOS		VOC_NULL


/**
 *  Public interface procedures.
 *
 */
PKCHAR *spp2c (XCHAR *instr,  int maxch);
int     c2spp (PKCHAR *instr, XCHAR *outstr, int maxch);
int    spplen (XCHAR *str);
int  dal_typecode (char *typestr);
int  out_typecode (char *typestr);



/**
 *  SPP2C -- Convert an SPP string to a C string.
 *
 *  @brief      Convert an SPP string to a C string.
 *  @fn         cstr = spp2c (XCHAR *sppstr, int maxch)
 *
 *  @param  sppstr      input SPP string
 *  @param  maxch       max length of SPP string
 *  @returns            converted C string
 */
char *
spp2c (XCHAR *instr, int maxch)
{
    XCHAR  *ip = instr;
    char   *outstr = (char *) calloc (1, maxch+1);
    char   *op = (char *) outstr;
    int      n = maxch;

    while ((*op++ = (char)*ip++) != (char) XEOS && --n >= 0)
        ;
    *--op = (char) XEOS;

    return (outstr);
}


/**
 *  C2SPP2C -- Convert a C string to an SPP string.
 *
 *  @brief      Convert a C string to an SPP string.
 *  @fn         sppstr = c2spp (PKCHAR *instr, int maxch)
 *
 *  @param  sppstr      input SPP string
 *  @param  maxch       max length of SPP string
 *  @returns            converted C string
 */
int c2spp (PKCHAR *instr, XCHAR *outstr, int maxch)
{
    char   *ip = (char *)instr;
    XCHAR  *op = outstr;
    int      len= 0, n = 0;

    /* Is is necessary to determine the length of the string in order to
     * be able to unpack the string in place, i.e., from right to left.
     */
    for (n=0;  n  < maxch;  n++)
        outstr[n] = (XCHAR) XEOS;
    for (n=0;  *ip;  n++)
        ip++;
    n -= 1;
    len = (n < maxch) ? n : maxch;
    op[n] = (XCHAR) XEOS;

    for (ip = (char *)instr;  --n >= 0;  )
        op[n] = ip[n];
/*
    op[maxch] = (XCHAR) XEOS;
*/
    op[maxch] = (XCHAR) VOC_NULL;

    return (len);
}


/**
 *  SPPLEN -- Get the length of an SPP string.
 *
 *  @brief      Get the length of an SPP string.
 *  @fn         len = spplen (XCHAR *str)
 *
 *  @param  str         input SPP string
 *  @returns            length of string
 */
int spplen (XCHAR *str)
{
    int len = 0;

    for (len=0; str[len] != (XCHAR) XEOS; len++)
	;

    return (len);
}


/**
 *  DAL_TYPECODE -- Convert a DAL service type string to a code.
 *
 *  @brief      Convert a DAL service type string to a code
 *  @fn         code = dal_typecode (char *typestr)
 *
 *  @param  typestr     input DAL string
 *  @returns            type code
 */

#define SZ_TYPECODE     32

int dal_typecode (char *typestr) 
{
    char type[SZ_TYPECODE];
    int  i = 0;

    memset (type, 0, SZ_TYPECODE);
    for (i=0; typestr[i] && i < SZ_TYPECODE; i++)
        type[i] = tolower (typestr[i]);

    if (strcmp (type, "dal") == 0)  return (DAL_CONN);
    if (strcmp (type, "cone") == 0) return (CONE_CONN);
    if (strcmp (type, "siap") == 0) return (SIAP_CONN);
    if (strcmp (type, "ssap") == 0) return (SSAP_CONN);
    if (strcmp (type, "slap") == 0) return (SLAP_CONN);
    if (strcmp (type, "stap") == 0) return (STAP_CONN);

    return (ERR);
}


/**
 *  OUT_TYPECODE -- Convert an output type string to a code.
 *
 *  @brief      Convert an output type string to a code
 *  @fn         code = out_typecode (char *typestr)
 *
 *  @param  typestr     input type string
 *  @returns            type code
 */
int out_typecode (char *typestr) 
{
    char type[SZ_TYPECODE];
    int  i = 0;

    memset (type, 0, SZ_TYPECODE);
    for (i=0; typestr[i] && i < SZ_TYPECODE; i++)
        type[i] = tolower (typestr[i]);

    if (strcmp (type, "raw") == 0)  	return (VOC_RAW);
    if (strcmp (type, "csv") == 0) 	return (VOC_CSV);
    if (strcmp (type, "votable") == 0) 	return (VOC_VOTABLE);

    return (ERR);
}
