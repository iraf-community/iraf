/***************************************************************************
 *
 *  SPP Language binding for the VOClient interface.
 *
 *  Michael Fitzpatrick, NOAO, Jul 2006
 *
 **************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>

#define _VOCLIENT_LIB_
#include "VOClient.h"


/*  SPP Name mapping macros.  SPP procedure names are mappad as the first-5
**  plus the last character of a name minus any underscores.  This should
**  be done such that a unique 6-character name is produced for each SPP
**  symbol.  In these definitions the SPP code may use the long form of the
**  name in the code, the mapping is done automatically and so we need the
**  macros here so the symbol entered in the library is actually the short
**  name.
*/


/* SPP Type definitions.
*/
#define XCHAR		short
#define PKCHAR		char
#define XINT		int
#define XEOS		VOC_NULL


/*  Public interface procedures.
**
*/
PKCHAR *spp2c (XCHAR *instr,  int maxch);
int     c2spp (PKCHAR *instr, XCHAR *outstr, int maxch);
int    spplen (XCHAR *str);
int  dal_typecode (char *typestr);
int  out_typecode (char *typestr);



char *spp2c (XCHAR *instr, int maxch)
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


int spplen (XCHAR *str)
{
    int len = 0;

    for (len=0; str[len] != (XCHAR) XEOS; len++)
	;

    return (len);
}




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

    return (ERR);
}


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
