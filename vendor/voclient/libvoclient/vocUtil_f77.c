/***************************************************************************
 *
 *  VOCF77.C  -- Fortran binding for the VOClient interface.  As part of 
 *  the binding we map the interface procedure names and convert string
 *  constants as needed per Fortran rules.  Note that another aspect of
 *  the fortran calling convention is that the length of strings in the
 *  argument list are appended to the call stack.  As C code we need to
 *  take this into account when defining the interface.
 *
 *  M. Fitzpatrick, NOAO, Jul 2006
 *
 **************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>

#define _VOCLIENT_LIB_
#include "VOClient.h"


/** Local interface declarations.
**/
char *sstrip (char *instr, int len);
void  spad (char *outstr, int len);
int   typecode (char *typestr);



/******************************************************************************
** Support utility to trim trailing blanks from string and add
** a null terminator.
*/

#ifdef _OLD_SSTRIP_
#define SZ_LINE		1024

char *sstrip (char *instr, int len)
{
    int i;

    memset (t, 0, SZ_LINE);
    if (instr == NULL)
        t[0] = '\0';
    else {
        strncpy (t, instr, (len < SZ_LINE ? len : SZ_LINE));
        i = (len < SZ_LINE ? len : SZ_LINE) - 1;
        while (t[i] == ' ')
            i = i - 1;
        t[i+1] = '\0';
    }
    return(t);

#else

char *sstrip (char *instr, int len)
{
    if (len > 0 && instr) {
        char *newstr = calloc (1, len+1);
	int i = len;

        strncpy (newstr, instr, len);

	/* trim trailing blanks */
	for (i=len; newstr[i] == ' ' || newstr[i] == '\0'; i--)	
	    newstr[i] = '\0';
	    
        return (newstr);
    }
    return ((char *) NULL);
}

#endif




/******************************************************************************
** SPAD --- Pad a string to length 'len' with blanks, as Fortran
** requires.
*/
 
void spad (char *outstr, int len)
{
    int i=0;
        
#ifndef _NO_SPAD_
    for (i = strlen(outstr); i < len; i++)
        outstr[i] = ' ';
#endif
}


#define SZ_TYPECODE 	32

int
typecode (char *typestr) 
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
