/**
 *
 *  VOCUTIL_F77.C  -- Utility routines to support Fortran bindings.
 *
 *  @file       vocUtil_f77.c
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
 *  Local interface declarations.
 */
char  *sstrip (char *instr, int len);
void   spad (char *outstr, int len);
int    typecode (char *typestr);



/**
 *  SSTRIP -- Strip trailing blanks from a string and add null terminator.
 *
 *  @brief      Strip trailing blanks from a string and add null terminator
 *  @fn         outstr = sstrip (char *instr, int len)
 *
 *  @param  instr       string to strip
 *  @param  len		length of input string
 *  @returns            stripped output string
 */
#define SZ_LINE		1024

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


/**
 *  SPAD -- Pad a string to length 'len' with blanks, as Fortran requires.
 *
 *  @brief      Pad a string to length 'len' with blanks, as Fortran requires.
 *  @fn         spad (char *outstr, int len)
 *
 *  @param  outstr      string to pad
 *  @param  len		pad string to this length
 *  @returns            nothing
 */
void spad (char *outstr, int len)
{
    int i=0;
        
#ifndef _NO_SPAD_
    for (i = strlen(outstr); i < len; i++)
        outstr[i] = ' ';
#endif
}


/**
 *  TYPECODE -- Convert a DAL type string to a code value.
 *
 *  @brief      Convert a DAL type string to a code value.
 *  @fn         val = typecode (char *typestr)
 *
 *  @param  typestr     DAL type string
 *  @returns            DAL type code
 */

#define SZ_TYPECODE 	32

int typecode (char *typestr) 
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

    return (ERR);
}
