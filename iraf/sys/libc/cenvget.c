/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/*
 * ENVGET[SBI] -- Assorted routines for fetching the values of environment
 *   variables.
 * ENVPUTS -- Set or redefine the value of an environment variable.
 * ENVRESET -- Reset (overwrite) the value of an environment variable.
 */

#define SZ_VALUESTR	SZ_COMMAND
static	XCHAR valuestr[SZ_VALUESTR+1];
static	XINT len_valuestr = SZ_VALUESTR;


/* ENVGET -- (Near) emulation of the UNIX getenv procedure.  This is a handy
 * routine for looking up environment variables in C programs.  The value is
 * fetched into an internal static array and a pointer to this array is returned
 * as the function value, else NULL if the environment variable is not found.
 * The caller is responsible for using the value string before it is overwritten
 * by the next call.
 */
char *envget ( const char *var )
{
	if (ENVFIND (c_sppstr(var), valuestr, &len_valuestr) < 0)
	    return (NULL);
	else
	    return (c_strpak (valuestr, (char *)valuestr, len_valuestr+1));
}


/* C_ENVGETS -- Fetch the string value of an environment variable into the
 * output string.  Return the strlen length of the output string as the
 * function value.  If the variable is not found and the process is being
 * used interactively a query is generated.  If no value is given in response
 * to the query or the process is noninteractive and the variable is not
 * found, zero is returned.
 */
/* var     : name of variable to be fetched */
/* outstr  : output string                  */
/* bufsize : buffer size of outstr          */
int c_envgets ( const char *var, char *outstr, size_t bufsize )
{
	XINT nchars;

	if ((nchars = ENVGETS (c_sppstr(var), valuestr, &len_valuestr)) < 0)
	    return (nchars);
	else {
	    c_strpak (valuestr, outstr, bufsize);
	    return (nchars > bufsize-1 ? bufsize-1 : nchars);
	}
}


/* C_ENVFIND -- Just like ENVGETS, except that a query will not be generated
 * even if working interactively.
 */
/* var     : name of variable to be fetched */
/* outstr  : output string                  */
/* bufsize : buffer size of outstr          */
int c_envfind ( const char *var, char *outstr, size_t bufsize )
{
	XINT nchars;

	if ((nchars = ENVFIND (c_sppstr(var), valuestr, &len_valuestr)) < 0)
	    return (nchars);
	else {
	    c_strpak (valuestr, outstr, bufsize);
	    return (nchars > bufsize-1 ? bufsize-1 : nchars);
	}
}


/* C_ENVGETB -- Return the boolean value of an environment variable.  An error
 * action is taken if the variable is not found or the value string cannot
 * be interpreted as a boolean value.
 */
/* var : name of variable to be fetched */
int c_envgetb ( const char *var )
{
	XBOOL val = ENVGETB (c_sppstr(var));
	return (BTOI (&val));
}


/* C_ENVGETI -- Return the integer value of an environment variable.  An error
 * action is taken if the variable is not found or the value string cannot
 * be interpreted as an integer value.
 */
/* var : name of variable to be fetched */
int c_envgeti ( const char *var )
{
	return (ENVGETI (c_sppstr(var)));
}


/* C_ENVPUTS -- Set or redefine the value of an environment variable.  If the
 * variable is not defined a new entry is created.  If the variable is already
 * defined but has a different value it is redefined, with the new definition
 * having precedence over the former (redefinitions can be undone; see envmark
 * and envfree).  If the variable is already defined and the new definition
 * has the same value, it is ignored.
 *
 * Arguments:		set var = value
 */
/* var   : name of variable to be set */
/* value : value string               */
void c_envputs ( const char *var, char *value )
{
	ENVPUTS (c_sppstr(var), c_strupk (value, valuestr, SZ_VALUESTR+1));
}


/* C_ENVRESET -- Reset (overwrite) the value of an environment variable.
 * If the variable is not defined a new entry is created.
 */
/* var   : name of variable to be set */
/* value : value string               */
void c_envreset ( const char *var, char *value )
{
	ENVRESET (c_sppstr(var), c_strupk (value, valuestr, SZ_VALUESTR+1));
}
