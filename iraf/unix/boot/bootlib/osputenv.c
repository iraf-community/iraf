/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdlib.h>
#include <string.h>
#define import_xnames
#include "bootlib.h"

#define SZ_VALUE	SZ_COMMAND

#ifdef NOVOS
/* OS_PUTENV -- Set the value of the named environment variable.
 */
void os_putenv ( const char *name, const char *value )
{
	char	buf[SZ_VALUE], *env;

	snprintf (buf, SZ_VALUE, "%s=%s", name, value);
	if (env = (char *) malloc (strlen(buf) + 1)) {
	    strcpy (env, buf);
#ifdef ultrix
	    putenv (env);			/* must keep env around. */
#else
#ifdef vax
	    setenv (name, value, 1);
#else
	    putenv (env);			/* must keep env around. */
#endif
#endif
	}
}

#else

/* OS_PUTENV -- Set the value of the named environment variable.
 */
void os_putenv ( const char *name, const char *value )
{
	XCHAR	x_name[SZ_FNAME+1];
	XCHAR	x_value[SZ_VALUE+1];
	char	buf[SZ_VALUE], *env;

	/* Set the VOS environment. */
	os_strupk (name, x_name, SZ_FNAME+1);
	os_strupk (value, x_value, SZ_VALUE+1);
	ENVRESET (x_name, x_value);

	/* Set the HOST environment. */
	snprintf (buf, SZ_VALUE, "%s=%s", name, value);
	if (env = (char *) malloc (strlen(buf) + 1)) {
	    strcpy (env, buf);
#ifdef ultrix
	    putenv (env);
#else
#ifdef vax
	    setenv (name, value, 1);
#else
	    putenv (env);			/* must keep env around. */
#endif
#endif
	}
}
#endif
