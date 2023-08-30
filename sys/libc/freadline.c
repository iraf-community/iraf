/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#include <string.h>

#define	import_spp
#include <iraf.h>


/*  FREADLINE -- Get a line from a user with editing.  This is a libc 
 *  interface to the host readline() interface.  The host readline()
 *  returns a buffer allocated which we free here, what's returned to
 *  the caller is a static buffer containing the input string.
 */
char *
u_freadline (
  char	*prompt				/* user supplied output buffer	*/
)
{
	char   *cmd = (char *) NULL;
	static char line[SZ_LINE];
	char   *readline (char *prompt);
        int     ZFREE (void *buf);


	memset (line, 0, SZ_LINE);
        if ((cmd = readline (prompt)) == (char *) NULL) {
            return ((char *) NULL);
	} else {
            /* Save to static buffer using host strncpy().  The 'cmd'
             * buffer returned by readline() is allocated with a host
             * malloc() and must be freed by the kernel.
             */
	    strncpy (line, cmd, min(strlen(cmd),SZ_LINE));		
	    ZFREE ((void *) cmd);
	}

	return ((char *) line);
}
