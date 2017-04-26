/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_stdio
#include <iraf.h>


/*  FREADLINE -- Get a line from a user with editing.  This is a libc 
 *  interface to the host readline() interface.  The host readline()
 *  returns a buffer allocated which we free here, what's returned to
 *  the caller is a static buffer containing the input string.
 */
char *
freadline (
  char	*prompt				/* user supplied output buffer	*/
)
{
	char  *cmd = (char *) NULL;
	static char line[SZ_LINE];
	char  *readline (char *prompt);


	memset (line, 0, SZ_LINE);
        if ((cmd = readline (prompt)) == (char *) NULL) {
            return ((char *) NULL);
	} else {
	    strcpy (line, cmd);		/* save to static buffer	*/
	    zfree_ ((void *) cmd);	/* free readline() buffer	*/
	}

	return ((char *) line);
}
