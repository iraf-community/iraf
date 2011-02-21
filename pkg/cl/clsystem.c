/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define	import_libc
#define	import_stdio
#include <iraf.h>
#include "errs.h"


/* CLSYSTEM -- Run a host system command and try to arrange for its standard
 * output and standard error output to go where our t_stdout is going; this
 * will let us redirect its output and use it with pipes..
 */
void
clsystem (
  char	*cmd,			/* command to be executed	*/
  FILE	*taskout,		/* stdout of task		*/
  FILE	*taskerr		/* stderr of task		*/
)
{
	register char	*ip;
	register int	ch;
	char	outfile[SZ_PATHNAME], errfile[SZ_PATHNAME];
	FILE	*fp;

	/* Ignore null commands.
	 */
	for (ip=cmd;  (*ip == ' ' || *ip == '\t');  ip++)
	    ;
	if (*ip == EOS)
	    return;

	/* Run command with output redirected into temporary files.
	 * This is done only if the output is redirected.
	 */
	outfile[0] = EOS;
	errfile[0] = EOS;

	if (taskout && taskout != stdout)
	    c_mktemp ("tmp$tso", outfile, SZ_PATHNAME);

	if (taskerr == taskout)
	    strcpy (errfile, outfile);
	else if (taskerr && taskerr != stderr)
	    c_mktemp ("tmp$tse", errfile, SZ_PATHNAME);

	c_oscmd (cmd, "", outfile, errfile);

	/* Copy spooled output, if any, to the error streams of the current
	 * task.
	 */
	if (outfile[0] != EOS)
	    if ((fp = fopen (outfile, "r")) != NULL) {
		while ((ch = fgetc (fp)) != EOF)
		    fputc (ch, taskout);
		fclose (fp);
		c_delete (outfile);
	    }

	if (errfile[0] != EOS && taskerr != taskout)
	    if ((fp = fopen (errfile, "r")) != NULL) {
		while ((ch = fgetc (fp)) != EOF)
		    fputc (ch, taskerr);
		fclose (fp);
		c_delete (errfile);
	    }
}
