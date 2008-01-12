/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_ENVLIST -- List the names and values of all environment variables on the
 * output file.  Variables are listed in the reverse of the order in which
 * they were defined.  If a variable is redefined all definitions or only the
 * most recent definition may be listed.  Each definition appears on a separate
 * line in the following format:
 *
 *	prefix var="value"
 *
 * where "prefix" is the prefix string supplied as an argument, "var" is the
 * name of the variable, and "value" is the value string.
 */
/* fd          : output file                  */
/* prefix      : prefix string, e.g. "set "   */
/* show_redefs : 0=hide redefs, 1=show redefs */
void c_envlist ( int fd, const char *prefix, int show_redefs )
{
	XINT x_fd = fd ;
	XINT x_show_redefs = show_redefs;
	ENVLIST (&x_fd, c_sppstr(prefix), &x_show_redefs);
}
