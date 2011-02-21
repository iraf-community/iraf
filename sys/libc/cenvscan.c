/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_ENVSCAN -- High level command to parse and enter a SET declaration into
** the environment list, or to parse and enter a list of SET declarations from
** a text file.  The single input string argument should be either a SET
** declaration, i.e.,
**
**	set var="value"				(quotes optional)
**
** or an indirect reference to a text file containing SET declarations, e.g.,
**
**	@filename
**
** If a file is specified, only lines beginning with the keyword "set" will
** be decoded; all other lines are ignored.  The number of SET declarations
** processed is returned as the function value.  There is no fixed limit on
** the number of SET declarations for a process.
*/
int
c_envscan (
  char	*input_source
)
{
	return (ENVSCAN (c_sppstr(input_source)));
}
