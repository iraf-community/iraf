/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <unistd.h>


/* OS_READ -- Read from a disk file.  We can use the UNIX procedures for
 * reading both binary and text files.
 */
int
os_read (
  int	fd,			/* input file		*/
  char	*buf,			/* output buffer	*/
  int	nbytes 			/* max bytes to read	*/
)
{
	return (read (fd, buf, nbytes));
}
