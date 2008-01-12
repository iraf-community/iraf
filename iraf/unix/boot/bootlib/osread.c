/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <unistd.h>

/* OS_READ -- Read from a disk file.  We can use the UNIX procedures for
 * reading both binary and text files.
 */
/* fd     : input file		*/
/* buf    : output buffer	*/
/* nbytes : max bytes to read	*/
int os_read ( int fd, char *buf, int nbytes )
{
	return (read (fd, buf, nbytes));
}
