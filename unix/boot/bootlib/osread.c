/* OS_READ -- Read from a disk file.  We can use the UNIX procedures for
 * reading both binary and text files.
 */
os_read (fd, buf, nbytes)
int	fd;			/* input file		*/
char	*buf;			/* output buffer	*/
int	nbytes;			/* max bytes to read	*/
{
	return (read (fd, buf, nbytes));
}
