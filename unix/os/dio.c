/*
 * DIO.C -- Stubbed out version of directio for compatibility on systems
 * that don't provide this routine in libc.a (e.g., Solaris 5.5).
 */

directio (fd, advice)
int fd;
int advice;
{
	return (-1);
}
