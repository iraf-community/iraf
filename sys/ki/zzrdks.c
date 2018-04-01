/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

int	spoolit = 0;
int	spoolfd = 0;

/* Intercept KS intput and spool in a file for subsequent debugging.
 * [MACHDEP].  This is a UNIX dependent debugging routine.  To get rid of
 * it, delete the file, edit the Makefile, and change the reference to
 * zzrdks in irafks.x to zardks.
 */
int 
zzrdks_ (int *chan, short *buf, int *maxb, int *off)
{
	int	status;

	zardks_ (chan, buf, maxb, off);

	if (spoolit) {
	    if (spoolfd == 0)
		spoolfd = creat ("/tmp/ks.in", 0644);
	    zawtks_ (chan, &status);
	    if (status > 0)
		write (spoolfd, buf, status);
	}
}
