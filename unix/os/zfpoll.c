/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <errno.h>

#define import_spp
#define	import_kernel
#define	import_knames
#define import_fpoll
#include <iraf.h>


/* ZFPOLL -- Wait for events on a set of file descriptors.  The 'pfds' 
 * array is passed in as an array of (nfd/3) integer triplets representing
 * the (fd,events,revents) elements of the pollfd stucture.  If the 'timeout'
 * value is negative the poll will block until an event occurs, otherwise we
 * will return after the specified number of milliseconds.  Upon return, 
 * the 'pfds' array is overwritten with the return events.
 */
ZFPOLL (pfds, nfds, timeout, npoll, status)
XINT	*pfds;				/* pollfd array			   */
XINT	*nfds;				/* no. of file descriptors to poll */
XINT	*timeout;			/* timeout (milliseconds)	   */
XINT	*npoll;				/* poll return value		   */
XINT	*status;			/* return status		   */
{
	int	i, j, nf = *nfds;
	extern	int errno;


	/* Check for errors and initialize the pollfd array.  */
	if (nf > MAX_POLL_FD) {
	    *npoll = -4;
	    *status = XERR;
	    return;
	}
	bzero ((char *)poll_fds, sizeof(poll_fds));

	/* Break out the pfds array into the proper pollfd struct. */
	for (i=j=0; i < nf; i++) {
	    poll_fds[i].fp_fd = pfds[j++];
	    poll_fds[i].fp_events = (unsigned short)pfds[j++];
	    poll_fds[i].fp_revents = (unsigned short)pfds[j++];
	}

	/* Do the poll of the descriptors. */
	*npoll = poll (poll_fds, nf, *timeout);
	if (*npoll < 0) {
	    if (*npoll == EBADF)
		*npoll = -3;
	    else if (*npoll == EINTR)
		*npoll = -2;
	    else 
		*npoll = XERR;
	    *status = XERR;
	    return;
	}

	/* Write the revents back to the pfds array. */
	for (j=0,i=2; j < nf; i+=3)
	    pfds[i] = poll_fds[j++].fp_revents;

	*status = XOK;
	return;
}
