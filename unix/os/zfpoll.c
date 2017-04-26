/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <errno.h>
#include <sys/poll.h>

#define import_spp
#define	import_kernel
#define	import_knames
#define import_fpoll
#include <iraf.h>


/* For compilation on systems with old and dev versions.
*/
#ifndef IRAF_POLLIN
#define IRAF_POLLIN      0x0001         /* There is data to read        */
#define IRAF_POLLPRI     0x0002         /* There is urgent data to read */
#define IRAF_POLLOUT     0x0004         /* Writing now will not block   */
#define IRAF_POLLERR     0x0008         /* Error condition              */
#define IRAF_POLLHUP     0x0010         /* Hung up                      */
#define IRAF_POLLNVAL    0x0020         /* Invalid request: fd not open */
#endif




/* ZFPOLL -- Wait for events on a set of file descriptors.  The 'pfds' 
 * array is passed in as an array of (nfd/3) integer triplets representing
 * the (fd,events,revents) elements of the pollfd stucture.  If the 'timeout'
 * value is negative the poll will block until an event occurs, otherwise we
 * will return after the specified number of milliseconds.  Upon return, 
 * the 'pfds' array is overwritten with the return events.
 */
int
ZFPOLL (
  XINT	*pfds,				/* pollfd array			   */
  XINT	*nfds,				/* no. of file descriptors to poll */
  XINT	*timeout,			/* timeout (milliseconds)	   */
  XINT	*npoll,				/* poll return value		   */
  XINT	*status 			/* return status		   */
)
{
	struct pollfd tmp_fds[MAX_POLL_FD];
	int	i, j, nf = *nfds;
	extern	int errno;


	/* Check for errors and initialize the pollfd array.  */
	if (nf > MAX_POLL_FD) {
	    *npoll = -4;
	    *status = XERR;
	    return (XERR);
	}
	memset ((char *)tmp_fds, 0, sizeof(tmp_fds));
	memset ((char *)poll_fds, 0, sizeof(poll_fds));

	/* Break out the pfds array into the proper pollfd struct. */
	for (i=j=0; i < nf; i++) {
	    poll_fds[i].fp_fd = pfds[j++];
	    poll_fds[i].fp_events = (unsigned short)pfds[j++];
	    poll_fds[i].fp_revents = (unsigned short)pfds[j++];
            tmp_fds[i].fd = poll_fds[i].fp_fd;
            if ( (poll_fds[i].fp_events & IRAF_POLLIN) != 0 )
                tmp_fds[i].events |= POLLIN;
            if ( (poll_fds[i].fp_events & IRAF_POLLPRI) != 0 )
                tmp_fds[i].events |= POLLPRI;
            if ( (poll_fds[i].fp_events & IRAF_POLLOUT) != 0 )
                tmp_fds[i].events |= POLLOUT;
            if ( (poll_fds[i].fp_events & IRAF_POLLERR) != 0 )
                tmp_fds[i].events |= POLLERR;
            if ( (poll_fds[i].fp_events & IRAF_POLLHUP) != 0 )
                tmp_fds[i].events |= POLLHUP;
            if ( (poll_fds[i].fp_events & IRAF_POLLNVAL) != 0 )
                tmp_fds[i].events |= POLLNVAL;
            tmp_fds[i].revents = tmp_fds[i].events;

	}

	/* Do the poll of the descriptors. */
	*npoll = poll (tmp_fds, nf, *timeout);

        for (i=0; i < nf; i++) {
            if ( (tmp_fds[i].revents & POLLIN) != 0 )
                poll_fds[i].fp_revents |= IRAF_POLLIN;
            else 
		poll_fds[i].fp_revents &= ~IRAF_POLLIN;
            if ( (tmp_fds[i].revents & POLLPRI) != 0 )
                poll_fds[i].fp_revents |= IRAF_POLLPRI;
            else 
		poll_fds[i].fp_revents &= ~IRAF_POLLPRI;
            if ( (tmp_fds[i].revents & POLLOUT) != 0 )
                poll_fds[i].fp_revents |= IRAF_POLLOUT;
            else 
		poll_fds[i].fp_revents &= ~IRAF_POLLOUT;
            if ( (tmp_fds[i].revents & IRAF_POLLERR) != 0 )
                poll_fds[i].fp_revents |= IRAF_POLLERR;
            else 
		poll_fds[i].fp_revents &= ~IRAF_POLLERR;
            if ( (tmp_fds[i].revents & POLLHUP) != 0 )
                poll_fds[i].fp_revents |= IRAF_POLLHUP;
            else 
		poll_fds[i].fp_revents &= ~IRAF_POLLHUP;
            if ( (tmp_fds[i].revents & POLLNVAL) != 0 )
                poll_fds[i].fp_revents |= IRAF_POLLNVAL;
            else 
		poll_fds[i].fp_revents &= ~IRAF_POLLNVAL;
        }


	if (*npoll < 0) {
	    if (*npoll == EBADF)
		*npoll = -3;
	    else if (*npoll == EINTR)
		*npoll = -2;
	    else 
		*npoll = XERR;
	    *status = XERR;
	    return (XERR);
	}

	/* Write the revents back to the pfds array. */
	for (j=0,i=2; j < nf; i+=3)
	    pfds[i] = poll_fds[j++].fp_revents;

	*status = XOK;
	return (*status);
}
