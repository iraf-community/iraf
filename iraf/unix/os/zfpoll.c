/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/poll.h>

#define NOLIBCNAMES
#define import_spp
#define import_kernel
#define import_knames
#define import_fpoll
#include <iraf.h>


/* ZFPOLL -- Wait for events on a set of file descriptors.  The 'pfds' 
 * array is passed in as an array of (nfd/3) integer triplets representing
 * the (fd,events,revents) elements of the pollfd stucture.  If the 'timeout'
 * value is negative the poll will block until an event occurs, otherwise we
 * will return after the specified number of milliseconds.  Upon return, 
 * the 'pfds' array is overwritten with the return events.
 */
/* pfds    : pollfd array			*/
/* nfds    : no. of file descriptors to poll	*/
/* timeout : timeout (milliseconds)		*/
/* npoll   : poll return value			*/
/* status  : return status			*/
int ZFPOLL ( XINT *pfds, XINT *nfds, XINT *timeout, XINT *npoll, XINT *status )
{
	struct pollfd tmp_poll_fds[MAX_POLL_FD];
	int i, j, nf = *nfds;

	/* Check for errors and initialize the pollfd array.  */
	if (nf > MAX_POLL_FD) {
	    *npoll = -4;
	    *status = XERR;
	    return *status;
	}
	memset ((char *)poll_fds, 0, sizeof(poll_fds));
	memset ((char *)tmp_poll_fds, 0, sizeof(tmp_poll_fds));

	/* Break out the pfds array into the proper pollfd struct. */
	for (i=j=0; i < nf; i++) {
	    poll_fds[i].fp_fd = pfds[j++];
	    poll_fds[i].fp_events = (unsigned short)pfds[j++];
	    poll_fds[i].fp_revents = (unsigned short)pfds[j++];
	    tmp_poll_fds[i].fd = poll_fds[i].fp_fd;
	    if ( (poll_fds[i].fp_events & IRAF_POLLIN) != 0 ) {
		tmp_poll_fds[i].events |= POLLIN;
	    }
	    if ( (poll_fds[i].fp_events & IRAF_POLLPRI) != 0 ) {
		tmp_poll_fds[i].events |= POLLPRI;
	    }
	    if ( (poll_fds[i].fp_events & IRAF_POLLOUT) != 0 ) {
		tmp_poll_fds[i].events |= POLLOUT;
	    }
	    if ( (poll_fds[i].fp_events & IRAF_POLLERR) != 0 ) {
		tmp_poll_fds[i].events |= POLLERR;
	    }
	    if ( (poll_fds[i].fp_events & IRAF_POLLHUP) != 0 ) {
		tmp_poll_fds[i].events |= POLLHUP;
	    }
	    if ( (poll_fds[i].fp_events & IRAF_POLLNVAL) != 0 ) {
		tmp_poll_fds[i].events |= POLLNVAL;
	    }
	    tmp_poll_fds[i].revents = tmp_poll_fds[i].events;
	}

	/* Do the poll of the descriptors. */
	*npoll = poll (tmp_poll_fds, nf, *timeout);

	for (i=0; i < nf; i++) {
	    if ( (tmp_poll_fds[i].revents & POLLIN) != 0 ) 
		poll_fds[i].fp_revents |= IRAF_POLLIN;
	    else poll_fds[i].fp_revents &= ~IRAF_POLLIN;
	    if ( (tmp_poll_fds[i].revents & POLLPRI) != 0 ) 
		poll_fds[i].fp_revents |= IRAF_POLLPRI;
	    else poll_fds[i].fp_revents &= ~IRAF_POLLPRI;
	    if ( (tmp_poll_fds[i].revents & POLLOUT) != 0 ) 
		poll_fds[i].fp_revents |= IRAF_POLLOUT;
	    else poll_fds[i].fp_revents &= ~IRAF_POLLOUT;
	    if ( (tmp_poll_fds[i].revents & IRAF_POLLERR) != 0 ) 
		poll_fds[i].fp_revents |= IRAF_POLLERR;
	    else poll_fds[i].fp_revents &= ~IRAF_POLLERR;
	    if ( (tmp_poll_fds[i].revents & POLLHUP) != 0 ) 
		poll_fds[i].fp_revents |= IRAF_POLLHUP;
	    else poll_fds[i].fp_revents &= ~IRAF_POLLHUP;
	    if ( (tmp_poll_fds[i].revents & POLLNVAL) != 0 ) 
		poll_fds[i].fp_revents |= IRAF_POLLNVAL;
	    else poll_fds[i].fp_revents &= ~IRAF_POLLNVAL;
	}

	if (*npoll < 0) {
	    if (*npoll == EBADF)
		*npoll = -3;
	    else if (*npoll == EINTR)
		*npoll = -2;
	    else 
		*npoll = XERR;
	    *status = XERR;
	    return *status;
	}

	/* Write the revents back to the pfds array. */
	for (j=0,i=2; j < nf; i+=3)
	    pfds[i] = poll_fds[j++].fp_revents;

	*status = XOK;
	return *status;
}
