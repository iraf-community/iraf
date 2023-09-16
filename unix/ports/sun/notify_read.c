/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <syscall.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <stdio.h>

static	int (*u_fcn)();		/* user functions to process read	*/
static	int u_fd;		/* fd to be monitored			*/

/* NOTIFY_READ -- This is a customized version of the SunView 3.2 notify_read
 * primitive, the notifier's version of the UNIX read() system call (when
 * the notifier is used, read() is a high level function, not a system call).
 * The function of this special version of notify_read is to intercept kernel
 * read calls made by the notifier for the purposes of monitoring, and
 * possibly filtering, low level input from a file descriptor.
 */
notify_read (fd, buf, maxch)
int	fd;
char	*buf;
int	maxch;
{
	register int n;

	/* This is a bit of a kludge, but lacking the shelltool source it
	 * was difficult to do better.  The 18 is the size of the tty packet
	 * echoed by the driver when a character is typed; this is not part
	 * of the normal output stream so we exclude these events.  The buf+1
	 * business is to hide the packet mode nature of the stream from the
	 * gtermio code; the first byte of each packet indicates the packet
	 * type.  These details could change in a future Sun release in which
	 * case this code would have to be modified.
	 */
	if (u_fcn && fd == u_fd && maxch != 18) {
	    n = syscall (SYS_read, fd, buf, maxch);
	    if (n > 0 && *buf == TIOCPKT_DATA)
		return ((*u_fcn)(buf+1, n-1, maxch-1) + 1);
	    else
		return (n);
	} else
	    return (syscall (SYS_read, fd, buf, maxch));
}


/* READV -- This is a customized version of the readv system call, used in
 * the Release 3.4 version of ttysw to read from the pty.  Usage is (appears
 * to be) identical to the old notify_read, except that the TIOCPKT byte is
 * returned separately from the data.
 */
readv (fd, iov, iovcnt)
register int	fd;
register struct	iovec *iov;
int	iovcnt;
{
	register int n;

	if (u_fcn && fd == u_fd && iovcnt == 2 && iov[0].iov_len == 1) {
	    n = syscall (SYS_readv, fd, iov, iovcnt);
	    if (n > 0 && *(iov[0].iov_base) == TIOCPKT_DATA)
		return ((*u_fcn)(iov[1].iov_base, n-1, iov[1].iov_len) + 1);
	    else
		return (n);
	} else
	    return (syscall (SYS_readv, fd, iov, iovcnt));
}


/* NOTIFY_READ_POST_MONITOR_FCN -- Post a user data monitor/filter function
 * to process the input on the specified file descriptor.  Only one file
 * descriptor can be monitored at present.
 */
notify_read_post_monitor_fcn (fd, fcn)
int	fd;
int	(*fcn)();
{
	if (u_fcn && !fcn)
	    return (-1);
	else {
	    u_fcn = fcn;
	    u_fd  = fd;
	    return (0);
	}
}
