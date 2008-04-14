/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_fpoll
#define import_xnames
#include <iraf.h>


/* C_POLL --  LIBC binding to the FIO polling interface.
 *
 *       fds = c_poll_open ()                   # open a poll descriptor set
 *         npolls = c_poll (fds, nfds, timeout) # poll the set
 *            c_poll_close (fds)                # free the poll descriptor set
 *
 *             c_poll_zero (fds)                # zero the poll array
 *              c_poll_set (fds, fd, type)      # set fd to poll for type
 *            c_poll_clear (fds, fd, type)      # unset type on fd poll
 *       y/n = c_poll_test (fds, fd, type)      # test fd for type event
 *            c_poll_print (fds)                # print the poll array
 *     N = c_poll_get_nfds (fds)                # get size of descriptor set
 */

/* C_POLL_OPEN -- Open a poll descriptor set.
 */
void *c_poll_open ( void )
{
	XPOINTER fds;

	iferr ((fds = POLL_OPEN ()))
	    return ((void *)0);
	else 
	    return ((void *)fds);
}


/* C_POLL -- Poll the descriptor set.
 */
/* fds     : descriptor set ptr */
/* nfds    : no. descriptors    */
/* timeout : poll timeout       */
int c_poll ( void *fds, int nfds, int timeout )
{
	XINT	npoll;
	XPOINTER x_fds = (XPOINTER)fds;
	XINT x_nfds = nfds;
	XINT x_timeout = timeout;

	npoll = POLL (&x_fds, &x_nfds, &x_timeout);
	return (npoll);
}


/* C_POLL_CLOSE -- Close and free a poll descriptor set.
 */
/* fds : descriptor set ptr */
void c_poll_close ( void *fds )
{
	XPOINTER x_fds = (XPOINTER)fds;
	POLL_CLOSE (&x_fds);
}


/* C_POLL_ZERO -- Zero the descriptor set.
 */
/* fds : descriptor set ptr */
void c_poll_zero ( void *fds )
{
	XPOINTER x_fds = (XPOINTER)fds;
	POLL_ZERO (&x_fds);
}


/* C_POLL_SET --  Add a descriptor to the set, and/or modify the event type.
 * The type may be a bitwise or of testable events.
 */
/* fds  : descriptor set ptr */
/* fd   : no. descriptors    */
/* type : event type         */
void c_poll_set ( void *fds, int fd, int type )
{
	XPOINTER x_fds = (XPOINTER)fds;
	XINT x_fd = fd;
	XINT x_type = type;
	POLL_SET (&x_fds, &x_fd, &x_type);
}


/* C_POLL_CLEAR --  Remove a descriptor or event type from the set.  The type
 * may be a bitwise or of testable events.  If the event mask becomes NULL the
 * descriptor is removed entirely from the set.
 */
/* fds  : descriptor set ptr */
/* fd   : no. descriptors    */
/* type : event type         */
void c_poll_clear ( void *fds, int fd, int type )
{
	XPOINTER x_fds = (XPOINTER)fds;
	XINT x_fd = fd;
	XINT x_type = type;
	POLL_CLEAR (&x_fds, &x_fd, &x_type);
}


/* C_POLL_TEST -- Test the descriptor for the given event type.
 */
/* fds  : descriptor set ptr */
/* fd   : no. descriptors    */
/* type : event type         */
int c_poll_test ( void *fds, int fd, int type )
{
	XPOINTER x_fds = (XPOINTER)fds;
	XINT x_fd = fd;
	XINT x_type = type;
	return (POLL_TEST (&x_fds, &x_fd, &x_type));
}


/* C_POLL_GET_NFDS -- Return the size of the descriptor set.
 */
/* fds : descriptor set ptr */
int c_poll_get_nfds ( void *fds )
{
	XPOINTER x_fds = (XPOINTER)fds;
	return (POLL_GET_NFDS (&x_fds));
}


/* C_POLL_PRINT --  Debug print utility.
 */
void c_poll_print ( void *fds )
{
	XPOINTER x_fds = (XPOINTER)fds;
	POLL_PRINT (&x_fds);
}
