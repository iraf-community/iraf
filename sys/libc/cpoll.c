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
c_poll_open ()
{
	XINT	fds;

	iferr ((fds = POLL_OPEN ()))
	    return (NULL);
	else 
	    return (fds);
}


/* C_POLL -- Poll the descriptor set.
 */
c_poll (fds, nfds, timeout)
int	fds;					/* descriptor set ptr	*/
int	nfds;					/* no. descriptors	*/
int	timeout;				/* poll timeout		*/
{
	int	npoll, POLL();

	npoll = POLL (&fds, &nfds, &timeout);
	return (npoll);
}


/* C_POLL_CLOSE -- Close and free a poll descriptor set.
 */
c_poll_close (fds)
int	fds;					/* descriptor set ptr	*/
{
	POLL_CLOSE (&fds);
}


/* C_POLL_ZERO -- Zero the descriptor set.
 */
c_poll_zero (fds)
int	fds;					/* descriptor set ptr	*/
{
	POLL_ZERO (&fds);
}


/* C_POLL_SET --  Add a descriptor to the set, and/or modify the event type.
 * The type may be a bitwise or of testable events.
 */
c_poll_set (fds, fd, type)
int	fds;					/* descriptor set ptr	*/
int	fd;					/* no. descriptors	*/
int	type;					/* event type		*/
{
	POLL_SET (&fds, &fd, &type);
}


/* C_POLL_CLEAR --  Remove a descriptor or event type from the set.  The type
 * may be a bitwise or of testable events.  If the event mask becomes NULL the
 * descriptor is removed entirely from the set.
 */
c_poll_clear (fds, fd, type)
int	fds;					/* descriptor set ptr	*/
int	fd;					/* no. descriptors	*/
int	type;					/* event type		*/
{
	POLL_CLEAR (&fds, &fd, &type);
}


/* C_POLL_TEST -- Test the descriptor for the given event type.
 */
c_poll_test (fds, fd, type)
int	fds;					/* descriptor set ptr	*/
int	fd;					/* no. descriptors	*/
int	type;					/* event type		*/
{
	return (POLL_TEST (&fds, &fd, &type));
}


/* C_POLL_GET_NFDS -- Return the size of the descriptor set.
 */
c_poll_get_nfds (fds)
int	fds;					/* descriptor set ptr	*/
{
	return (POLL_GET_NFDS (&fds));
}



/* C_POLL_PRINT --  Debug print utility.
 */
c_poll_print (fds) int	fds; { POLL_PRINT (&fds); }
