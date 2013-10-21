# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fio.h>
include	<fset.h>
include <config.h>
include	<syserr.h>
include	<poll.h>


.help poll
.nf ___________________________________________________________________________
POLL -- FIO descriptor polling interface.  See <poll.h> for a definition of
the interface data structures and flags, this file is required to be included
by source files using this interface.

       fds = poll_open ()		    # open a poll descriptor set
	 npolls = poll (fds, nfds, timeout) # poll the set
            poll_close (fds)		    # free the poll descriptor set

             poll_zero (fds)		    # zero the poll array
              poll_set (fds, fd, type)	    # set fd to poll for type
            poll_clear (fds, fd, type)	    # unset type on fd poll
       y/n = poll_test (fds, fd, type)	    # test fd for type event
     N = poll_get_nfds (fds)		    # get size of descriptor set

The polling interface provides the same functionality as the unix function
of the same name with a few implementation differences.  The poll_open()
procedure is used to allocate a dynamic structure containing the descriptors
to be polled, poll_close() is used when done to free that structure.
	The poll_zero(), poll_set(), and poll_clear() utility functions are 
used to manipulate the descriptor set by zeroing the entire set, or adding or
removing a descriptor check for the specified polling type.  Polling types
include POLLIN (fd is readable), POLLOUT (fd is writeable & won't block), or
POLLPRI (priority info at fd).  The poll_test() function can be used to test
for these types folling the return of poll().  Additionally, a descriptor may
be checked for POLLERR (fd has error condition), POLLHUP (fd has been hung up
on), POLLNVAL (invalid pollfd entry).  Descriptors may be checked for more
than one testable event.
	Once the descriptor set has been created, the poll() function can be
called to check for activity on the set.  A negative timeout value will cause
the function to block indefinitely, otherwise it represents a wait time given
in milliseconds.  The poll() function will return a negative number if an
error is encountered, zero if the call times out and no file descriptors have
been selected, or a positive number indicating the number of descriptors
which can be serviced without blocking.
.endhelp ______________________________________________________________________


# POLL_OPEN -- Open a poll descriptor set.

pointer procedure poll_open ()

pointer	fds

begin
	iferr (call calloc (fds, LEN_FPOLL, TY_STRUCT))
	    call syserr (SYS_MFULL)

	return (fds)
end


# POLL -- Poll the descriptor set.

int procedure poll (fds, nfds, timeout)

pointer	fds
int	nfds
int	timeout

int	pfds[LEN_FPOLL]
int	i, j, npoll, status

include	<fio.com>

begin
	# Transform the descriptor set to a linear array.
	j = 0 
	for (i=1; j < nfds; i=i+3) {
	     pfds[i  ] = FCHAN(fiodes[POLL_FD(fds,j)])
	     pfds[i+1] = POLL_EVENTS(fds,j)
	     pfds[i+2] = POLL_REVENTS(fds,j)
	     j = j + 1
	}

	# Call the kernel routine to poll on the descriptor set.
	call zfpoll (pfds, nfds, timeout, npoll, status)
	if (status == ERR)
	    return (ERR)

	j = 0 
	for (i=3; j < nfds; i=i+3) {
	     POLL_REVENTS(fds,j) = pfds[i]
	     j = j + 1
	}

	return (npoll)
end


# POLL_CLOSE -- Close and free a poll descriptor set.

procedure poll_close (fds)

pointer	fds					#i descriptor set pointer

begin
	call mfree (fds, TY_STRUCT)
end


# POLL_ZERO -- Zero the descriptor set.

procedure poll_zero (fds)

pointer	fds					#i descriptor set pointer

begin
	call aclri (Memi[fds], LEN_FPOLL)
end


# POLL_SET -- Add a descriptor to the set, and/or modify the event type.
# The type may be a bitwise or of testable events.

procedure poll_set (fds, fd, type)

pointer	fds					#i descriptor set pointer
int	fd					#i file descriptor
int	type					#i event type

int	i, top
int	ori()

begin
	top = POLL_NFD(fds)
	if (top > MAX_POLL_FD)
	    call eprintf ("File descriptor set overflow.\n")

	for (i=0; i < top; i=i+1) {
	    # Search for requested descriptor and OR the type on the event mask.
	    if (fd == POLL_FD(fds,i)) {
		POLL_EVENTS(fds,i) = ori (POLL_EVENTS(fds,i), type)
		return
	    }
	}

	# Descriptor not found, add it to the set at the top
	POLL_FD(fds,top) = fd
	POLL_EVENTS(fds,top) = ori (POLL_EVENTS(fds,top), type)
	POLL_NFD(fds) = top + 1
end


# POLL_GET_NFDS -- Get the number of descriptors in the set.

int procedure poll_get_nfds (fds)

pointer	fds					#i descriptor set pointer

begin
	return (POLL_NFD(fds))
end


# POLL_CLEAR -- Remove a descriptor or event type from the set.  The type
# may be a bitwise or of testable events.   If the event mask becomes NULL
# the descriptor is removed entirely from the set.

procedure poll_clear (fds, fd, type)

pointer	fds					#i descriptor set pointer
int	fd					#i file descriptor
int	type					#i event type

int	i, j, top
int	noti(), andi()

begin
	top = POLL_NFD(fds)

	for (i=0; i < top; i=i+1) {
	    # Search for requested descriptor.
	    if (fd == POLL_FD(fds,i)) {
		POLL_EVENTS(fds,i) = andi (noti(type), POLL_EVENTS(fds,i))
		
		# If there are no events, remove the descriptor from the set
		# by deleting it from the array and shifting the remainder.
		if (POLL_EVENTS(fds,i) == 0) {
		    for (j=i+1; i < top; j=j+1) {
		        POLL_FD(fds,i) = POLL_FD(fds,j)
		        POLL_EVENTS(fds,i) = POLL_EVENTS(fds,j)
		        i = i + 1
		    }
		    POLL_NFD(fds) = top - 1
		    break
		}
	    }
	}
end


# POLL_TEST -- Test the descriptor for the given event type.

int procedure poll_test (fds, fd, type)

pointer	fds					#i descriptor set pointer
int	fd					#i file descriptor
int	type					#i event type

int	i, top
int	andi()

begin
	top = POLL_NFD(fds)

	for (i=0; i < top; i=i+1) {
	    # Search for requested descriptor.
	    if (fd == POLL_FD(fds,i)) {
		# OR the type on the event mask.
		if (andi (POLL_REVENTS(fds,i), type) > 0)
		    return (YES)
		else
		    return (NO)
	    }
	}

	return (NO)
end


# POLL_PRINT -- Print the descriptor set (debug utility).

procedure poll_print (fds)

pointer	fds					#i descriptor set pointer

int	i, top

begin
	top = POLL_NFD(fds)

	for (i=0; i < top; i=i+1) {
	    call eprintf ("%2d:  fd=%3d  events=%6d  revents=%6d\n")
		call pargi(i)
		call pargi(POLL_FD(fds,i))
		call pargi(POLL_EVENTS(fds,i))
		call pargi(POLL_REVENTS(fds,i))
	}
end
