# POLL.H -- FPOLL definitions (poll a set of file descriptors).

define  MAX_POLL_FD    	32              # max number of polling fds
define  MPFD		MAX_POLL_FD    	# utility shorthand
define	LEN_FPOLL	(4+(3*MPFD))	# length of polling structure

define	INFTIM		-1		# poll will block until event


# Structure of file descriptor/event pairs supplied in the poll arrays. The
# array elements are zero-indexed.
define	POLL_NFD	Memi[$1]	       # no. of file descriptors to poll
define	POLL_FD		Memi[$1+1+(0*MPFD)+$2] # file descriptor array
define	POLL_EVENTS	Memi[$1+1+(1*MPFD)+$2] # events of interest on fd
define	POLL_REVENTS	Memi[$1+1+(2*MPFD)+$2] # (return) events on fd

# Testable select events.
define 	POLLIN		001B		# fd is readable
define 	POLLPRI		002B		# priority info at fd
define 	POLLOUT         004B		# fd is writeable (won't block)

# Non-testable poll events (may not be specified in events field,
# but may be returned in revents field).
define POLLERR		010B		# fd has error condition
define POLLHUP		020B		# fd has been hung up on
define POLLNVAL		040B		# invalid pollfd entry
