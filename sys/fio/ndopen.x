# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# NDOPEN -- Open a network device.  This is used by a client to connect to
# a server, or by a server to establish a port to which clients can connect.
# The open may or may not block until a client has connected, depending upon
# the type of connection.  The access mode should be NEW_FILE for a server
# connection, anything else is a client connection.   Most clients use mode
# READ_WRITE.  The connection is bidirectional and stream oriented.
#
# The syntax of the filename argument (network address) is determined by the
# host level ND driver.  The filename is passed on to the driver transparently
# to the portable IRAF code.  System independent IRAF code should treat these
# strings as data, like host filenames, and not attempt to parse or construct
# the strings.  Refer to the ND driver source for further information on the
# ND filename syntax.
#
# The host driver (os$zfiond.c) determines the types of network or
# interprocess connections supported.  For example, the initial ND driver for
# UNIX/IRAF systems supports Internet sockets, UNIX domain sockets, and FIFO
# pipes.
# 
# Since the same file descriptor is used for both reading and writing some is
# needed to synchronize data transfer.  When switching between reads and
# writes, the client code should execute a F_CANCEL on the stream before the
# first read or write of a sequence.  FLUSH should be called after the last
# write.  For example,
# 
# 	call fseti (fd, F_CANCEL, OK)
# 	call write (fd, buf, nchars)
# 	    <optional additional writes>
# 	call flush (fd)
# 
# 	call fseti (fd, F_CANCEL, OK)
# 	nchars = read (fd, buf, maxch)
# 	    <optional additional reads>
# 
# Any of the i/o routines may be used, e.g., getc/putc may be used to perform
# character i/o on the stream, with FIO doing the buffering.
#
# Once opened all ND connections are byte streams.  The protocol used for
# client-server communications is determined entirely by the server; an IRAF
# client may connect to a "foreign" server via an ND connection, so long
# as the correct client-server protocol is observed.  If the server supports
# multiple clients multiple ND connections may be made, either in the same
# process or in different processes.  An IRAF task using the ND interface
# may be a server, but currently the ND driver does not support multiple
# concurrent client connections, since the connection and i/o block.
# Multiple nonconcurrent (i.e. sequential) clients are possible.  Multiple
# conncurent connections are possible only if a scheme is used such as having
# inetd spawn a server process for each connection.

int procedure ndopen (fname, mode)

char	fname[ARB]		#I network address
int	mode			#I access mode

int	fopnbf()
extern	zopnnd(), zardnd(), zawrnd(), zawtnd(), zsttnd(), zclsnd()

begin
	return (fopnbf (fname, mode,
	    zopnnd, zardnd, zawrnd, zawtnd, zsttnd, zclsnd))
end
