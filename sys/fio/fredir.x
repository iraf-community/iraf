# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<config.h>
include	<fio.h>

# FREDIR -- Redirect the i/o to stream FD onto file FNAME.  FD must be the file
# descriptor of an open file which has not already been redirected.  The mode
# of access and file type of the redirection file need not agree with those of
# the stream being redirected.
#
# The "redir_fd" file parameter has the following meaning:
#	redir_fd[fd] <  0	i/o redirected in the parent process
#	redir_fd[fd] == 0	i/o is not redirected
#	redir_fd[fd] >  0	i/o has been redirected in the local process
#				  to file FD redir_fd[fd]

procedure fredir (fd, fname, mode, type)

int	fd			# stream to be redirected
char	fname[ARB]		# name of redirection file
int	mode			# access mode of redirection file
int	type			# file type of redirection file

int	newfd, junk
int	open(), itoc()
include	<fio.com>
errchk	open, syserrs

begin
	# Cancel any pushback on the open file.
	call fcanpb (fd)

	# Verify that file FD is open and has not already been redirected.

	junk = itoc (fd, pathname, SZ_PATHNAME)
	if (fiodes[fd] == NULL)
	    call syserrs (SYS_FREDIRFNO, pathname)
	else if (redir_fd[fd] != NULL)
	    call syserrs (SYS_FMULTREDIR, pathname)

	# Open the redirection file and swap file descriptors.  CLOSE will
	# automatically swap back and close the redirection file.

	newfd = open (fname, mode, type)
	call frediro (fd, newfd)
end


# FREDIRO -- Redirect a stream to another stream which has already been
# opened.

procedure frediro (fd, newfd)

int	fd			# stream to be redirected
int	newfd			# where it is to be redirected
include	<fio.com>

begin
	call fswapfd (fd, newfd)
	redir_fd[fd] = newfd
end
