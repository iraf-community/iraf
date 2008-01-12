# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<config.h>
include	<fio.h>

# CLOSE -- Close a file, after possibly flushing the output buffer, and
# returning the file buffer (if any) and file descriptor.

procedure close (fd_arg)

int	fd_arg, fd
int	status
errchk	flush, mfree, frtnfd
include	<fio.com>

begin
	fp = fiodes[fd_arg]
	if (fp == NULL)
	    return
	else
	    call fcanpb (fd_arg)		# cancel any pushback

	if (redir_fd[fd_arg] > 0) {
	    # If the stream was redirected locally onto a new file by FREDIR,
	    # swap streams back to their original order and close the redir
	    # file.

	    fd = redir_fd[fd_arg]
	    call flush (fd_arg)
	    call fswapfd (fd, fd_arg)
	    redir_fd[fd_arg] = 0
	} else
	    fd = fd_arg

	switch (fd) {
	case STDIN, CLIN:
	    return
	case STDOUT, STDERR, CLOUT, STDGRAPH, STDIMAGE, STDPLOT:
	    call flush (fd)

	default:
	    call flush (fd)
	    status = OK

	    switch (FTYPE(fp)) {
	    case TEXT_FILE:
		call zcall2 (ZCLSTX(fp), FCHAN(fp), status)
		call frtnfd (fd)
	    case STRING_FILE:
		call strclose (fd)
	    case SPOOL_FILE:
		call frtnfd (fd)

	    default:
		FREFCNT(fp) = FREFCNT(fp) - 1
		if (FREFCNT(fp) <= 0) {
		    if (FCHAN(fp) != ERR)
			call zcall2 (ZCLSBF(fp), FCHAN(fp), status)

		    if (FCD(fp) != FLCD(fp))		# separate chandes?
			call mfree (FCD(fp), TY_STRUCT)
		}
		call frtnfd (fd)
	    }

	    if (status == ERR)
		call filerr (FNAME(fp), SYS_FCLOSE)
	}
end
