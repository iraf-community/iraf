# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<fset.h>
include	<error.h>
include	<fio.h>

# FIO_CLEANUP -- Clean up FIO after a crash, or upon normal termination of
# a task.  Flush all open files (harmless on read only files, closed files).
# Close all open user files, unless the KEEP flag bit is set.  Delete any
# partial new files, and all temporary files.  Since this routine is called
# during error restart, convert any errors into warning messages to avoid an
# infinite loop.

procedure fio_cleanup (status)

int	status

int	fd
bool	stddev
int	mode, ref_count
include	<fio.com>
errchk	close

begin
	call flush (STDERR)
	call fio_qflush (STDOUT,   status)
	call fio_qflush (STDGRAPH, status)
	call fio_qflush (STDIMAGE, status)
	call fio_qflush (STDPLOT,  status)

	for (fd=1;  fd < FIRST_FD;  fd=fd+1) {
	    # Cancel any pushback on the standard streams.
	    if (and (fflags[fd], FF_PUSHBACK) != 0)
		call fcanpb (fd)

	    # If any of the standard streams have been redirected locally (>0),
	    # cancel the redirection and close the redirection files.
	    # If streams were redirected by parent (<0), cancel the flag as
	    # the duration of the flag is only until task termination.

	    if (redir_fd[fd] > 0) {
		iferr (call close (fd))
		    ;
	    } else if (redir_fd[fd] < 0)
		redir_fd[fd] = 0
	}

	# Restore the default no flush on newline attribute to STDOUT.
	call fseti (STDOUT, F_FLUSHNL, NO)

	# Delete any files opened TEMP_FILE during program execution.
	iferr (call frmtmp())
	    call erract (EA_WARN)

	# Close all open user files unless the F_KEEP (keep open) flag has
	# been set.

	for (fd=FIRST_FD;  fd <= LAST_FD;  fd=fd+1) {
	    fp = fiodes[fd]

	    if (fp != NULL) {				# file open?
		# Do nothing if file is to be kept open.
		if (and (FF_KEEP, fflags[fd]) != 0)
		    next

		# Do not try to flush the output of a string file, or it
		# will cause error recursion.   The mode of the string file
		# is reset to READ_ONLY to avoid writing the EOS at the end
		# of the string buffer, as if the file is being closed during
		# cleanup following task termination (which should not
		# normally be the case) the buffer may no longer exist.

		if (FTYPE(fp) == STRING_FILE) {
		    call strsetmode (fd, READ_ONLY)
		    call close (fd)
		    next
		} else if (FTYPE(fp) == SPOOL_FILE) {
		    call close (fd)
		    next
		}

		iferr (call fio_qflush (fd, status))
		    call erract (EA_WARN)		# keep open?

		stddev = (FDEV(fp)==TX_DRIVER || FDEV(fp)==BF_DRIVER)
		call strcpy (FNAME(fp), pathname, SZ_PATHNAME)
		ref_count = FREFCNT(fp) - 1
		mode = FMODE(fp)

		iferr {
		    call close (fd)

		    # Delete any new files that have been only partially
		    # written into.

		    if (stddev && mode == NEW_FILE && ref_count <= 0)
			call delete (pathname)
		} then
		    call erract (EA_WARN)
	    }
	}
end


# FIO_QFLUSH -- If cleanup is being performed following normal task completion
# (status is OK), flush any buffered output to file.  If cleanup occurs during
# error restart, cancel any buffered output.

procedure fio_qflush (fd, status)

int	fd, status
pointer	bp
include	<fio.com>

begin
	if (status == OK) {
	    # Flush any buffered output.
	    call flush (fd)

	} else {
	    # Cancel any buffered output.
	    call fcanpb (fd)

	    bp = bufptr[fd]
	    itop[fd] = bp
	    otop[fd] = bp
	    iop[fd] = bp
	}
end
