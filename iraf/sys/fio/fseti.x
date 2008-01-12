# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<error.h>
include	<mach.h>
include	<fset.h>
include	<fio.h>

# FSETI -- Set File I/O options.  FSETI is not intended to be called
# routinely by user code.  To provide maximum flexibility, any FIO parameter
# can be changed, and no checking is performed.  Hence, the file buffer size,
# device, device channel, and so on can be changed during i/o to a file,
# though doing so is probably an error.  User beware.
# 
# The only FIO parameters that should be set in applications programs are
# F_FLUSHNL (flush output at end of every line of text), and occasionally
# F_ADVICE (sequential or random).

procedure fseti (fd, param, value)

int	fd			# file in question
int	param			# parameter to be set
int	value			# value of parameter

pointer	bp, ffp
long	file_offset
int	i, junk, outfd, flags
bool	blocked_file, setraw, ndelay
char	set_redraw[LEN_SETREDRAW]
char	rawcmd[LEN_RAWCMD+1]

int	await(), xisatty()
include	<fio.com>

begin
	ffp = fiodes[fd]
	if (fd <= 0 || ffp == NULL)
	    iferr (call syserr (SYS_FILENOTOPEN))
		call erract (EA_FATAL)

	switch (param) {

	case F_ADVICE:
	    # Set file buffer size, based on expected type of i/o.  By default,
	    # the device dependent OPTBUFSIZE is used.  If i/o is expected to
	    # be very random, an integral multiple of the device block size is
	    # used (beware of 1 char block files).  If highly sequential i/o is
	    # expected, a large buffer is allocated.

	    if (fd >= FIRST_FD && bufptr[fd] == NULL)
		switch (value) {
		case RANDOM:
		    FBUFSIZE(ffp) = LEN_RANDBUF * max (1, FBLKSIZE(ffp))
		case SEQUENTIAL:
		    FBUFSIZE(ffp) = LEN_SEQBUF * FOPTBUFSIZE(ffp)
		default:
		    FBUFSIZE(ffp) = FOPTBUFSIZE(ffp)
		}
	    
	case F_ASYNC:
	    # Enable asynchronous i/o.
	    ;						# not implemented

	case F_BLKSIZE:
	    # Set the device block size in chars.
	    FBLKSIZE(ffp) = value

	case F_BUFPTR, F_BUFSIZE:
	    # An externally created buffer can be installed by setting F_BUFPTR
	    # and either F_BUFSIZE or F_BUFTOP (do NOT forget to set both).
	    # The file buffer size can be changed by a call to F_BUFSIZE,
	    # even after doing i/o on a file. In both cases, the current file
	    # offset will be retained.

	    if (param == F_BUFSIZE && FBUFSIZE(ffp) == value)
		return
	    else if (bufptr[fd] != NULL) {
		call flush (fd)
		call frmbfs (fd)
	    }

	    if (param == F_BUFSIZE) {
		FBUFSIZE(ffp) = value
		if (buftop[fd] == NULL && bufptr[fd] != NULL)
		    buftop[fd] = bufptr[fd] + value
	    } else {
		file_offset = LNOTE (fd)
		bufptr[fd] = value
		boffset[fd] = NULL
		LSEEK (fd, file_offset)
		if (buftop[fd] == NULL && FBUFSIZE(ffp) != NULL)
		    buftop[fd] = bufptr[fd] + FBUFSIZE(ffp)
	    }

	case F_BUFTOP:
	    # Set a pointer to the top of a buffer (first char after buffer).
	    buftop[fd] = value
	    if (FBUFSIZE(ffp) == NULL && bufptr[fd] != NULL)
		FBUFSIZE(ffp) = buftop[fd] - bufptr[fd]

	case F_FILESIZE:
	    # Set the file size.  Should not be called by ordinary programs;
	    # intended for use only in system code which writes to a file at
	    # the kernel level, preventing FIO from keeping track of the file
	    # size.

	    FILSIZE(ffp) = value

	case F_FIRSTBUFOFF:
	    # FIO divides a random access binary file up into a series of
	    # fixed size buffers, or file "pages".  By default the first buffer
	    # is at file offset BOF=1, but this does not have to be the case,
	    # and sometimes it is desirable to align the file buffers starting
	    # at some format specific offset in the file.  Note that doing so
	    # renders the file segment to the left of FIRSTBUFOFF inaccessible.

	    call flush (fd)
	    call frmbfs (fd)
	    FIRSTBUFOFF(ffp) = value

	case F_BUFTYPE:
	    # Use file-local buffers or the global pool.
	    ;						# not implemented

	case F_CANCEL:
	    # Cancel any buffered data.  For a blocked file, the file offset
	    # is preserved, hence the only effect is to force the file buffer
	    # to be refilled.  Any changes made to buffered data are cancelled
	    # when the buffer is refilled.  For a streaming file, the i/o
	    # pointers are reset to the beginning of the buffer, to force the
	    # next read to refill the buffer, or to cause the next write to
	    # start filling the buffer.

	    call fcanpb (fd)
	    blocked_file = (FBLKSIZE(ffp) > 0)
	    if (blocked_file)
		file_offset = LNOTE(fd)

	    bp = bufptr[fd]
	    if (FFIOMODE(ffp) != INACTIVE)
		junk = await (fd)

	    iop[fd] = bp
	    itop[fd] = bp
	    otop[fd] = bp
	    
	    if (blocked_file) {
		boffset[fd] = 0		# invalidate buffer
		LSEEK (fd, file_offset)
	    } else
		boffset[fd] = 1		# causes rewind to set iop=bp

	    FILSTAT(ffp) = value

	case F_CHANNEL:
	    # Kernel i/o channel number.
	    FCHAN(ffp) = value

	case F_CLOBBER:
	    # Allow NEW_FILE files to overwrite old files of the same name.
	    call fset_env ("clobber", value)

	case F_CLOSEFD:
	    # If this option is set for a file descriptor, the host channel
	    # is reopened every time an i/o operation takes place (aread or
	    # awrite), and is closed while the channel is inactive.  This
	    # is useful to save host system file descriptors, so that a
	    # program may have a very large number of files "open" at any one
	    # time (one is still limited by the maximum number of available
	    # FIO file descriptors).  This option is supported only for binary
	    # files opened in some mode other than NEW_FILE; special devices
	    # are supported, but only if the device is randomly accessible and
	    # does not map file segments in to memory.

	    if (value == YES && FCLOSEFD(ffp) == NO) {
		if (FTYPE(ffp) == TEXT_FILE)
		    iferr (call filerr (FNAME(ffp), SYS_FCLFDTX))
			call erract (EA_FATAL)
		if (FMODE(ffp) == NEW_FILE)
		    iferr (call filerr (FNAME(ffp), SYS_FCLFDNF))
			call erract (EA_FATAL)

		if (FCHAN(ffp) != ERR) {
		    call zcall2 (ZCLSBF(ffp), FCHAN(ffp), junk)
		    FCHAN(ffp) = ERR
		}

		FCLOSEFD(ffp) = YES

	    } else if (value == NO) {
		# Wait until the next i/o operation occurs to reopen the file.
		FCLOSEFD(ffp) = NO
	    }

	case F_DEVICE:
	    # Set entry point address of the read entry point of the device
	    # driver for a file.

	    for (i=1;  i < next_dev;  i=i+LEN_DTE)
		if (value == zdev[i]) {
		    FDEV(ffp) = i
		    return
		}
	    iferr (call filerr (FNAME(ffp), SYS_FDEVNOTFOUND))
		call erract (EA_FATAL)

	case F_FILEWAIT:
	    # Wait for a file to become accessible during open.
	    call fset_env ("filewait", value)

	case F_FLUSHNL:
	    # Flush output when newline is seen.
	    if (value == YES)
		fflags[fd] = or (FF_FLUSH + FF_FLUSHNL, fflags[fd])
	    else if (FTYPE(ffp) == TEXT_FILE)
		fflags[fd] = and (not(FF_FLUSHNL), fflags[fd])
	    else
		fflags[fd] = and (not(FF_FLUSH + FF_FLUSHNL), fflags[fd])

	case F_KEEP:
	    # Keep a file open after program termination.
	    if (value == YES)
		fflags[fd] = or (FF_KEEP, fflags[fd])
	    else
		fflags[fd] = and (not(FF_KEEP), fflags[fd])

	case F_MODE:
	    # Set access mode of a file.
	    switch (value) {
	    case READ_ONLY:
		fflags[fd] = and (not(FF_WRITE), fflags[fd])
	    case READ_WRITE:
		fflags[fd] = or (FF_READ + FF_WRITE, fflags[fd])
	    case WRITE_ONLY:			# disable buf read in ffault
		fflags[fd] = and (not(FF_READ), fflags[fd])
	    default:
		iferr (call filerr (FNAME(ffp), SYS_FILLEGMODE))
		    call erract (EA_FATAL)
	    }
	    FMODE(ffp) = value

	case F_NBUFS:
	    # Set the number of i/o buffers for a file.
	    ;						# not implemented

	case F_ONEVERSION:
	    # Keep only one version of each file (in UNIX fashion, as opposed
	    # to the multiple versions of VMS).
	    call fset_env ("multversions", value)

	case F_PBBSIZE:
	    # Set the push-back buffer size for a file.
	    if (FPBBUF(ffp) == NULL)
		FPBBUFSIZE(ffp) = value

	case F_TYPE:
	    # Set the file type (text, binary, etc).
	    FTYPE(ffp) = value

	    if (value == TEXT_FILE) {
		fflags[fd] = or (FF_FLUSH, fflags[fd])

	    } else if (value == SPOOL_FILE) {
		# Reading and writing must be disabled for spool file or
		# filbuf and flsbuf won't work.  Also set the block size
		# to zero, since spool files are considered to be streaming
		# files.

		fflags[fd] = 0
		FBLKSIZE(ffp) = 0
	    }

	case F_IOMODE, F_RAW:
	    # Set the i/o mode for reading from a text device.  In raw mode,
	    # if the text device is a terminal, each character is returned as
	    # it is typed and most control characters are passed through on
	    # reads.  If nonblocking raw mode is selected, each read will
	    # return immediately whether or not there is any data to be read.
	    # If no data could be read EOF is returned, but in RAWNB mode
	    # this indicates merely that no input data was available.

	    setraw = (and (value, IO_RAW) != 0)
	    ndelay = (and (value, IO_NDELAY) != 0)

	    fflags[fd] = and (not(FF_RAW+FF_NDELAY), fflags[fd])
	    if (setraw) {
		flags = FF_RAW
		if (ndelay)
		    flags = flags + FF_NDELAY
		fflags[fd] = or (fflags[fd], flags)
	    }

	    # Send a special control sequence to the IRAF tty driver to
	    # physically turn raw mode on or off.  If this were not done then
	    # raw mode would not be turned off until the next read occurred,
	    # which is counter intuitive and dangerous, as an abort might
	    # occur before the read, leaving the terminal in never never
	    # land.  The funny string must of course agree with what ZPUTTY
	    # expects.

	    if (fd == STDIN || fd == STDOUT || fd == STDERR)
		outfd = STDOUT
	    else
		outfd = fd

	    if (xisatty (outfd) == YES && and (fflags[outfd], FF_WRITE) != 0) {
		call flush (outfd)

		if (setraw) {
		    call strcpy (RAWON, rawcmd, LEN_RAWCMD)
		    if (ndelay)
			rawcmd[LEN_RAWCMD+1] = 'N'
		    else
			rawcmd[LEN_RAWCMD+1] = 'B'
		    rawcmd[LEN_RAWCMD+2] = EOS
		    call putline (outfd, rawcmd)
		} else
		    call putline (outfd, RAWOFF)

		call flush (outfd)
	    }

	case F_SETREDRAW:
	    # Set the value of, and enable transmission of, the redraw control
	    # code to be issued following process suspension while in raw mode.
	    # Following a process suspend/continue while in raw mode, this code
	    # will be returned to the applications process in the next GETC
	    # call, as if it had been typed by the user.  The redraw control
	    # code must be set to some positive nonzero value to enable
	    # transmission of the code by the terminal driver.  Setting the
	    # code to zero disables the feature.  The terminal driver (ZFIOTY)
	    # for host systems which do not support process suspension will
	    # recognize but ignore this control sequence.  Note that the redraw
	    # control code to be returned by the driver is limited to a single
	    # character, e.g., <ctrl/l> or <ctrl/r>, depending upon the
	    # application.

	    if (fd == STDIN || fd == STDOUT || fd == STDERR)
		outfd = STDOUT
	    else
		outfd = fd

	    if (xisatty (outfd) == YES && and (fflags[outfd], FF_WRITE) != 0) {
		call strcpy (SETREDRAW, set_redraw, LEN_SETREDRAW)
		set_redraw[LEN_SETREDRAW] = value
		call flush (outfd)
		call write (outfd, set_redraw, LEN_SETREDRAW)
		call flush (outfd)
	    }

	case F_REDIR:
	    # Set redir_fd to a negative value to indicate that the stream
	    # has been redirected in the parent process.  If redir_fd is
	    # already set to a nonzero value, indicating that i/o has already
	    # been redirected either locally or in the parent, do nothing.

	    if (value == YES) {
		if (redir_fd[fd] == 0)
		    redir_fd[fd] = -1
	    } else
		redir_fd[fd] = 0

	case F_VALIDATE:
	    # Validate the contents of the FIO buffer, e.g., after an i/o
	    # error has occurred during AREAD but it is thought that at least
	    # part of the data in the buffer may be valid.  VALUE is the
	    # number of chars for which the FIO buffer is to be validated
	    # in the next call to FILBUF.  This must be the only case for
	    # which FNCHARS can take on a negative value.

	    FNCHARS(ffp) = -value
	    FNBYTES(ffp) = value * SZB_CHAR
	    FILSTAT(ffp) = OK

	default:
	    # This is a fatal error to prevent error recursion.
	    iferr (call filerr (FNAME(ffp), SYS_FSETUKNPAR))
		call erract (EA_FATAL)
	}
end


# FSET_ENV -- Set the value of a boolean environment variable used for file
# control.  A set environment call affects all programs in the current process
# and in all subprocesses, unless overriden by another SET statement or
# forgotten by ENVFREE.

procedure fset_env (envvar, value)

char	envvar[ARB]		# name of environment variable to be set
int	value			# YES or NO
int	junk
int	envputs()

begin
	switch (value) {
	case YES:
	    junk = envputs (envvar, "yes")
	case NO:
	    junk = envputs (envvar, "no")
	}
end
