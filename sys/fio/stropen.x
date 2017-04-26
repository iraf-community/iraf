# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<fio.h>

# STROPEN, STRCLOSE -- Open/close a character string for file i/o.  Called by
# sprintf (for example) to make the output string look like a file.
# 
# The string is made to appear to be the file buffer.  It is a fatal error
# if the string is not char aligned with Mem.  If the output string should
# overflow, FIO will call FLSBUF, resulting in a system error action.  STROPEN,
# for efficiency reasons, does not really open a file, it merely validates the
# buffer pointers and reserves a file descriptor.  Note that the buffer
# pointers may be negative.  Seeks are illegal on a string file, and will
# cause an error action to be taken.
# 
# If alignment is not automatically guaranteed for char data on your machine,
# define "stropen" as "memopen($1,$2,$3,TEXT_FILE)", and "strclose" as "close",
# in <iraf.h> (or install a dummy procedure in the library).

int procedure stropen (str, maxch, mode)

char	str[ARB]			#I string buffer for i/o
int	maxch				#I capacity of buffer
int	mode				#I FIO access mode

pointer	bp
int	fd, ip, loc_str, loc_Mem
errchk	syserr
include	<fio.com>

begin
	# Find an unused file descriptor.
	for (fd=FIRST_FD;  fd <= LAST_FD && fiodes[fd] != NULL;  fd=fd+1)
	    ;
	if (fd > LAST_FD)
	    call syserr (SYS_FTOOMANYFILES)

	# Compute pointer (Memc index) to the string.
	call zlocva (str,  loc_str)
	call zlocva (Memc, loc_Mem)
	bp = loc_str - loc_Mem + 1

	# Get file descriptor and init the buffer pointers.
	call fstrfp (fiodes[fd])
	call strsetmode (fd, mode)
	bufptr[fd] = bp
	buftop[fd] = bp + maxch
	fflags[fd] = 0

	# If string is being opened in any of the following modes, it
	# must be an initialized (written into) string with an EOS.
	# Find EOS and set itop accordingly.

	if (mode == READ_ONLY || mode == READ_WRITE || mode == APPEND)
	    for (ip=1;  str[ip] != EOS && ip <= maxch;  ip=ip+1)
		;

	# Seeks are illegal on strings.  Modes RO and RW are equivalent, as
	# are WO, NF, and TF.  Append is like WO/NF/TF, but the i/o pointer is
	# positioned at the EOS.  An EOS will automatically be written when
	# a file opened with mode WO, NF, TF, or AP is closed.

	iop[fd]  = bp
	itop[fd] = bp
	otop[fd] = bp + maxch

	switch (mode) {
	case READ_ONLY, READ_WRITE:
	    itop[fd] = bp + ip - 1
	    otop[fd] = bp
	case APPEND:
	    iop[fd] = bp + ip - 1
	}

	return (fd)
end


# STRCLOSE -- Close a string file previously opened by STROPEN.  If writing
# to a new string, append an EOS to the end of the string.  This routine is
# automatically called by CLOSE if the string was opened as a file with
# STROPEN.  Applications should call CLOSE, instead of calling STRCLOSE
# directly, to ensure that the file descriptor allocated by STROPEN and FIO
# is fully closed.

procedure strclose (fd)

int	fd				#I file descriptor
int	strgetmode()
errchk	syserr
include	<fio.com>

begin
	if (fd < 0 || fiodes[fd] == NULL)
	    call syserr (SYS_FILENOTOPEN)

	# Free any file pushback.
	call mfree (FPBBUF(fiodes[fd]), TY_CHAR)

	# If string was opened for writing, append EOS.  NOTE that if the
	# string was opened with length N, the EOS will go into location N+1
	# if the string is completely full.

	switch (strgetmode(fd)) {
	case WRITE_ONLY, APPEND, NEW_FILE, TEMP_FILE:
	    Memc[iop[fd]] = EOS
	default:
	    ;
	}

	# Free the file descriptor.
	bufptr[fd] = NULL
	fiodes[fd] = NULL
end


# STRSETMODE -- Set the access mode for a string file.  This is an internal
# routine normally called only by STROPEN above.  It may also called during
# task termination and cleanup to change the string file access mode to avoid
# an attempt to EOS terminate the string buffer, before closing off any still
# open string files

procedure strsetmode (fd, mode)

int	fd				#I file descriptor
int	mode				#I file access mode
include	<fio.com>

begin
	# For a string file the access mode is arbitrarily saved in BOFFSET
	# for CLOSE (strclose), which needs to know the access mode in order
	# to append an EOS.  BOFFSET is not otherwise used for string files
	# since the string buffer has no associated file offset.

	boffset[fd] = mode
end


# STRGETMODE -- Get the access mode for a string file.  This is an internal
# routine normally called only by STRCLOSE.

int procedure strgetmode (fd)

int	fd				#I file descriptor
include	<fio.com>

begin
	return (boffset[fd])
end
