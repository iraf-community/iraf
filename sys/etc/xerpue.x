# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<mach.h>
include	<fio.h>

# XER_PUTLINE -- Put a line to the output file (STDERR), using only low level
# routines.  It is important to use run time indirection through the device
# table here, to avoid linking the entire IPC and KI into non-IRAF programs
# that use error handlers, e.g., HSI or IMFORT programs.

procedure xer_putline (fd, text)

int	fd
char	text[ARB]

long	offset
int	nchars, junk
int	strlen()
include	<fio.com>

begin
	nchars = strlen (text)
	fp = fiodes[fd]

	if (FTYPE(fp) == BINARY_FILE) {
	    offset = 0
	    call zcall4 (ZAWRBF(fp), FCHAN(fp), text, nchars * SZB_CHAR, offset)
	    call zcall2 (ZAWTBF(fp), FCHAN(fp), junk)
	} else
	    call zcall4 (ZPUTTX(fp), FCHAN(fp), text, nchars, junk)
end
