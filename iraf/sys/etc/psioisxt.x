# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<ctype.h>
include	<gio.h>


# PSIO_ISXMIT -- Test for a pseudofile directive.  Return XMIT, XFER, or DATA
# as the function value, and if we do have a pseudofile, decode the pseudofile
# number and char count.
#
# Syntax: "xmit(P,NNN)" or "xfer(P,NNN)"
#          12345678         12345678
#
# where P is the pseudofile code (0<P<10) and NNN is the size of the data block
# in chars.  In the following code all explicit integer constants refer to the
# character offsets shown above.

int procedure psio_isxmit (lbuf, pseudofile, nchars)

char	lbuf[ARB]		# text
int	pseudofile		# pseudofile code (output)
int	nchars			# block size (output)
int	line_type, ip
int	ctoi()
errchk	syserr

begin
	# Decode line type.  If we are called we have already determined that
	# lbuf[1] is 'x'.

	if (lbuf[2] == 'm') {
	    if (lbuf[3] == 'i' && lbuf[4] == 't' && lbuf[5] == '(')
		line_type = XMIT
	    else
		return (DATA)
	} else if (lbuf[2] == 'f') {
	    if (lbuf[3] == 'e' && lbuf[4] == 'r' && lbuf[5] == '(')
		line_type = XFER
	    else
		return (DATA)
	} else
	    return (DATA)
	
	# Get pseudofile code.
	ip = 6
	if (ctoi (lbuf, ip, pseudofile) <= 0)
	    call syserr (SYS_PRIPCSYNTAX)

	while (lbuf[ip] == ',' || IS_WHITE(lbuf[ip]))
	    ip = ip + 1

	# Get char size of data block.
	if (ctoi (lbuf, ip, nchars) <= 0)
	    call syserr (SYS_PRIPCSYNTAX)

	return (line_type)
end
