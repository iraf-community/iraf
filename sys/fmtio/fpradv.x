# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<chars.h>
include	<printf.h>

# FPRADV -- Copy format chars to output until next "%w.dC" format sequence is
# encountered, or until EOS is encountered on format string.  When EOS is
# encountered, return buffer containing format string, and if mem_flag is set,
# close the output file (a string) as well.  If a format string contains no
# regular format sequences, and hence requires no PARG_ calls, we are all done.

procedure fpradv()

int	i, junk, ival, ch
char	cch
int	ip_save
int	ctoi(), cctoc()
include "fmt.com"
errchk	putci

begin
	for (ch = format[ip];  ch != EOS;  ch = format[ip]) {
	    cch = ch
	    if (ch == ESCAPE) {
		junk = cctoc (format, ip, cch)

	    } else if (ch == START_OF_FORMAT) {
		if (format[ip+1] == START_OF_FORMAT)	# '%%' --> '%'
		    ip = ip + 2

		else if (IS_DIGIT (format[ip+1])) {	# %Nw or %Nt
		    ip_save = ip			# ip_save --> '%'
		    ip = ip + 1

		    junk = ctoi (format, ip, ival)

		    switch (format[ip]) {
		    case FMT_WHITESPACE:		# output blanks
			do i = 1, ival
			    call putci (fd, BLANK)
			col = col + ival
		    case FMT_TOCOLUMN:			# advance to column
			for (;  col < ival;  col=col+1)
			    call putci (fd, BLANK)
		    default:
			ip = ip_save			# regular format spec
			return
		    }

		    ip = ip + 1				# eat "t" or "w"
		    next

		} else
		    return				# regular format spec

	    } else
		ip = ip + 1

	    call putc (fd, cch)				# output ordinary chars
	    if (IS_PRINT (cch))				# keep track of column
		col = col + 1
	    else
		call fmt_setcol (cch, col)
	}

	switch (ofile_type) {				# EOS of format reached
	case STRING_FILE:
	    call close (fd)
	case CL_PARAM:
	    call putline (CLOUT, "\"\n")
	}

	ofile_type = REGULAR_FILE			# restore default
	fd = NULL
end
