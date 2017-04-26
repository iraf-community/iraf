# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<printf.h>

# SPRINTF: Open string as a file, call fprntf.  When the last argument is
# passed, and EOS is reached, the string will be closed (by fpradv).

procedure sprintf (outstr, maxch, format_string)

char	outstr[maxch]
int	maxch
char	format_string[ARB]
int	mem_fd, stropen()
errchk	stropen, fprntf

begin
	mem_fd = stropen (outstr, maxch, WRITE_ONLY)
	call fprntf (mem_fd, format_string, STRING_FILE)
end
