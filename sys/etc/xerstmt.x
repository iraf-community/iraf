# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	<ctype.h>

define	SZ_NUMBUF	6

# XERSTMT -- Format and issue an error statement to the CL.  Note that this is
# a command issued to the CL, not a line written to STDERR.  The error code and
# error message string output were posted in the last call to ERROR or FATAL.
# 
# Example:	ERROR (501, "Access Violation")
# 
# The actual concatentation and transmission of the error message is carried
# out by the primitive XERPUTC, rather than by PUTLINE and PUTC calls to CLOUT,
# to avoid recursion in the FIO routines, probably leading to error recursion.

procedure xer_send_error_statement_to_cl (errcode)

int	errcode
char	numbuf[SZ_NUMBUF]
int	ip, junk, itoc()
include	"error.com"

begin
	# The error code is passed as an argument rather than taken from the
	# xercom common because XERPOP clears the error code before we are
	# called by the IRAF Main.

	junk = itoc (errcode, numbuf, SZ_NUMBUF)

	# Format the ERROR statement and sent it to the CL.

	call xerpstr ("ERROR (")
	call xerpstr (numbuf)
	call xerpstr (", \"")

	# Output error message string, omitting characters like newline or
	# quote which could cause syntax problems.

	for (ip=1;  xermsg[ip] != EOS;  ip=ip+1)
	    if (IS_PRINT (xermsg[ip]) && xermsg[ip] != '"')
		call xerputc (xermsg[ip])

	# Ring terminal bell if unexpected error (anything other than
	# a keyboard interrupt).

	if (xercod != SYS_XINT)
	    call xerpstr ("\7")
	call xerpstr ("\")\n")
end


# XERPSTR -- Put a string to the CL (special routine, to avoid recursion).
# Use PUTLINE in normal code.

procedure xerpstr (str)

char	str[ARB]
int	ip

begin
	for (ip=1;  str[ip] != EOS;  ip=ip+1)
	    call xerputc (str[ip])
end
