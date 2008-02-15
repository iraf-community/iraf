# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<finfo.h>
include	<time.h>
include <ctype.h>
include	"help.h"

# PR_FILE -- Print a file.  Called to print menu files and source files.
# Do not print block header, but do page output if enabled by user.

procedure pr_file (fname, ctrl, pakname)

char	fname[ARB]
pointer	ctrl
char	pakname[ARB]

int	center_col, ip
long	fi[LEN_FINFO]
pointer	sp, lbuf, time
int	open(), hinput(), strlen(), finfo()
errchk	houtput, open, hinput

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)
	call salloc (time, SZ_TIME, TY_CHAR)

	H_EOF(ctrl) = NO
	H_RAWIN(ctrl) = YES
	center_col = (H_LMARGIN(ctrl) + H_RMARGIN(ctrl)) / 2

	# Clear screen and print filename if printing source file.
	# Filename is printed centered between left and right margins.
	# Note header must be two lines for man_output().

	if (H_OPTION(ctrl) == O_SOURCE) {
	    if (finfo (fname, fi) == ERR) {
		call sprintf (Memc[lbuf], SZ_LINE, "%s `%s'")
		    call pargstr ("Cannot get file info for file")
		    call pargstr (fname)
		call error (20, Memc[lbuf])
	    }
	    call cnvtime (FI_MTIME(fi), Memc[time], SZ_TIME)

	    # Header format: "FILE  date  FILE".
	    call sprintf (Memc[lbuf], SZ_LINE, "%*t%s %*t%s %*t%s\n")
		call pargi   (H_LMARGIN(ctrl))
		call pargstr (fname)
		call pargi   (center_col - strlen(Memc[time]) / 2)
		call pargstr (Memc[time])
		call pargi   (H_RMARGIN(ctrl) - strlen (fname) + 1)
		call pargstr (fname)

	    call houtput (ctrl, "\f")
	    call houtput (ctrl, Memc[lbuf])
	    call houtput (ctrl, "\n")
	}

	# Use hinput/houtput to read and print file so that output is
	# paginated.
	H_IN(ctrl) = open (fname, READ_ONLY, TEXT_FILE)

	while (hinput (ctrl, Memc[lbuf]) != EOF) {
	    if (H_OPTION(ctrl) == O_REFERENCES) {
		# Replace the newline character.
		ip = strlen (Memc[lbuf]) - 1
		Memc[lbuf+ip] = ' '

		# Append the package name.
		call strcat (" [", Memc[lbuf], SZ_LINE)
		call strcat (pakname, Memc[lbuf], SZ_LINE)
		call strcat ("]\n", Memc[lbuf], SZ_LINE)

		# Strip leading whitespace.
		for (ip=0; IS_WHITE(Memc[lbuf+ip]); ip=ip+1)
		    ;
	        call houtput (ctrl, Memc[lbuf+ip])
	    } else
	        call houtput (ctrl, Memc[lbuf])
	}

	call close (H_IN(ctrl))
	call sfree (sp)
end
