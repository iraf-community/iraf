# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<chars.h>
include	<fset.h>
include	"tty.h"

# TTYINIT -- Initialize the terminal.  The termcap entry potentially contains
# two initialization entries.  The first, "is" is an initialization string
# which is sent to the terminal.  The second, "if", is the name of a file
# containing the initialization string.  If both are given, "if" is sent
# followed by "is", however, there seems no reason to have an "is" string
# if there is already an initialization file.  The names of initialization
# files may be either IRAF virtual filenames or host system pathnames.

procedure ttyinit (fd, tty)

int	fd			# file descriptor of terminal
pointer	tty			# tty descriptor

pointer	sp, fname
int	in, junk, rawmode
int	ttyctrl(), ttygets(), open(), fstati()
errchk	ttygets, fcopyo

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)

	# Output contents of initialization file, if any.
	if (ttygets (tty, "if", Memc[fname], SZ_PATHNAME) > 0)
	    iferr (in = open (Memc[fname], READ_ONLY, TEXT_FILE))
		call erract (EA_WARN)
	    else {
		rawmode = fstati (fd, F_RAW)
		call fseti (fd, F_RAW, YES)
		call fcopyo (in, fd)
		call fseti (fd, F_RAW, rawmode)
		call close (in)
	    }

	# Output initialization string.
	junk = ttyctrl (fd, tty, "is", 1)

	call sfree (sp)
end
