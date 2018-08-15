# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<syserr.h>
include	<ctype.h>
include	<chars.h>
include	"tty.h"

define	DEF_BAUDRATE	9600
define	DEF_TTYNLINES	24
define	DEF_TTYNCOLS	80

# TTYODES -- Open a TTY terminal descriptor.  If ttyname is "terminal" or
# "printer", get the name of the default terminal or printer from the
# environment.  If the "name" of the terminal is a filename, the first termcap
# entry in the name file is read.  Otherwise, the termcap file is searched for
# an entry corresponding to the named device.
# 
# The descriptor is then allocated, and the termcap entry read in.  Termcap
# permits an entry to be defined in terms of another entry with the "tc"
# field; we must expand such references by rescanning the file once for each
# such reference.  Finally, the termcap entry is indexed for efficient access.
# 
# The form of a termcap entry is one logical line (usually extending over
# several physical lines using newline escapes), consisting of several alternate
# names for the device, followed by a list of ':' delimited capabilities:
# 
# 	name1 '|' name2 [ '|' namen... ] ':' cap ':' [ cap ':' ... ]
# 
# If the final cap in an entry is of the form ":tc=name:", the capability
# is replaced by the capability list of the named entry.

pointer procedure ttyodes (ttyname)

char	ttyname[ARB]

bool	istty
int	nchars
pointer	sp, ttysource, device, devname, fname, tty

pointer	ttyopen()
extern	ttyload()
bool	streq(), ttygetb()
int	envgets(), envgeti(), btoi()
int	fnldir(), ttygeti(), ttygets()
errchk	syserrs, tty_index_caps, ttygeti, ttyopen, ttygets
errchk	envgets, envgeti, envindir

string	terminal "terminal"	# terminal named in environment
string	printer  "printer"	# printer named in environment
string	termcap  "termcap"	# name of termcap file stored in env. too

begin
	call smark (sp)
	call salloc (ttysource, SZ_FNAME, TY_CHAR)
	call salloc (devname, SZ_FNAME, TY_CHAR)
	call salloc (device, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Resolve any indirection in the device name.
	call envindir (ttyname, Memc[devname], SZ_FNAME)

	# Get device name or termcap file name.
	if (streq (Memc[devname], terminal)) {
	    if (envgets (terminal, Memc[ttysource], SZ_FNAME) <= 0)
		call syserrs (SYS_ENVNF, terminal)
	} else if (streq (Memc[devname], printer)) {
	    if (envgets (printer, Memc[ttysource], SZ_FNAME) <= 0)
		call syserrs (SYS_ENVNF, printer)
	} else
	    call strcpy (Memc[devname], Memc[ttysource], SZ_FNAME)

	# If ttysource is a filename, we assume that it is the name of
	# a termcap format file, the first entry of which (matched by the
	# null device name) is the entry for the device.  Otherwise,
	# ttysource is the name of the desired termcap entry in the regular
	# termcap file.

	if (fnldir (Memc[ttysource], Memc[fname], SZ_FNAME) > 0) {
	    call strcpy (Memc[ttysource], Memc[fname], SZ_FNAME)
	    Memc[device] = EOS
	} else {
	    if (envgets (termcap, Memc[fname], SZ_FNAME) <= 0)
		call syserrs (SYS_ENVNF, termcap)
	    call strcpy (Memc[ttysource], Memc[device], SZ_FNAME)
	}

	# Truncate the device name if device fields are appended.
	call ttydevname (Memc[device], Memc[device], SZ_FNAME)

	# Allocate and initialize the tty descriptor structure and fetch
	# termcap entry from termcap file.  The TTYLOAD procedure, passed
	# as an argument to TTYOPEN, is searched for cached termcap entries
	# before accessing the actual file.

	tty = ttyopen (Memc[fname], Memc[device], ttyload)

	# Prepare index of fields in the descriptor, so that we can more
	# efficiently search for fields later.

	call tty_index_caps (tty, T_CAPCODE(tty), T_CAPINDEX(tty),
	    T_NCAPS(tty))

	# Determine whether or not the terminal can backspace with BS.
	if (ttygetb (tty, "bs"))
	    T_BSOK(tty) = YES
	else
	    T_BSOK(tty) = NO

	# Determine whether or not the stdgraph device can expand tabs.
	# If it can but it requires some long string, don't bother.

	T_HTOK(tty) = NO
	T_TABCHAR(tty) = 0
	if (ttygetb (tty, "pt")) {
	    nchars = ttygets (tty, "ta", Memc[fname], SZ_FNAME)
	    if (nchars <= 0) {
		T_HTOK(tty) = YES
		T_TABCHAR(tty) = '\t'
	    } else if (nchars == 1) {
		T_HTOK(tty) = YES
		T_TABCHAR(tty) = Memc[fname]
	    }
	}

	# Does the terminal autoadvance at the right margin?
	T_AM(tty) = btoi (ttygetb (tty, "am"))

	# Determine the optimimum mode for handling standout mode control,
	# and save in the descriptor.

	if (ttygetb (tty, "so")) {
	    T_SOTYPE(tty) = SOSE		# use so, se
	} else if (ttygetb (tty, "os")) {
	    if (T_BSOK(tty) == YES || ttygetb (tty, "bc"))
		T_SOTYPE(tty) = BSOS		# backspace, ostrike
	    else
		T_SOTYPE(tty) = CROS		# ostrike whole line
	} else
	    T_SOTYPE(tty) = TOUP		# to upper case

	# Get pad char and baud rate (used by ttyputs to generate delays)
	# and put in descriptor for efficient access.  Also get tty screen
	# dimensions since they are fundamental and will probably prove
	# useful later.

	T_PADCHAR(tty) = ttygeti (tty, "pc")	# returns 0 if field not found

	# Allow environment variables to override physical screen dimensions
	# if device is the standard terminal.

	istty = (streq (Memc[devname], terminal))

	# Get nlines and ncols
	if (istty)
	    iferr (T_NLINES(tty) = envgeti ("ttynlines"))
		T_NLINES(tty) = 0
	if (istty)
	    iferr (T_NCOLS(tty) = envgeti ("ttyncols"))
		T_NCOLS(tty) = 0

	if (T_NLINES(tty) <= 0 || T_NCOLS(tty) <= 0) {
	    call zttysz (1,T_NCOLS(tty),T_NLINES(tty))
	}

	if (T_NLINES(tty) <= 0)
	    iferr (T_NLINES(tty) = ttygeti (tty, "li"))
		T_NLINES(tty) = 0
	if (T_NCOLS(tty) <= 0)
	    iferr (T_NCOLS(tty) = ttygeti (tty, "co"))
		T_NCOLS(tty) = 0

	if (T_NLINES(tty) <= 0)
	    T_NLINES(tty) = DEF_TTYNLINES
	if (T_NCOLS(tty) <= 0)
	    T_NCOLS(tty) = DEF_TTYNCOLS

	# Baud rate may not be right if device is a printer attached to the
	# host, and user is working remotely.  Nonetheless it is safer to pick
	# up the value from the environment than to assume a default.

	iferr (T_BAUD(tty) = envgeti ("ttybaud"))
	    T_BAUD(tty) = DEF_BAUDRATE

	call sfree (sp)
	return (tty)
end
