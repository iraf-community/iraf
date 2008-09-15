# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<syserr.h>
include	<ctype.h>
include	<chars.h>
include	"tty.h"

define	DEF_BAUDRATE	9600

# TTYGDES -- Open a TTY graphics stdgraph descriptor.  If ttyname is
# "stdgraph" or "stdplot", get the name of the physical graphics device
# from the environment.  The name of the graphcap file is fetched from the
# environment and the graphcap file is searched for an entry corresponding
# to the named device.
# 
# The descriptor is then allocated, and the graphcap entry read in.  Graphcap
# permits an entry to be defined in terms of another entry with the "tc"
# field; we must expand such references by rescanning the file once for each
# such reference.  Finally, the graphcap entry is indexed for efficient access.
# The form of a graphcap entry is identical to that for a termcap entry.

pointer procedure ttygdes (ttyname)

char	ttyname[ARB]

int	nchars
pointer	sp, ttysource, device, devname, fname, tty
pointer	ttyopen()
extern	g_ttyload()
bool	streq(), ttygetb()
int	fnldir(), ttygeti(), ttygets()
int	envgets(), envgeti()
errchk	envgets, envgeti, ttyopen, ttygets, syserrs
errchk	tty_index_caps, ttygeti, envindir

string	stdgraph "stdgraph"
string	stdimage "stdimage"
string	stdplot  "stdplot"
string	graphcap "graphcap"

begin
	call smark (sp)
	call salloc (ttysource, SZ_FNAME, TY_CHAR)
	call salloc (devname, SZ_FNAME, TY_CHAR)
	call salloc (device, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Resolve any indirection in the device name.
	call envindir (ttyname, Memc[devname], SZ_FNAME)

	# Get device name or graphcap file name.
	if (streq (Memc[devname], stdgraph)) {
	    if (envgets (stdgraph, Memc[ttysource], SZ_FNAME) <= 0)
		call syserrs (SYS_ENVNF, stdgraph)
	} else if (streq (Memc[devname], stdimage)) {
	    if (envgets (stdimage, Memc[ttysource], SZ_FNAME) <= 0)
		call syserrs (SYS_ENVNF, stdimage)
	} else if (streq (Memc[devname], stdplot)) {
	    if (envgets (stdplot, Memc[ttysource], SZ_FNAME) <= 0)
		call syserrs (SYS_ENVNF, stdplot)
	} else
	    call strcpy (Memc[devname], Memc[ttysource], SZ_FNAME)

	# If ttysource is a filename, we assume that it is the name of
	# a graphcap format file, the first entry of which (matched by the
	# null device name) is the entry for the device.  Otherwise,
	# ttysource is the name of the desired graphcap entry in the regular
	# graphcap file.

	if (fnldir (Memc[ttysource], Memc[fname], SZ_FNAME) > 0) {
	    call strcpy (Memc[ttysource], Memc[fname], SZ_FNAME)
	    Memc[device] = EOS
	} else {
	    if (envgets (graphcap, Memc[fname], SZ_FNAME) <= 0)
		call syserrs (SYS_ENVNF, graphcap)
	    call strcpy (Memc[ttysource], Memc[device], SZ_FNAME)
	}

	# Truncate the device name if device fields are appended.
	call ttydevname (Memc[device], Memc[device], SZ_FNAME)

	# Allocate and initialize the tty descriptor structure.  Fetch graphcap
	# entry from graphcap file into descriptor.  The G_TTYLOAD procedure,
	# passed as an argument to TTYOPEN, is searched for cached termcap
	# entries before accessing the actual file.

	tty = ttyopen (Memc[fname], Memc[device], g_ttyload)

	# Prepare index of fields in the descriptor, so that we can more
	# efficiently search for fields later.

	call tty_index_caps (tty, T_CAPCODE(tty), T_CAPINDEX(tty), T_NCAPS(tty))

	# Determine whether or not the stdgraph device can backspace with BS.
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
	T_NLINES(tty)  = ttygeti (tty, "li")
	T_NCOLS(tty)   = ttygeti (tty, "co")

	# Baud rate may not be right if device is attached to the host, and
	# and user is working remotely.  Nonetheless it is safer to pick up
	# the value from the environment than to assume a default.

	iferr (T_BAUD(tty) = envgeti ("ttybaud"))
	    T_BAUD(tty) = DEF_BAUDRATE

	call sfree (sp)
	return (tty)
end
