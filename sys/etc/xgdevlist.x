# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<xalloc.h>
include	<ctype.h>

define	DEVTABLE	"dev$devices"
define	SZ_LONGLINE	1024


# XGDEVLIST -- Fetch the host device list for the named logical device
# from the device table.  DV_DEVNOTFOUND is returned there is no entry in
# the device table for the device.  An error action is taken if there is any
# problem reading the device table file.
#
# Each device appears as a single line of text in the devices file, ignoring
# backslash line continuation and comments.  The format of a device entry is
# as follows:
#
#	mtX[.density] [%maxbufsize] osname [other]
#
# where mta, mtb, etc are device names, .density (optional) is something like
# .1600, %maxbufsize is the maximum transfer (record) size for the device,
# osname is the host system device name, and [other] is other text to be passed
# on to the device driver at open time.  If maxbufsize is not given in the
# device entry, -1 is returned; 0 is a valid value indicating that there is no
# max transfer size.

int procedure xgdevlist (device, maxbufsize, outstr, maxch, onedev)

char	device[ARB]		# IRAF name of device entry
int	maxbufsize		# receives max device transfer size, bytes
char	outstr[maxch]		# receives device list
int	maxch			# max chars out
int	onedev			# return only first name in list?

bool	notfound
int	fd, lineno, nchars
pointer	sp, ip, lbuf, node, devlist, fname
int	open(), getlongline(), strncmp(), strlen(), ctoi(), ki_extnode()
errchk	open, getlongline

begin
	call smark (sp)
	call salloc (lbuf, SZ_LONGLINE, TY_CHAR)
	call salloc (node, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Get node on which resource resides.
	ip = ki_extnode (device, Memc[node], SZ_FNAME, nchars) + 1

	# Fetch the device table entry for the named device into the line
	# buffer.

	call strcpy (Memc[node], Memc[fname], SZ_FNAME)
	call strcat (DEVTABLE,   Memc[fname], SZ_FNAME)

	fd = open (Memc[fname], READ_ONLY, TEXT_FILE)
	nchars = strlen (device[ip])
	lineno = 1
	notfound = true

	while (getlongline (fd, Memc[lbuf], SZ_LONGLINE, lineno) != EOF)
	    if (IS_WHITE (Memc[lbuf+nchars]))
		if (strncmp (Memc[lbuf], device[ip], nchars) == 0) {
		    notfound = false
		    break
		}

	call close (fd)
	if (notfound) {
	    call sfree (sp)
	    return (DV_DEVNOTFOUND)
	}

	# Extract the packed list of aliases (host device name or names
	# corresponding to the named logical device).  Strip the newline
	# and any trailing whitespace.  If there is only one host device
	# name per logical device, a simple name with no leading or trailing
	# whitespace is passed to the host.

	# Skip over the "mtX.NNNN   ".
	for (ip=lbuf;  Memc[ip] != EOS;  ip=ip+1)
	    if (IS_WHITE(Memc[ip]) || Memc[ip] == '%')
		break
	for (;  IS_WHITE (Memc[ip]);  ip=ip+1)
	    ;

	# Get optional max transfer size, %NNNN.
	if (Memc[ip] == '%') {
	    ip = ip + 1
	    if (ctoi (Memc, ip, maxbufsize) <= 0)
		maxbufsize = -1
	} else
	    maxbufsize = -1

	# Set devlist to EOS delimited device driver id string.
	for (;  IS_WHITE (Memc[ip]);  ip=ip+1)
	    ;
	for (devlist=ip;  Memc[ip] != EOS;  ip=ip+1)
	    ;
	for (ip=ip-1;  IS_WHITE (Memc[ip]) || Memc[ip] == '\n';  ip=ip-1)
	    ;
	Memc[ip+1] = EOS

	# Truncate list to first device name if only one name is required.
	if (onedev == YES) {
	    for (ip=devlist;  Memc[ip] != EOS && !IS_WHITE (Memc[ip]);  ip=ip+1)
		;
	    Memc[ip] = EOS
	}

	# Return the host device name list, preserving the node name.
	call strcpy (Memc[node], outstr, maxch)
	call strcat (Memc[devlist], outstr, maxch)

	call sfree (sp)
	return (OK)
end
