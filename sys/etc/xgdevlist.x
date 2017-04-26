# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<xalloc.h>

# XGDEVLIST -- Fetch the allocation string for the named logical device from
# the device table (tapecap file).  DV_DEVNOTFOUND is returned there is no
# entry in the device table for the device.  An error action is taken if there
# is any problem reading the device entry.
#
# This routine is a bit of an anachronism in the days of tapecap, but is
# left pretty much as it was originally to minimize code modifications.
# In principle the allocation code can be used to allocate any device, not
# just tape drives.  This is still the case, given an entry for the device
# in the tapecap file.

int procedure xgdevlist (device, outstr, maxch, onedev)

char	device[ARB]		#I logical device name
char	outstr[maxch]		#O receives device list
int	maxch			#I max chars out
int	onedev			#I return i/o device instead?

pointer	gty
int	nchars
pointer	mtcap()
int	gtygets(), strlen()
errchk	syserrs

begin
	# Fetch the tapecap entry for the named device.  Do not close the GTY
	# descriptor.  mtcap always keeps the last one in an internal cache.

	iferr (gty = mtcap (device))
	    return (DV_DEVNOTFOUND)

	if (onedev == YES)
	    nchars = gtygets (gty, "dv", outstr, maxch)
	else
	    nchars = gtygets (gty, "al", outstr, maxch)

	call ki_xnode (device, outstr, maxch)
	nchars = strlen (outstr)

	if (nchars <= 0)
	    call syserrs (SYS_MTTAPECAP, device)

	return (OK)
end
