# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <syserr.h>
include "mtio.h"

# MT_GTYOPEN -- Given a logical device name, open the tapecap file and extract
# the entry for the named device into an open GTY descriptor.  The descriptor
# pointer is returned as the function value.  The last entry accessed is
# cached indefinitely so that repeated references cause the tapecap file to
# be scanned only once.
#
# The tapecap file may be defined in the user environment, otherwise it
# defaults to the compiled in default TAPECAP (dev$tapecap).  The format
# of the "tapecap" environment variable is "filename[:devcap]", e.g.
#
#	home$tapecap:so
#
#
# would cause the file home$tapecap to be used as the tapecap file, adding the
# device capabilities :so to each tapecap device access (any devcap fields
# given on the command line will override these).  Either the filename or
# devcap field may be omitted.  For example,  reset tapecap = ":so"  causes
# the default tapecap file dev$tapecap to be used, but enables status output
# logging in each magtape access.

pointer procedure mt_gtyopen (device, ufields)

char	device[ARB]			#I local device name (incl node)
char	ufields[ARB]			#I optional user tapecap fields

int	len_ufields, junk
char	c_device[SZ_DEVICE]
char	c_ufields[SZ_DEVCAP]
bool	first_time, capseen, remote
pointer	c_gty, gty, sp, tapecap, devcap, ip, op
pointer	fname, dname, nname, tname

bool	streq()
pointer	gtyopen()
int	envfind(), stridxs(), strlen(), gstrcpy(), ki_gnode()
errchk	gtyopen, syserrs
data	first_time /true/

begin
	# First time initialization.
	if (first_time) {
	    c_gty = NULL
	    first_time = false
	}

	# Check the cache.
	if (c_gty != NULL && streq(device,c_device) && streq(ufields,c_ufields))
	    return (c_gty)

	# Cache miss.  Free the old descriptor.
	if (c_gty != NULL)
	    call gtyclose (c_gty)

	call smark (sp)
	call salloc (tapecap, SZ_DEVCAP, TY_CHAR)
	call salloc (devcap, SZ_DEVCAP, TY_CHAR)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (tname, SZ_PATHNAME, TY_CHAR)
	call salloc (dname, SZ_FNAME, TY_CHAR)
	call salloc (nname, SZ_FNAME, TY_CHAR)

	# Get tapecap definition.
	if (envfind ("tapecap", Memc[tapecap], SZ_DEVCAP) <= 0)
	    call strcpy (TAPECAP, Memc[tapecap], SZ_DEVCAP)

	# Parse into filename and devcap fields.
	op = fname
	capseen = false
	len_ufields = gstrcpy (ufields, Memc[devcap], SZ_DEVCAP)
	for (ip=tapecap;  Memc[ip] != EOS;  ip=ip+1) {
	    if (Memc[ip] == ':' && !capseen) {
		Memc[op] = EOS
		op = devcap + len_ufields
		capseen = true
	    }
	    Memc[op] = Memc[ip]
	    op = op + 1
	}
	Memc[op] = EOS

	# Supply default filename if none given.
	if (Memc[fname] == EOS)
	    call strcpy (TAPECAP, Memc[fname], SZ_PATHNAME)

	# If no node is specified for the tapecap file, access the tapecap
	# file on the same node as the magtape device file.

	if (stridxs ("!", Memc[fname]) <= 0)
	    call ki_xnode (device, Memc[fname], SZ_PATHNAME)

	# Get the node name the device is on.
	remote = (ki_gnode (Memc[fname], Memc[nname], junk) != 0)

	# Get the tapecap device name minus any node prefix.
	call strcpy (device, Memc[dname], SZ_FNAME)
	call ki_xnode ("", Memc[dname], SZ_FNAME)

	# Open the tapecap entry.  Try "tapecap.<node>" first then "tapecap".
	# <node> is the hostname of the host the tape device is on, i.e. the
	# network server on which the drive is located.  The "tapecap.<node>"
	# feature allows a shared common IRAF installation to support distinct
	# tapecap files for different servers, falling back on the standard
	# tapecap file if no node-specific file is found.

	call strcpy (Memc[fname], Memc[tname], SZ_PATHNAME)
	call strcat (".", Memc[tname], SZ_PATHNAME)
	call strcat (Memc[nname], Memc[tname], SZ_PATHNAME)

	iferr (gty = gtyopen (Memc[tname], Memc[dname], Memc[devcap]))
	    iferr (gty = gtyopen (Memc[fname], Memc[dname], Memc[devcap]))
		call syserrs (SYS_MTTAPECAP, device)
	if (gty == NULL)
	    call syserrs (SYS_MTTAPECAP, device)

	# Update the cache.
	if (strlen(ufields) <= SZ_DEVCAP) {
	    call strcpy (device, c_device, SZ_DEVICE)
	    call strcpy (Memc[devcap], c_ufields, SZ_DEVCAP)
	    c_gty = gty
	}

	call sfree (sp)
	return (gty)
end
