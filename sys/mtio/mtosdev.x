# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <syserr.h>
include <xalloc.h>

define	SZ_NAME		15

# MT_OSDEV -- Convert a logical drive name and density into a dev$devices table
# entry name, look the entry up in the table, and return the OS device name.
# The last name translation is cached for faster access when reading multiple
# files on a tape.

procedure mt_osdev (drive, density, osdev, maxch, maxbufsize)

char	drive[ARB]		# logical drive name (e.g., "a", "b")
int	density			# desired density, else zero
char	osdev[maxch]		# receives OS device name
int	maxch
int	maxbufsize		# receives max device transfer (record) size

char	devname[SZ_NAME]
int	onedev, c_maxbufsize, nchars, ip
char	c_mtname[SZ_NAME], c_osname[SZ_NAME], node[SZ_NAME]
bool	first_time, streq()
int	xgdevlist(), strlen(), ki_extnode()
errchk	xgdevlist, syserrs
data	first_time /true/

begin
	if (first_time) {
	    c_mtname[1] = EOS
	    first_time = false
	}

	# Get node prefix, if any, and index of drive name.
	ip = ki_extnode (drive, node, SZ_FNAME, nchars) + 1

	# Format drive name as in DEV$DEVICES table, e.g., "mta.1600".
	if (density > 0) {
	    call sprintf (devname, SZ_NAME, "%smt%s.%d")
		call pargstr (node)
		call pargstr (drive[ip])
		call pargi   (density)
	} else {
	    call sprintf (devname, SZ_NAME, "%smt%s")
		call pargstr (node)
		call pargstr (drive[ip])
	}

	# Check the cache first.
	if (streq (devname, c_mtname)) {
	    call strcpy (c_osname, osdev, maxch)
	    maxbufsize = c_maxbufsize
	    return
	}

	# Translate name using device table.
	onedev = YES
	if (xgdevlist (devname,maxbufsize,osdev,maxch,onedev) == DV_DEVNOTFOUND)
	    call syserrs (SYS_MTDEVNF, devname)

	# Save in cache if it fits.
	if (strlen (osdev) <= SZ_NAME) {
	    call strcpy (devname, c_mtname, SZ_NAME)
	    call strcpy (osdev,   c_osname, SZ_NAME)
	    c_maxbufsize = maxbufsize
	}
end
