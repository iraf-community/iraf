# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include <knet.h>
include "zdisplay.h"
include "iis.h"

# ----------------------------------------------------------------------
# MODIFIED VERSION OF IISOPN.X FOR IMTOOL -- DO NOT DELETE.
# Referenced by the Sun/IRAF special file list: see hlib$mkpkg.sf.
# ----------------------------------------------------------------------

# IISOPN -- Open IIS display.

procedure iisopn (devinfo, mode, chan)

char	devinfo[ARB]		# device info for zopen (packed)
int	mode			# access mode
int	chan[ARB]		# receives IIS descriptor

int	delim
char	resource[SZ_FNAME]
char	node[SZ_FNAME]
bool	first_time
data	first_time /true/
int	ki_gnode(), strncmp()
include	"iis.com"
include	"imd.com"
define	quit_ 91

begin
	if (first_time) {
	    iisnopen = 0
	    iis_version = 0
	    first_time = false
	}

	# We permit multiple opens but only open the physical device once.
	if (iisnopen == 0) {
	    call zopngd (devinfo, mode, iischan)

	    # Initialize imd_gcur.
	    call strcpy (devinfo, imd_devinfo, SZ_LINE)
	    imd_mode = mode
	    imd_magic = -1
	}

	if (iischan != ERR) {
	    iisnopen = iisnopen + 1
	    chan[1] = FRTOCHAN(iisframe)

	    # The following code is DEVICE DEPENDENT (horrible kludge, but
	    # it simplifies things and this is throw away code).

	    # Byte pack i/o if the device is on a remote node since the i/o
	    # bandwidth is the limiting factor; do not bytepack if on local
	    # node since cpu time is the limiting factor.

	    call strupk (devinfo, resource, SZ_FNAME)
	    packit = (ki_gnode (resource, node, delim) != 0)
	    if (!packit)
		packit = (strncmp (resource[delim+1], "imt", 3) == 0)

	    # Enable byte swapping if the device is byte swapped but the
	    # local host is not (assumes that if there is an IIS it is on
	    # a byte swapped VAX - this should be done in graphcap instead).

	    swap_bytes = (strncmp (resource[delim+1], "iis", 3) == 0 &&
		BYTE_SWAP2 == NO)

	    # Initialize zoom.
	    call iiszm (1, 0, 0)

	} else
	    chan[1] = ERR
end
