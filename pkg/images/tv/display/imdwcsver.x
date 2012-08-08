# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "iis.h"
include "zdisplay.h"

# IMD_WCSVER -- Query the server for the WCS version supported.  A zero
# will be returned for the "old" wcs format used, otherwise the server  
# will return a version identifier.

int procedure imd_wcsver ()

pointer	ds
int     chan, status, frame, ip
char	wcstext[SZ_OLD_WCSTEXT]

int     strncmp(), ctoi(), iisflu()
pointer	imd_mapframe1()
bool	envgetb()

include	"iis.com"

begin
	iis_valid = NO				# initialize

	# Check the environment for a flag to disable the new WCS info.
        if (envgetb ("disable_wcs_maps")) {
            iis_version = 0
	    return (iis_version)
	}

	# Open a temporary connection to the server if needed.
	ds = NULL
	if (iisnopen == 0)
	    ds = imd_mapframe1 (1, READ_ONLY, NO, NO)

        # Send a WCS query with the X and Y register set.  This tells a
	# knowledgeable server to reply with a WCS version string, 
	# otherwise it is a no-op and we get the normal WCS response
	# indicating the old format.

	frame = 1
        chan = iisflu (FRTOCHAN(frame))
        call aclrc (wcstext, SZ_OLD_WCSTEXT)
        call iishdr (IREAD+PACKED, SZ_OLD_WCSTEXT, WCS, 1, 1, chan, 0)
        call iisio (wcstext, SZ_OLD_WCSTEXT, status)
        if (status > 0)
            call strupk (wcstext, wcstext, SZ_OLD_WCSTEXT)
	else {
            iis_version = 0
	    call imunmap (ds)
	    return (iis_version)
	}

        # Decode the version from the WCS text.
        if (strncmp (wcstext, "version=", 8) == 0) {
            ip = 9
            status = ctoi (wcstext, ip, iis_version)
        } else
            iis_version = 0


	if (ds != NULL)
	    call imunmap (ds)
        return (iis_version)
end
