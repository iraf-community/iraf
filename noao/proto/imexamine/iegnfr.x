# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imhdr.h>
include "imexam.h"

# IE_GETNFRAMES -- Determine the number of image display frames.  If the
# display can be accessed at all we assume there is always at least one
# frame; beyond that presence of a valid WCS is used to test whether we
# are interested in looking at a frame.

int procedure ie_getnframes (ie)

pointer	ie			#I imexamine descriptor

pointer	sp, imname, ds, iw
int	server, nframes, status, i

int	clgeti(), strncmp()
pointer	imd_mapframe(), iw_open()
errchk	imd_mapframe, clgeti

begin
	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)

	nframes = clgeti ("nframes")
	if (nframes == 0) {
	    # Try to automatically determine the number of frames.
	    ds = IE_DS(ie)
	    if (ds == NULL)
		ds = imd_mapframe (1, READ_WRITE, NO)
	    
	    # If we are talking to a simple image display we assume the device
	    # has 4 frames (until more general display interfaces come along).
	    # Servers are more complicated because the number of frames is
	    # dynamically configurable, even while imexamine is running.
	    # We use the WCS query to try to count the current number of
	    # allocated frames in the case of a server device.

	    server = IM_LEN(ds,4)
	    if (server == YES) {
		nframes = 1
		do i = 1, MAX_FRAMES {
		    iferr (iw = iw_open (ds, i, Memc[imname], SZ_FNAME, status))
			next
		    call iw_close (iw)
		    if (strncmp (Memc[imname], "[NOSUCHFRAME]", 3) != 0)
			nframes = max (nframes, i)
		}
	    } else
		nframes = 4

	    if (IE_DS(ie) == NULL)
		call imunmap (ds)
	}

	IE_NFRAMES(ie) = max (nframes, IE_DFRAME(ie))
	call sfree (sp)

	return (nframes)
end
