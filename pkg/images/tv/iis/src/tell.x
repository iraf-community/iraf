# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.


include	"../lib/ids.h"

# TELL -- Tell user about display state

procedure tell()

short	f[IDS_MAXIMPL+2]		# Ultimately, want an array terminated
					# with IDS_EOD as usual

include "cv.com"

begin
	# We don't know much, do we?

	call cvwhich(f)
	if ( f[1] > 0) {
	    call eprintf ("Frame %d, at least, is on.\n")
		call pargs (f[1])
	} else 
	    call eprintf ("No frames are on.\n")
end
