include <imhdr.h>
include "../lib/daophotdef.h"

# DP_AIRMASS --  Set the image airmass.

procedure dp_airmass (im, dao)

pointer	im		# pointer to IRAF image
pointer	dao		# pointer to the daophot structure

pointer	sp, key
real	xair
real	imgetr(), dp_statr()

begin
	# Get the airmass keyword.
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call dp_stats (dao, AIRMASS, Memc[key], SZ_FNAME)

	# Get the value.
	if (Memc[key] == EOS)
	    xair = dp_statr (dao, XAIRMASS)
	else {
	    iferr { 
	        xair = imgetr (im, Memc[key])
	    } then {
		xair = dp_statr (dao, XAIRMASS)
		call eprintf ("Warning: Image %s  Keyword: %s not found\n")
		    call pargstr (IM_HDRFILE(im))
		    call pargstr (Memc[key])
	    }
	}

	# Store the value.
	if (IS_INDEFR(xair) || xair <= 0.0)
	    call dp_setr (dao, XAIRMASS, INDEFR)
	else
	    call dp_setr (dao, XAIRMASS, xair)

	call sfree (sp)
end
