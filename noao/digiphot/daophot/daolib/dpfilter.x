include <imhdr.h>
include "../lib/daophotdef.h"

# DP_FILTER --  Procedure to set the image airmass.

procedure dp_filter (im, dao)

pointer	im		# pointer to IRAF image
pointer	dao		# pointer to the daophot structure

pointer	sp, key, filt

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (filt, SZ_FNAME, TY_CHAR)

	call dp_stats (dao, FILTER, Memc[key], SZ_FNAME)
	Memc[filt] = EOS
	if (Memc[key] == EOS)
	    call dp_stats (dao, IFILTER, Memc[filt], SZ_FNAME)
	else {
	    iferr { 
	        call imgstr (im, Memc[key], Memc[filt], SZ_FNAME)
	    } then {
	        call dp_stats (dao, IFILTER, Memc[filt], SZ_FNAME)
		call eprintf ("Warning: Image %s  Keyword: %s not found\n")
		    call pargstr (IM_HDRFILE(im))
		    call pargstr (Memc[key])
	    }
	}

	if (Memc[filt] == EOS) {
	    call dp_sets (dao, IFILTER, "INDEF")
	} else {
	    call dp_rmwhite (Memc[filt], Memc[filt], SZ_FNAME)
	    call dp_sets (dao, IFILTER, Memc[filt])
	}

	call sfree (sp)
end
