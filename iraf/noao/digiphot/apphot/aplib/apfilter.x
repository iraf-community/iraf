include <imhdr.h>
include "../lib/apphot.h"

# AP_FILTER --  Procedure to set the image airmass.

procedure ap_filter (im, ap)

pointer	im		# pointer to IRAF image
pointer	ap		# pointer to apphot structure

size_t	sz_val
pointer	sp, key, filt

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (key, sz_val, TY_CHAR)
	call salloc (filt, sz_val, TY_CHAR)

	call apstats (ap, FILTER, Memc[key], SZ_FNAME)
	Memc[filt] = EOS
	if (Memc[key] == EOS)
	    call apstats (ap, FILTERID, Memc[filt], SZ_FNAME)
	else {
	    iferr { 
	        call imgstr (im, Memc[key], Memc[filt], SZ_FNAME)
	    } then {
	        call apstats (ap, FILTERID, Memc[filt], SZ_FNAME)
		call eprintf ("Warning: Image %s  Keyword: %s not found\n")
		    call pargstr (IM_HDRFILE(im))
		    call pargstr (Memc[key])
	    }
	}

	if (Memc[filt] == EOS) {
	    call apsets (ap, FILTERID, "INDEF")
	} else {
	    call ap_rmwhite (Memc[filt], Memc[filt], SZ_FNAME)
	    call apsets (ap, FILTERID, Memc[filt])
	}

	call sfree (sp)
end
