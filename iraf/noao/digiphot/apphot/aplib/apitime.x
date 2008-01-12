include <imhdr.h>
include "../lib/apphot.h"

# AP_ITIME -  Procedure to set the image exposure time .

procedure ap_itime (im, ap)

pointer	im		# pointer to IRAF image
pointer	ap		# pointer to apphot structure

pointer	sp, key
real	itime
real	imgetr(), apstatr()

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call apstats (ap, EXPOSURE, Memc[key], SZ_FNAME)
	if (Memc[key] == EOS)
	    itime = apstatr (ap, ITIME)
	else {
	    iferr { 
	        itime = imgetr (im, Memc[key])
	    } then {
		itime = apstatr (ap, ITIME)
		call eprintf ("Warning: Image %s  Keyword: %s not found\n")
		    call pargstr (IM_HDRFILE(im))
		    call pargstr (Memc[key])
	    }
	}
	if (IS_INDEFR(itime) || itime <= 0.0)
	    call apsetr (ap, ITIME, 1.0)
	else
	    call apsetr (ap, ITIME, itime)
	call sfree (sp)
end
