include <imhdr.h>
include "../lib/apphot.h"

# AP_AIRMASS -  Procedure to determine the image airmass.

procedure ap_airmass (im, ap)

pointer	im		# pointer to IRAF image
pointer	ap		# pointer to apphot structure

pointer	sp, key
real	xair
real	imgetr(), apstatr()

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call apstats (ap, AIRMASS, Memc[key], SZ_FNAME)
	if (Memc[key] == EOS)
	    xair = apstatr (ap, XAIRMASS)
	else {
	    iferr { 
	        xair = imgetr (im, Memc[key])
	    } then {
		xair = apstatr (ap, XAIRMASS)
		call eprintf ("Warning: Image %s  Keyword: %s not found\n")
		    call pargstr (IM_HDRFILE(im))
		    call pargstr (Memc[key])
	    }
	}
	if (IS_INDEFR(xair) || xair <= 0.0)
	    call apsetr (ap, XAIRMASS, INDEFR)
	else
	    call apsetr (ap, XAIRMASS, xair)
	call sfree (sp)
end
