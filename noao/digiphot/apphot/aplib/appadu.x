include <imhdr.h>
include "../lib/noise.h"

# AP_PADU -- Procedure to set the gain parameter for the noise model
# computation.

procedure ap_padu (im, ap)

pointer	im		# pointer to IRAF image
pointer	ap		# pointer to apphot structure

pointer	sp, key
real	padu
real	imgetr(), apstatr()

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call apstats (ap, GAIN, Memc[key], SZ_FNAME)
	if (Memc[key] == EOS)
	    padu = apstatr (ap, EPADU)
	else {
	    iferr {
	        padu = imgetr (im, Memc[key])
	    } then {
		padu = apstatr (ap, EPADU)
		call eprintf ("Warning: Image %s  Keyword %s not found.\n")
		    call pargstr (IM_HDRFILE(im))
		    call pargstr (Memc[key])
	    }
	}
	if (IS_INDEFR(padu) || padu <= 0.0)
	    call apsetr (ap, EPADU, 1.0)
	else
	    call apsetr (ap, EPADU, padu)
	call sfree (sp)
end
