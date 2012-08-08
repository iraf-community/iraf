include <imhdr.h>
include "../lib/noise.h"

# AP_RDNOISE -  Procedure to set the image read noise parameter.

procedure ap_rdnoise (im, ap)

pointer	im		# pointer to IRAF image
pointer	ap		# pointer to apphot structure

pointer	sp, key
real	rdnoise
real	imgetr(), apstatr()

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call apstats (ap, CCDREAD, Memc[key], SZ_FNAME)
	if (Memc[key] == EOS)
	    rdnoise = apstatr (ap, READNOISE)
	else {
	    iferr {
	        rdnoise = imgetr (im, Memc[key])
	    } then {
		rdnoise = apstatr (ap, READNOISE)
		call eprintf ("Warning: Image %s  Keyword %s not found.\n")
		    call pargstr (IM_HDRFILE(im))
		    call pargstr (Memc[key])
	    }
	}
	if (IS_INDEFR(rdnoise) || rdnoise <= 0.0)
	    call apsetr (ap, READNOISE, 0.0)
	else
	    call apsetr (ap, READNOISE, rdnoise)
	call sfree (sp)
end
