include <imhdr.h>
include "../lib/daophot.h"

# DP_PADU -- Procedure to set the gain parameter for the noise model
# computation.

procedure dp_padu (im, dao)

pointer	im		# pointer to IRAF image
pointer	dao		# pointer to the daophot structure

pointer	sp, key
real	padu
real	imgetr(), dp_statr()

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call dp_stats (dao, CCDGAIN, Memc[key], SZ_FNAME)
	if (Memc[key] == EOS)
	    padu = dp_statr (dao, PHOT_ADC)
	else {
	    iferr {
	        padu = imgetr (im, Memc[key])
	    } then {
		padu = dp_statr (dao, PHOT_ADC)
		call eprintf ("Warning: Image %s  Keyword %s not found.\n")
		    call pargstr (IM_HDRFILE(im))
		    call pargstr (Memc[key])
	    }
	}
	if (IS_INDEFR(padu) || padu <= 0.0)
	    call dp_setr (dao, PHOT_ADC, 1.0)
	else
	    call dp_setr (dao, PHOT_ADC, padu)
	call sfree (sp)
end
