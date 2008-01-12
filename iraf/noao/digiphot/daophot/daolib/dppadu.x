include <imhdr.h>
include "../lib/daophotdef.h"

# DP_PADU -- Read the gain value from the image header.

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
	    padu = dp_statr (dao, PHOTADU)
	else {
	    iferr {
	        padu = imgetr (im, Memc[key])
	    } then {
		padu = dp_statr (dao, PHOTADU)
		call eprintf ("Warning: Image %s  Keyword %s not found.\n")
		    call pargstr (IM_HDRFILE(im))
		    call pargstr (Memc[key])
	    }
	}
	if (IS_INDEFR(padu) || padu <= 0.0)
	    call dp_setr (dao, PHOTADU, 1.0)
	else
	    call dp_setr (dao, PHOTADU, padu)
	call sfree (sp)
end
