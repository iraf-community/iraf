include <imhdr.h>
include "../lib/daophot.h"

# DP_RDNOISE -  Procedure to set the image read noise parameter.

procedure dp_rdnoise (im, dao)

pointer	im		# pointer to IRAF image
pointer	dao		# pointer to the daophot structure

pointer	sp, key
real	rdnoise
real	imgetr(), dp_statr()

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call dp_stats (dao, CCDREAD, Memc[key], SZ_FNAME)
	if (Memc[key] == EOS)
	    rdnoise = dp_statr (dao, READ_NOISE)
	else {
	    iferr {
	        rdnoise = imgetr (im, Memc[key])
	    } then {
		rdnoise = dp_statr (dao, READ_NOISE)
		call eprintf ("Warning: Image %s  Keyword %s not found.\n")
		    call pargstr (IM_HDRFILE(im))
		    call pargstr (Memc[key])
	    }
	}
	if (IS_INDEFR(rdnoise) || rdnoise <= 0.0)
	    call dp_setr (dao, READ_NOISE, 0.0)
	else
	    call dp_setr (dao, READ_NOISE, rdnoise)
	call sfree (sp)
end
