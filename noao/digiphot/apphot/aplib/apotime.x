include <imhdr.h>
include "../lib/apphot.h"

# AP_OTIME --  Fetch the time or epoch of the observation from the image
# header.

procedure ap_otime (im, ap)

pointer	im		# pointer to IRAF image
pointer	ap		# pointer to apphot structure

char	timechar
int	index
pointer	sp, key, otime
bool	streq()
int	strldx()

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (otime, SZ_FNAME, TY_CHAR)

	call apstats (ap, OBSTIME, Memc[key], SZ_FNAME)
	Memc[otime] = EOS
	if (Memc[key] == EOS)
	    call apstats (ap, OTIME, Memc[otime], SZ_FNAME)
	else {
	    iferr { 
	        call imgstr (im, Memc[key], Memc[otime], SZ_FNAME)
	    } then {
	        call apstats (ap, OTIME, Memc[otime], SZ_FNAME)
		call eprintf ("Warning: Image %s  Keyword: %s not found\n")
		    call pargstr (IM_HDRFILE(im))
		    call pargstr (Memc[key])
	    }
	}
	if (Memc[otime] == EOS) {
	    call apsets (ap, OTIME, "INDEF")
	} else if (streq ("DATE-OBS", Memc[key]) || streq ("date-obs",
	    Memc[key])) {
	    timechar = 'T'
	    index = strldx (timechar, Memc[otime])
	    if (index > 0)
		call apsets (ap, OTIME, Memc[otime+index])
	    else
	        call apsets (ap, OTIME, "INDEF")
	} else {
	    call apsets (ap, OTIME, Memc[otime])
	}

	call sfree (sp)
end
