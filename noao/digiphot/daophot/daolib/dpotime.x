include <imhdr.h>
include "../lib/daophotdef.h"

# DP_OTIME -- Read the epoch of the observation from the image header.

procedure dp_otime (im, dao)

pointer	im		# pointer to IRAF image
pointer	dao		# pointer to the daophot structure

char	timechar
int	index
pointer	sp, key, otime
bool	streq()
int	strldx()

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (otime, SZ_FNAME, TY_CHAR)

	call dp_stats (dao, OBSTIME, Memc[key], SZ_FNAME)
	Memc[otime] = EOS
	if (Memc[key] == EOS)
	    call dp_stats (dao, OTIME, Memc[otime], SZ_FNAME)
	else {
	    iferr { 
	        call imgstr (im, Memc[key], Memc[otime], SZ_FNAME)
	    } then {
	        call dp_stats (dao, OTIME, Memc[otime], SZ_FNAME)
		call eprintf ("Warning: Image %s  Keyword: %s not found\n")
		    call pargstr (IM_HDRFILE(im))
		    call pargstr (Memc[key])
	    }
	}
	if (Memc[otime] == EOS) {
	    call dp_sets (dao, OTIME, "INDEF")
        } else if (streq ("DATE-OBS", Memc[key]) || streq ("date-obs",
            Memc[key])) {
            timechar = 'T'
            index = strldx (timechar, Memc[otime])
            if (index > 0)
                call dp_sets (dao, OTIME, Memc[otime+index])
            else
                call dp_sets (dao, OTIME, "INDEF")
	} else {
	    call dp_sets (dao, OTIME, Memc[otime])
	}

	call sfree (sp)
end
