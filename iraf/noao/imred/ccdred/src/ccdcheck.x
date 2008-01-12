include	<imhdr.h>
include	"ccdtypes.h"

# CCDCHECK -- Check processing status.

bool procedure ccdcheck (im, ccdtype)

pointer	im			# IMIO pointer
int	ccdtype			# CCD type

real	ccdmean, hdmgetr()
bool	clgetb(), ccdflag()
long	time
int	hdmgeti()

begin
	if (clgetb ("trim") && !ccdflag (im, "trim"))
	    return (true)
	if (clgetb ("fixpix") && !ccdflag (im, "fixpix"))
	    return (true)
	if (clgetb ("overscan") && !ccdflag (im, "overscan"))
	    return (true)

	switch (ccdtype) {
	case ZERO:
	    if (clgetb ("readcor") && !ccdflag (im, "readcor"))
	        return (true)
	case DARK:
	    if (clgetb ("zerocor") && !ccdflag (im, "zerocor"))
	        return (true)
	case FLAT:
	    if (clgetb ("zerocor") && !ccdflag (im, "zerocor"))
	        return (true)
	    if (clgetb ("darkcor") && !ccdflag (im, "darkcor"))
	        return (true)
	    if (clgetb ("scancor") && !ccdflag (im, "scancor"))
	        return (true)
	    iferr (ccdmean = hdmgetr (im, "ccdmean"))
		    return (true)
	    iferr (time = hdmgeti (im, "ccdmeant"))
		time = IM_MTIME(im)
	    if (time < IM_MTIME(im))
		return (true)
	case ILLUM:
	    if (clgetb ("zerocor") && !ccdflag (im, "zerocor"))
	        return (true)
	    if (clgetb ("darkcor") && !ccdflag (im, "darkcor"))
	        return (true)
	    if (clgetb ("flatcor") && !ccdflag (im, "flatcor"))
	        return (true)
	    iferr (ccdmean = hdmgetr (im, "ccdmean"))
		return (true)
	default:
	    if (clgetb ("zerocor") && !ccdflag (im, "zerocor"))
	        return (true)
	    if (clgetb ("darkcor") && !ccdflag (im, "darkcor"))
	        return (true)
	    if (clgetb ("flatcor") && !ccdflag (im, "flatcor"))
	        return (true)
	    if (clgetb ("illumcor") && !ccdflag (im, "illumcor"))
	        return (true)
	    if (clgetb ("fringecor") && !ccdflag (im, "fringcor"))
	        return (true)
	}

	return (false)
end
