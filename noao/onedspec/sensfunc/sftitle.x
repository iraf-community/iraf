include	"sensfunc.h"


# SF_TITLE -- Make title string for graphs.

procedure sf_title (gp, aperture, function, order, npts, rms)

pointer	gp
int	aperture
char	function[ARB]
int	order
int	npts
real	rms

begin
	call sprintf (GP_TITLE(gp), GP_SZTITLE,
	    "Aperture=%d  Function=%s  Order=%d  Points=%d  RMS=%6.4f")
	    call pargi (aperture)
	    call pargstr (function)
	    call pargi (order)
	    call pargi (npts)
	    call pargr (rms)
end
