include	"apertures.h"

# AP_GMARK -- Mark an aperture.

define	SZ_TEXT	10	# Maximum size of aperture number string

procedure ap_gmark (gp, imvec, aps, naps)

pointer	gp			# GIO pointer
int	imvec			# Image vector
pointer	aps[ARB]		# Aperture data
int	naps			# Number of apertures

int	i, apaxis
real	x1, x2, y1, y2, dy, xc, xl, xu
pointer	sp, text, format, ap

int	itoc()
real	cveval()

begin
	# The aperture is marked at the top of the graph.
	call smark (sp)
	call salloc (text, SZ_TEXT, TY_CHAR)

	call ggwind (gp, xl, xu, y1, y2)
	x1 = min (xl, xu)
	x2 = max (xl, xu)
	dy = 0.025 * (y2 - y1)
	y1 = y2 - 4 * dy

	if (naps > 20) {
	    call salloc (format, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[format], SZ_LINE, "h=c,v=b,s=%4.2f")
		call pargr (20. / naps)
	}
 
	for (i = 1; i <= naps; i = i + 1) {
	    ap = aps[i]
	    apaxis = AP_AXIS(ap)

	    xc = AP_CEN(ap, apaxis) + cveval (AP_CV(ap), real (imvec))
	    xl = xc + AP_LOW(ap, apaxis)
	    xu = xc + AP_HIGH(ap, apaxis)
	    call gline (gp, xc, y1 - 2 * dy, xc, y1 + 2 * dy)
	    call gline (gp, xl, y1 - dy, xl, y1 + dy)
	    call gline (gp, xu, y1 - dy, xu, y1 + dy)
	    call gline (gp, xl, y1, xu, y1)
	    if ((xc > x1) && (xc < x2)) {
	        if (itoc (AP_ID(ap), Memc[text], SZ_TEXT) > 0) {
		    if (naps > 20)
		        call gtext (gp, xc, y1 + 2.5 * dy, Memc[text],
			    Memc[format])
		    else
		        call gtext (gp, xc, y1 + 2.5 * dy, Memc[text],
			    "h=c,v=b")
		}
	    }
	}

	call sfree (sp)
end
