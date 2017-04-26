include <fset.h>
include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/fitsky.h"
include "../lib/polyphot.h"

# AP_YBPHOT -- Measure the flux inside a list of polygons.

procedure ap_ybphot (py, im, cl, pl, out, id, ld, pd, gid, interactive)

pointer py			# pointer to apphot structure
pointer	im			# pointer to IRAF image
int	cl			# coordinates file descriptor
int	pl			# vertices list file descriptor
int	out			# output file descriptor
int	id			# output file sequence number
int	ld			# coordinate list number
int	pd			# polygon list number
pointer	gid			# pointer to image display stream
int	interactive		# interactive or batch mode

real	apstatr()
pointer	sp, x, y, xout, yout
int	req_num, prev_num, cier, sier, pier, nvertices, delim
int	ap_ynextobj(), ap_ycenter(), apfitsky(), ap_yfit(), apstati()
data	delim /';'/

begin
	# Allocate temporary space for arrays.
	call smark (sp)
	call salloc (x, MAX_NVERTICES + 1, TY_REAL)
	call salloc (y, MAX_NVERTICES + 1, TY_REAL)
	call salloc (xout, MAX_NVERTICES + 1, TY_REAL)
	call salloc (yout, MAX_NVERTICES + 1, TY_REAL)

	# Initialize
	if (pl != NULL)
	    call seek (pl, BOF)
	if (cl != NULL)
	    call seek (cl, BOF)

	# Get the first polygon.
	pd = 0
	prev_num = 0
	req_num = ld + 1
	nvertices = ap_ynextobj (py, im, gid, pl, cl, delim, Memr[x], Memr[y],
	    MAX_NVERTICES, prev_num, req_num, ld, pd)

	while (nvertices != EOF) {
	    
	    # Fit the center, sky and measure the polygon.
	    cier = ap_ycenter (py, im, apstatr (py, PYCX), apstatr (py, PYCY),
	        Memr[x], Memr[y], nvertices + 1)
	    sier = apfitsky (py, im, apstatr (py, PYCX), apstatr (py,
	        PYCY), NULL, NULL)
	    pier = ap_yfit (py, im, Memr[x], Memr[y],
		nvertices + 1, apstatr (py, SKY_MODE), apstatr (py,
		SKY_SIGMA), apstati (py, NSKY))

	    # Write the output to the standard output.
	    if (interactive == YES) {
	        call ap_qyprint (py, cier, sier, pier)
		if (gid != NULL)
		    call appymark (py, gid, Memr[x], Memr[y], nvertices + 1,
		        YES, apstati (py, MKSKY), apstati (py, MKPOLYGON))
	    }

	    # Write the output to a file.
	    if (id == 1)
	        call ap_param (py, out, "polyphot")
            switch (apstati(py,WCSOUT)) {
            case WCS_WORLD, WCS_PHYSICAL:
                call ap_ltoo (py, Memr[x], Memr[y], Memr[xout], Memr[yout],
		    nvertices + 1)
            case WCS_TV:
                call ap_ltov (im, Memr[x], Memr[y], Memr[xout], Memr[yout],
		    nvertices + 1)
            default:
		call amovr (Memr[x], Memr[xout], nvertices + 1)
		call amovr (Memr[y], Memr[yout], nvertices + 1)
            }
	    call ap_yprint (py, out, Memr[xout], Memr[yout], nvertices, id, ld,
	        pd, cier, sier, pier) 
	    id = id + 1

	    # Setup for next polygon.
	    prev_num = ld
	    req_num = ld + 1
	    nvertices = ap_ynextobj (py, im, gid, pl, cl, delim, Memr[x],
	        Memr[y], MAX_NVERTICES, prev_num, req_num, ld, pd)
	}   

	call sfree (sp)
end
