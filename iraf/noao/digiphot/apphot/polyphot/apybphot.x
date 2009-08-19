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
long	id			# output file sequence number
long	ld			# coordinate list number
long	pd			# polygon list number
pointer	gid			# pointer to image display stream
int	interactive		# interactive or batch mode

size_t	sz_val
pointer	sp, x, y, xout, yout
int	cier, sier, pier, nvertices, delim
long	req_num, prev_num, l_val
int	ap_ynextobj(), ap_ycenter(), apfitsky(), ap_yfit(), apstati()
long	apstatl()
real	apstatr()
data	delim /';'/

include	<nullptr.inc>

begin
	# Allocate temporary space for arrays.
	call smark (sp)
	sz_val = MAX_NVERTICES + 1
	call salloc (x, sz_val, TY_REAL)
	call salloc (y, sz_val, TY_REAL)
	call salloc (xout, sz_val, TY_REAL)
	call salloc (yout, sz_val, TY_REAL)

	# Initialize
	if (pl != NULL) {
	    l_val = BOF
	    call seek (pl, l_val)
	}
	if (cl != NULL) {
	    l_val = BOF
	    call seek (cl, l_val)
	}

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
	        PYCY), NULL, NULLPTR)
	    pier = ap_yfit (py, im, Memr[x], Memr[y],
		nvertices + 1, apstatr (py, SKY_MODE), apstatr (py,
		SKY_SIGMA), apstatl (py, NSKY))

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
		sz_val = nvertices + 1
                call ap_ltoo (py, Memr[x], Memr[y], Memr[xout], Memr[yout],
			      sz_val)
            case WCS_TV:
		sz_val = nvertices + 1
                call ap_ltov (im, Memr[x], Memr[y], Memr[xout], Memr[yout],
			      sz_val)
            default:
		sz_val = nvertices + 1
		call amovr (Memr[x], Memr[xout], sz_val)
		call amovr (Memr[y], Memr[yout], sz_val)
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
