include "../lib/polyphot.h"

# APYDRAW -- Procedure to draw polygons interactively on the  display.

procedure ap_ydraw (py, cl, pl, ld, pd, id)

pointer py			# pointer to apphot structure
int	cl			# coordinates file descriptor
int	pl			# vertices list file descriptor
int	ld			# coordinate list number
int	pd			# polygon list number
pointer	id			# pointer to image display stream

int	req_num, prev_num, nvertices, delim
pointer	sp, x, y
int	ap_ynextobj()
data	delim /';'/

begin
	# Allocate temporary space for arrays.
	call smark (sp)
	call salloc (x, MAX_NVERTICES + 1, TY_REAL)
	call salloc (y, MAX_NVERTICES + 1, TY_REAL)

	# Initialize
	if (pl != NULL)
	    call seek (pl, BOF)
	if (cl != NULL)
	    call seek (cl, BOF)

	pd = 0
	prev_num = 0
	req_num = ld + 1

	nvertices = ap_ynextobj (py, id, pl, cl, delim, Memr[x], Memr[y],
	    MAX_NVERTICES, prev_num, req_num, ld, pd)

	# Loop over the coordinate and polygon file.
	while (nvertices != EOF) {
	    
	    # Write the output.
	    if (id != NULL)
		call appymark (py, id, Memr[x], Memr[y], nvertices + 1,
		    NO, NO, YES)

	    # Setup for next polygon.
	    prev_num = ld
	    req_num = ld + 1
	    nvertices = ap_ynextobj (py, id, pl, cl, delim, Memr[x], Memr[y],
	        MAX_NVERTICES, prev_num, req_num, ld, pd)
	}   

	call sfree (sp)
end
