include "../lib/apphot.h"
include "../lib/polyphot.h"

# AP_YWRITE -- Procedure to write a polygon to a file.

procedure ap_ywrite (py, cl, pl, x, y, nvertices, cid, pid, firstpoly, newpoly)

pointer	py		# pointer to the apphot structure
int	cl		# pointer to the coordinate list file
int	pl		# pointer to the polygon list file
real	x[ARB]		# x coordinates of the vertices
real	y[ARB]		# y coordinates of the vertices
int	nvertices	# number of vertices
int	cid		# coordinate list index
int	pid		# polygon list index
int	firstpoly	# first polygon measured
int	newpoly		# new polygon

int	i
real	apstatr()

begin
	# Make sure the output files are at EOF.
	if (pl == NULL)
	    return
	call seek (pl, EOF)
	if (cl != NULL)
	    call seek (cl, EOF)

	if (newpoly == YES) {

	    # Terminate the coord list that belongs with the first polygon.
	    if (firstpoly == NO && cl != NULL)
		call fprintf (cl, ";\n")

	    # Write out the coordinates.
	    do i = 1, nvertices {
		call fprintf (pl, "%g  %g\n")
		    call pargr (x[i])
		    call pargr (y[i])
	    }
	    if (nvertices > 0)
	        call fprintf (pl, ";\n")

	    pid = pid + 1

	    # Reset polygon parameters.
	    newpoly = NO
	    if (firstpoly == YES)
		firstpoly = NO
	}


	# Write out the central coordinates of the polygon.
	if (firstpoly == NO && cl != NULL) {

	    call fprintf (cl, "%g  %g\n")
		call pargr (apstatr (py, CWX))
		call pargr (apstatr (py, CWY))

	    cid = cid + 1
	}

	# Flush the output files.
	if (pl != NULL)
	    call flush (pl)
	if (cl != NULL)
	    call flush (cl)
end
