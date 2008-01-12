include <fset.h>
include "../lib/apphot.h"
include "../lib/polyphot.h"

# AP_YNEXTOBJ -- Read the next polygon from the list polygon and/or coordinate
# list.

int procedure ap_ynextobj (py, im, id, pl, cl, delim, x, y, maxnver, prev_num,
    req_num, ld, pd)

pointer	py		# pointer to the apphot structre
pointer	im		# the input image descriptor
pointer	id		# pointer to image display stream
int	pl		# polygons file descriptor
int	cl		# coordinates file descriptor
int	delim		# delimiter character
real	x[ARB]		# x coordinates of the polygon vertices
real	y[ARB]		# y coordinates of the polygon vertices
int	maxnver		# maximum number of vertices
int	prev_num	# previous object
int	req_num		# requested object
int	ld		# current object
int	pd		# current polygon

real	xshift, yshift
pointer	sp, fname
int	stdin, nskip, ncount, nver, stat
real	apstatr()
int	strncmp(), ap_yget(), ap_ycoords(), apstati()
errchk	greactivate, gdeactivate, gscur

begin
	# The coordinate list is undefined.
	if (cl == NULL) {

	    # Get the input file name.
	    call smark (sp)
	    call salloc (fname, SZ_FNAME, TY_CHAR)
	    call fstats (pl, F_FILENAME, Memc[fname], SZ_FNAME)

	    # Compute the number of polygons that must be skipped.
	    if (strncmp ("STDIN", Memc[fname], 5) == 0) {
		stdin = YES
		nskip = 1
	    } else {
		stdin = NO
		if (req_num <= prev_num) {
		    call seek (pl, BOF)
		    nskip = req_num
		} else
		    nskip = req_num - prev_num
	    }

	    # Initialize the polygon search.
	    ncount = 0
	    pd = prev_num

	    # Find the correct polygon.
	    repeat {

		call apsetr (py, PYX, apstatr (py, PYCX))
		call apsetr (py, PYY, apstatr (py, PYCY))
		nver = ap_yget (py, im, pl, delim, x, y, maxnver)

		if (nver == EOF) {
		    ncount = EOF
		}  else if (nver > 0) {
		    ncount = ncount + 1
		    pd = pd + 1
		}

	    } until (ncount == EOF || ncount == nskip)

	    # Return the polygon number.
	    if (req_num <= prev_num)
		pd = ncount 
	    ld = pd

	    call sfree (sp)

	    if (ncount == EOF) {
		return (EOF)
	    } else if (id != NULL) {
		iferr {
		    call greactivate (id, 0)
		    call gscur (id, apstatr (py, PYCX), apstatr (py, PYCY))
		    call gdeactivate (id, 0)
		} then
		    ;
		return (nver)
	     } else {
		return (nver)
	    }

	# Both the polygon and coordinate file are defined.
	} else {

	    # Get the input file name.
	    call smark (sp)
	    call salloc (fname, SZ_FNAME, TY_CHAR)
	    call fstats (cl, F_FILENAME, Memc[fname], SZ_FNAME)

	    # Compute the number of objects that must be skipped.
	    if (strncmp ("STDIN", Memc[fname], 5) == 0) {
		stdin = YES
		nskip = 1
	    } else {
		stdin = NO
		if (req_num <= prev_num) {
		    call seek (cl, BOF)
		    call seek (pl, BOF)
		    pd = 0
		    nskip = req_num
		} else
		    nskip = req_num - prev_num
	    }

	    # Initialize the polygon id.
	    ncount = 0
	    ld = prev_num

	    # Find the correct object and shift the coordinates.
	    repeat {

		# Read the coordinates and the polygon.
		stat = ap_ycoords (cl, delim, xshift, yshift, stdin)
		if (stat == EOF) {
		    ncount = EOF
		} else if (stat == NEXT_POLYGON || pd == 0) {
		    call apsetr (py, PYX, apstatr (py, PYCX))
		    call apsetr (py, PYY, apstatr (py, PYCY))
		    nver = ap_yget (py, im, pl, delim, x, y, maxnver)
		    if (nver == EOF)
		        ncount = EOF
		    else if (nver > 0)
		        pd = pd + 1
		}

		# Shift the polygon coordinates.
		if (stat == THIS_OBJECT && ncount != EOF && nver > 0) {
            	    switch (apstati(py,WCSIN)) {
            	    case WCS_WORLD, WCS_PHYSICAL:
                	call ap_itol (py, xshift, yshift, xshift, yshift, 1)
            	    case WCS_TV:
                	call ap_vtol (im, xshift, yshift, xshift, yshift, 1)
            	    default:
                	;
            	    }
		    call aaddkr (x, (xshift - apstatr (py, PYCX)), x, nver + 1)
		    call aaddkr (y, (yshift - apstatr (py, PYCY)), y, nver + 1)
		    call apsetr (py, PYCX, xshift)
		    call apsetr (py, PYCY, yshift)
                    switch (apstati(py,WCSOUT)) {
            	    case WCS_WORLD, WCS_PHYSICAL:
                	call ap_ltoo (py, xshift, yshift, xshift, yshift, 1)
            	    case WCS_TV:
                	call ap_ltov (im, xshift, yshift, xshift, yshift, 1)
            	    default:
    			;
            	    }
		    call apsetr (py, OPYCX, xshift)
		    call apsetr (py, OPYCY, yshift)
		    ncount = ncount + 1
		    ld = ld + 1
		}

	    } until (ncount == EOF || ncount == nskip)

	    # Get the correct id.
	    if (req_num <= prev_num)
		ld = ncount

	    call sfree (sp)

	    if (ncount == EOF) {
		return (EOF)
	    } else if (id != NULL) {
		iferr {
		    call greactivate (id, 0)
		    call gscur (id, apstatr (py, PYCX), apstatr (py, PYCY))
		    call gdeactivate (id, 0)
		} then
		    ;
		return (nver)
	    } else {
		return (nver)
	    }
	}

end
