include <fset.h>
include "../lib/apphot.h"
include "../lib/fitsky.h"
include "../lib/polyphot.h"

# AP_YGET -- Procedure to fetch the coordinates of the vertices of a
# polygon from a text file.

int procedure ap_yget (py, im, fd, delim, x, y, max_nvertices)

pointer	py		# polyphot structure
pointer	im		# the input image descriptor
int	fd		# polygon file descriptor
int	delim		# delimiter character
real	x[ARB]		# x coords of vertices
real	y[ARB]		# y coords of vertices
int	max_nvertices	# maximum number of vertices

real	xc, yc, r2max, r2, xtemp, ytemp
pointer	sp, str
int	i, nvertices, nreal, stat
char	marker
real	asumr()
int	fscan(), nscan(), strncmp(), apstati()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Print prompts if input file is STDIN.
	call fstats (fd, F_FILENAME, Memc[str], SZ_LINE)
	if (strncmp ("STDIN", Memc[str], 5) == 0) {
	    call printf ("Type x and y coordinates of vertex\n")
	    call printf ("\t1 vertex per line\n")
	    call printf ("\t; to end polygon\n")
	    call printf ("\t^Z to end list\n")
	    call flush (STDOUT)
	}

	# Get the polygon.
	nvertices = 0
	stat = fscan (fd)
	while (stat != EOF) {

	    # Read the vertices from the file
	    call gargr (xtemp)
	    call gargr (ytemp)
	    nreal = nscan ()
	    if (nreal != 2) {
		call reset_scan ()
		call gargc (marker)
		if (int (marker) == delim)
		    break
		nreal = nscan ()
	    }

	    # Store the vertices.
	    if (nreal >= 2) {
	        nvertices = nvertices + 1
	        if (nvertices <= max_nvertices) {
		    x[nvertices] = xtemp
		    y[nvertices] = ytemp
	        }
	    }

	    stat = fscan (fd)
	}

	if (nvertices == 0 && stat == EOF) {

	    nvertices = EOF
	    call apsetr (py, PYXMEAN, INDEFR)
	    call apsetr (py, PYYMEAN, INDEFR)
	    call apsetr (py, OPYXMEAN, INDEFR)
	    call apsetr (py, OPYYMEAN, INDEFR)
	    call apsetr (py, PYCX, INDEFR)
	    call apsetr (py, PYCY, INDEFR)
	    call apsetr (py, OPYCX, INDEFR)
	    call apsetr (py, OPYCY, INDEFR)
	    call apsetr (py, PYMINRAD, INDEFR)
	    call apseti (py, PYNVER, 0)

	} else if (nvertices <= 2) {

	    call apsetr (py, PYXMEAN, INDEFR)
	    call apsetr (py, PYYMEAN, INDEFR)
	    call apsetr (py, OPYXMEAN, INDEFR)
	    call apsetr (py, OPYYMEAN, INDEFR)
	    call apsetr (py, PYCX, INDEFR)
	    call apsetr (py, PYCY, INDEFR)
	    call apsetr (py, OPYCX, INDEFR)
	    call apsetr (py, OPYCY, INDEFR)
	    call apsetr (py, PYMINRAD, INDEFR)
	    call apseti (py, PYNVER, 0)
	    nvertices = 0

	} else {

	    # Add in the last vertex.
	    x[nvertices+1] = x[1]
	    y[nvertices+1] = y[1]

            # Transform the input coordinates.
            switch (apstati(py,WCSIN)) {
            case WCS_WORLD, WCS_PHYSICAL:
                call ap_itol (py, x, y, x, y, nvertices + 1)
            case WCS_TV:
                call ap_vtol (im, x, y, x, y, nvertices + 1)
            default:
                ;
            }

	    # Compute the mean polygon coordinates.
	    xc = asumr (x, nvertices) / nvertices
	    yc = asumr (y, nvertices) / nvertices
	    call apsetr (py, PYXMEAN, xc)
	    call apsetr (py, PYYMEAN, yc)
	    call apsetr (py, PYCX, xc)
	    call apsetr (py, PYCY, yc)

	    # Set the minimum size of the sky annulus.
	    r2max = 0.0
	    do i = 1, nvertices {
		r2 = (x[i] - xc) ** 2 + (y[i] - yc) ** 2
		if (r2 > r2max)
		    r2max = r2
	    }

            switch (apstati(py,WCSOUT)) {
            case WCS_WORLD, WCS_PHYSICAL:
                call ap_ltoo (py, xc, yc, xc, yc, 1)
            case WCS_TV:
                call ap_ltov (im, xc, yc, xc, yc, 1)
            default:
    		;
	    }
	    call apsetr (py, OPYXMEAN, xc)
	    call apsetr (py, OPYYMEAN, yc)
	    call apsetr (py, OPYCX, xc)
	    call apsetr (py, OPYCY, yc)

	    call apsetr (py, PYMINRAD, (sqrt (r2max) + 1.1))
	    call apseti (py, PYNVER, nvertices)
	}

	call sfree (sp)
	return (nvertices)
end


# AP_YMKPOLY -- Mark the coordinates of a polygon on the display device.

int procedure ap_ymkpoly (py, im, id, x, y, max_nvertices, idflush)

pointer	py		# polyphot structure
pointer	im		# the input image descriptor
pointer	id		# display pointer
real	x[ARB]		# x coords of vertices
real	y[ARB]		# y coords of vertices
int	max_nvertices	# maximum number of vertices
int	idflush		# flush the imd interface ?

int	i, nvertices, stat, wcs, key
pointer	sp, cmd
real	xtemp, ytemp, xc, yc, r2max, r2
real	apstatr(), asumr()
int	clgcur(), apstati()
errchk	gscur

begin
	# Reopen the device.
	if (id != NULL)
	    call greactivate (id, 0)

	# Initialize.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Type prompt.
	call printf (
	    "Mark polygon vertex [space=mark,q=quit].\n")
	stat = clgcur ("icommands", xtemp, ytemp, wcs, key, Memc[cmd], SZ_LINE)
	call ap_vtol (im, xtemp, ytemp, xtemp, ytemp, 1)


	# Fetch the polygon and draw it on the display.
	nvertices = 0
	while (stat != EOF) {

	    if (key == 'q')
		break

	    # Decode and draw vertices.
	    nvertices = nvertices + 1
	    if (nvertices <= max_nvertices) {
		x[nvertices] = xtemp
		y[nvertices] = ytemp
		if (id != NULL) {
		    if (nvertices == 1)
		        call gamove (id, x[1], y[1])
		    else {
		        call gadraw (id, x[nvertices], y[nvertices])
			if (idflush == YES)
			    call gframe (id)
			else
			    call gflush (id)
		    }
		}
	    } else
		break

	    # Type prompt.
	    call printf (
	        "Mark polygon vertex [space=mark,q=quit].\n")
	    stat = clgcur ("icommands", xtemp, ytemp, wcs, key, Memc[cmd],
	        SZ_LINE)
	    call ap_vtol (im, xtemp, ytemp, xtemp, ytemp, 1)
	}
	call printf ("\n")

	call sfree (sp)

	# Return EOF or the number of vertices in the polygon.
	if (stat == EOF) {

	    call apsetr (py, PYXMEAN, INDEFR)
	    call apsetr (py, PYYMEAN, INDEFR)
	    call apsetr (py, OPYXMEAN, INDEFR)
	    call apsetr (py, OPYYMEAN, INDEFR)
	    call apsetr (py, PYCX, INDEFR)
	    call apsetr (py, PYCY, INDEFR)
	    call apsetr (py, OPYCX, INDEFR)
	    call apsetr (py, OPYCY, INDEFR)
	    call apsetr (py, PYMINRAD, INDEFR)
	    call apseti (py, PYNVER, 0)
	    nvertices = EOF

	} else if (nvertices  <= 2) {

	    call apsetr (py, PYXMEAN, INDEFR)
	    call apsetr (py, PYYMEAN, INDEFR)
	    call apsetr (py, OPYXMEAN, INDEFR)
	    call apsetr (py, OPYYMEAN, INDEFR)
	    call apsetr (py, PYCX, INDEFR)
	    call apsetr (py, PYCY, INDEFR)
	    call apsetr (py, OPYCX, INDEFR)
	    call apsetr (py, OPYCY, INDEFR)
	    call apsetr (py, PYMINRAD, INDEFR)
	    call apseti (py, PYNVER, 0)
	    nvertices = 0

	} else {

	    # Add the last vertex and close the polygon.
	    x[nvertices+1] = x[1]
	    y[nvertices+1] = y[1]
	    if (id != NULL) {
	        call gadraw (id, x[1], y[1])
		if (idflush == YES)
		    call gframe (id)
		else
	            call gflush (id)
	    }

	    # Compute and save the mean polygon coords.
	    xc = asumr (x, nvertices) / nvertices
	    yc = asumr (y, nvertices) / nvertices
	    call apsetr (py, PYXMEAN, xc)
	    call apsetr (py, PYYMEAN, yc)
	    call apsetr (py, PYCX, xc)
	    call apsetr (py, PYCY, yc)
	    if (id != NULL)
	        call gscur (id, apstatr (py, PYCX), apstatr (py, PYCY))

	    # Compute the mean sky annulus.
	    r2max = 0.0
	    do i = 1, nvertices {
		r2 = (x[i] - xc) ** 2 + (y[i] - yc) ** 2
		if (r2 > r2max)
		    r2max = r2
	    }

	    # Compute output coordinate centers.
            switch (apstati(py,WCSOUT)) {
            case WCS_WORLD, WCS_PHYSICAL:
                call ap_ltoo (py, xc, yc, xc, yc, 1)
            case WCS_TV:
                call ap_ltov (im, xc, yc, xc, yc, 1)
            default:
                ;
            }
            call apsetr (py, OPYXMEAN, xc)
            call apsetr (py, OPYYMEAN, yc)
            call apsetr (py, OPYCX, xc)
            call apsetr (py, OPYCY, yc)

	    call apseti (py, PYNVER, nvertices)
	    call apsetr (py, PYMINRAD, (sqrt (r2max) + 1.1))

	}

	if (id != NULL)
	    call gdeactivate (id, 0)

	return (nvertices)
end
