include "wcsxymatch.h"

define	DEF_BUFSIZE	200

# RG_RDXY -- Read in the x and y coordinates from a file.

int procedure rg_rdxy (fd, x, y, wcs, xcolumn, ycolumn, xunits, yunits)

int	fd			#I the input file descriptor
pointer	x			#U pointer to the x coordinates
pointer	y			#U pointer to the y coordinates
int	wcs			#I the world coordinate system
int	xcolumn			#I column containing the x coordinate
int	ycolumn			#I column containing the y coordinate
int	xunits			#I the x coordinate units
int	yunits			#I the y coordinate units

double	xval, yval
int	i, ip, bufsize, maxcols, npts
pointer	sp, str
int	fscan(), nscan(), ctod()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	bufsize = DEF_BUFSIZE
	call malloc (x, bufsize, TY_DOUBLE)
	call malloc (y, bufsize, TY_DOUBLE)
	maxcols = max (xcolumn, ycolumn)

	npts = 0
	while (fscan(fd) != EOF) {

	    xval = INDEFD
	    yval = INDEFD
	    do i = 1, maxcols {
		call gargwrd (Memc[str], SZ_FNAME)
		if (i != nscan())
		    break
		ip = 1
		if (i == xcolumn) {
		    if (ctod (Memc[str], ip, xval) <= 0)
			xval = INDEFD
		} else if (i == ycolumn) {
		    if (ctod (Memc[str], ip, yval) <= 0)
			yval = INDEFD
		}
	    }
	    if (IS_INDEFD(xval) || IS_INDEFD(yval))
		next

	    Memd[x+npts] = xval
	    Memd[y+npts] = yval
	    npts = npts + 1
	    if (npts >= bufsize) {
		bufsize = bufsize + DEF_BUFSIZE
		call realloc (x, bufsize, TY_DOUBLE)
		call realloc (y, bufsize, TY_DOUBLE)
	    }
	}

	# Convert the coordinates if necessary.
	switch (wcs) {
	case RG_WORLD:
	    if (xunits == RG_UHOURS)
		call amulkd (Memd[x], 15.0d0, Memd[x], npts)
	    if (yunits == RG_UHOURS)
		call amulkd (Memd[y], 15.0d0, Memd[y], npts)
	default:
	    ;
	}

	call sfree (sp)

	return (npts)
end
