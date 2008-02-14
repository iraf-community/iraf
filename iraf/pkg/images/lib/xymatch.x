include "xyxymatch.h"

# RG_RDXYI -- Read in the x and y coordinates from a file and set the
# line number index.

int procedure rg_rdxyi (fd, x, y, lineno, xcolumn, ycolumn)

int	fd			#I the input file descriptor
pointer	x			#U pointer to the x coordinates
pointer	y			#U pointer to the y coordinates
pointer	lineno			#U pointer to the line numbers
int	xcolumn			#I column containing the x coordinate
int	ycolumn			#I column containing the y coordinate

int	i, ip, bufsize, npts, lnpts, maxcols
pointer	sp, str
real	xval, yval
int	fscan(), nscan(), ctor()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	bufsize = DEF_BUFSIZE
	call malloc (x, bufsize, TY_REAL)
	call malloc (y, bufsize, TY_REAL)
	call malloc (lineno, bufsize, TY_INT)
	maxcols = max (xcolumn, ycolumn)

	npts = 0
	lnpts = 0
	while (fscan(fd) != EOF) {

	    lnpts = lnpts + 1
	    xval = INDEFR
	    yval = INDEFR
	    do i = 1, maxcols {
		call gargwrd (Memc[str], SZ_LINE)
		if (i != nscan())
		    break
		ip = 1
		if (i == xcolumn) {
		    if (ctor (Memc[str], ip, xval) <= 0)
			xval = INDEFR
		} else if (i == ycolumn) {
		    if (ctor (Memc[str], ip, yval) <= 0)
			yval = INDEFR
		}
	    }
	    if (IS_INDEFR(xval) || IS_INDEFR(yval))
		next

	    Memr[x+npts] = xval
	    Memr[y+npts] = yval
	    Memi[lineno+npts] = lnpts
	    npts = npts + 1
	    if (npts >= bufsize) {
		bufsize = bufsize + DEF_BUFSIZE
		call realloc (x, bufsize, TY_REAL)
		call realloc (y, bufsize, TY_REAL)
		call realloc (lineno, bufsize, TY_INT)
	    }
	}

	call sfree (sp)

	return (npts)
end


# RG_SORT --  If the coordinates are not already sorted, sort the coordinates
# first in y then in x. Remove points which are close together than a given
# tolerance, if the coincident point remove flag is on.

int procedure rg_sort (xcoord, ycoord, rsindex, npts, tolerance, sort, coincid)

real	xcoord[ARB]		#I pointer to the x coordinates
real	ycoord[ARB]		#I pointer to the y coordinates
int	rsindex[ARB]		#I pointer to sort index
int	npts			#I the number of objects
real	tolerance		#I coincidence tolerance in pixels
int	sort			#I sort the pixels ?
int	coincid			#I remove coincident points

int	i, ndif
int	rg_xycoincide()

begin
	# Initialize the sort index.
	do i = 1, npts
	    rsindex[i] = i

	# Sort the pixels in y and then x if the arrays are unsorted.
	if (sort == YES) {
	    call rg_qsortr (ycoord, rsindex, rsindex, npts)
	    call rg_sqsort (xcoord, ycoord, rsindex, rsindex, npts)
	}

	# Remove objects that are closer together than tolerance.
	if (coincid == NO)
	    ndif = npts
	else
	    ndif = rg_xycoincide (xcoord, ycoord, rsindex, rsindex, npts,
	        tolerance)

	return (ndif)
end


# RG_XYCOINCIDE -- Remove points from a list which are closer together than
# a specified tolerance. The arrays are assumed to be sorted first in y then
# in x.

int procedure rg_xycoincide (xcoord, ycoord, a, b, npts, tolerance)

real	xcoord[ARB]		#I the input x coordinate values
real	ycoord[ARB]		#I the input y coordinate values
int	a[ARB]			#I the input sort index
int	b[ARB]			#O the output sort index
int	npts			#I the  number of points
real	tolerance		#I the coincidence tolerace

int	iprev, i, nunique
real	tol2, r2

begin
	tol2 = tolerance ** 2
	nunique = npts

	iprev = 1
	repeat {

	    do i = iprev + 1, npts {

		# Jump to the next object if this one has been deleted
		# since all comparisons are then invalid.
		if (a[iprev] == 0)
		    break

		# Skip to the next object if this one has been deleted.
		if (a[i] == 0)
		    next

	        # Check the tolerance limit in y and step to the next object
	        # if the bounds are exceeded.
	        r2 = (ycoord[a[i]] - ycoord[a[iprev]]) ** 2
	        if (r2 > tol2)
		    break

	        # Check the tolerance limit.
	        r2 = r2 + (xcoord[a[i]] - xcoord[a[iprev]]) ** 2
	        if (r2 <= tol2) {
		    a[i] = 0
		    nunique = nunique - 1
	        }
	    }

	    iprev = iprev + 1

	} until (iprev >= npts)

	# Reorder the index array.
	if (nunique < npts) {
	    iprev = 0
	    do i = 1, npts {
		if (a[i] != 0) {
		    iprev = iprev + 1
		    b[iprev] = a[i]
		}
	    }
	}

	return (nunique)
end

