include	<fset.h>

define	SZ_TABLE	200
define	LINEAR		1
define	SPLINE		2


# T_INTERP -- Interpolate for values in a table
#
# A table of x,y pairs contained in a file is used to
# find interpolated values, y, for any other given independent
# variable, x. Extrapolation is performed if necessary.
#
# A series of values may be generated to generate a fine grid
# through a coarse sampling for purposes of plotting. This is
# done by setting the hidden parameter curve_gen to yes.
# The starting point, ending point, and sampling interval
# are also needed in this case (x1, x2, dx).
#
# If only a small number of values are needed to be interpolated
# from the table, the user may enter a number of x's from either
# a file or STDIN.
# 
# The number of elements in the table is limited to SZ_TABLE
# which is currently set to 200.

procedure t_interp()

real	xtab[SZ_TABLE], ytab[SZ_TABLE]
real	x, y, x1, x2, dx
int	npts, ierr
int	filelist, tbl, in, imode
char	fname[SZ_FNAME], tbl_file[SZ_FNAME], mode[SZ_FNAME]
bool	gen

int	clpopni(), clgfil(), open(), fscan(), strncmp(), nscan()
real	clgetr()
bool	clgetb()

begin
	# Initialize interpolator
	call intrp0 (1)

	# File containing x,y pairs in a table
	call clgstr ("tbl_file", tbl_file, SZ_FNAME)

	# Open table file and read as many points as possible
	tbl = open (tbl_file, READ_ONLY, TEXT_FILE)

	npts = 0

	while (fscan(tbl) != EOF) {
	    npts = npts + 1
	    if (npts > SZ_TABLE)
		call error (1, "Maximum table size [SZ_TABLE] exceeded.")
	    call gargr (xtab[npts])
	    call gargr (ytab[npts])
	    if (nscan() < 2) {
		call eprintf ("Error reading x,y pairs\n")
		npts = npts - 1
	    }
	}

	call close (tbl)

	if (npts < 1)
	    call error (1, "Table has no entries.")

	# Linear or spline interpolator?
	call clgstr ("int_mode", mode, SZ_FNAME)
	if (strncmp (mode, "linear", 6) == 0)
	    imode = LINEAR
	else
	    imode = SPLINE

	# Generate a curve?
	gen = clgetb ("curve_gen")
	if (gen) {
	    x1 = clgetr ("x1")
	    x2 = clgetr ("x2")
	    dx = clgetr ("dx")

	    # Verify that dx will not cause an infinite loop
	    if (dx == 0.0 || dx * (x2-x1) < 0.0)
		call error (1, "Interval paramater dx implies infinite loop.")

	    for (x=x1; x <= x2; x = x+dx) {
		call intrp (1, xtab, ytab, npts, x, y, ierr)
		call printf ("%12.5g  %12.5g\n")
		    call pargr (x)
		    call pargr (y)
	    }

	# No, just one point at a time
	} else {

	    # Open input list
	    filelist = clpopni ("input")

	    while (clgfil (filelist, fname, SZ_FNAME) != EOF) {
		call fseti (STDOUT, F_FLUSHNL, YES)
		in = open (fname, READ_ONLY, TEXT_FILE)

		# Process input requests
		while (fscan(in) != EOF) {
		    call gargr (x)
		    if (imode == LINEAR)
			call lintrp (1, xtab, ytab, npts, x, y, ierr)
		    else
			call intrp  (1, xtab, ytab, npts, x, y, ierr)

		    call printf ("%12.5g  %12.5g\n")
			call pargr (x)
			call pargr (y)
		}

		call close (in)
	    }

	    call clpcls (filelist)
	}
end
