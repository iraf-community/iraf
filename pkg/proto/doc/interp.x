include	<fset.h>

define	SZ_TABLE	4096
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


procedure t_interp()

double	x, y, x1, x2, dx
pointer	xtab, ytab
int	npts, ierr, tbsize
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

	# Allocate the initial arrays.
	call calloc (xtab, SZ_TABLE, TY_DOUBLE)
	call calloc (ytab, SZ_TABLE, TY_DOUBLE)
	tbsize = SZ_TABLE

	while (fscan(tbl) != EOF) {
	    npts = npts + 1
	    if (npts > tbsize) {
		call realloc (xtab, (tbsize+SZ_TABLE), TY_DOUBLE)
		call realloc (ytab, (tbsize+SZ_TABLE), TY_DOUBLE)
		tbsize = tbsize + SZ_TABLE
	    }
	    call gargd (Memd[xtab+npts-1])
	    call gargd (Memd[ytab+npts-1])
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
	    x1 = double(clgetr ("x1"))
	    x2 = double(clgetr ("x2"))
	    dx = double(clgetr ("dx"))

	    # Verify that dx will not cause an infinite loop
	    if (dx == 0.0 || dx * (x2-x1) < 0.0)
		call error (1, "Interval paramater dx implies infinite loop.")

	    for (x=x1; x <= x2; x = x+dx) {
		call intrp (1, Memd[xtab], Memd[ytab], npts, x, y, ierr)
		call printf ("%12.5g  %12.5g\n")
		    call pargd (x)
		    call pargd (y)
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
		    call gargd (x)
		    if (imode == LINEAR)
			call lintrp (1, Memd[xtab], Memd[ytab], npts, x,y, ierr)
		    else
			call intrp  (1, Memd[xtab], Memd[ytab], npts, x,y, ierr)

		    call printf ("%12.5g  %12.5g\n")
			call pargd (x)
			call pargd (y)
		}

		call close (in)
	    }

	    call clpcls (filelist)
	}

	# Free the pointers.
	call mfree (xtab, TY_DOUBLE)
	call mfree (ytab, TY_DOUBLE)
end
