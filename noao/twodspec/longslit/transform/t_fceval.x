# T_FCEVAL -- Evaluate FITCOORDS solutions.
# Input consists of a text file of pixel coordinates to be evaluated and the
# user coordinate surfaces from FITCOORDS.  The output is a text file of the
# input coordinates followed by the output coordinates.  When there is no fit
# for an axis the unit transformation is used and when there is more than one
# fit for an axis the average is used.

procedure t_fceval ()

pointer	input			# File of input coordinates
pointer	output			# File of output coordinates
int	fitnames		# List of user coordinate fits
pointer	database		# Database

int	i, j, in, out, nsf[2]
double	x[2], y[2]
pointer	sp, fitname, sf[2], un[2], sf1, un1

bool	un_compare()
int	open(), fscan(), nscan()
int	clpopnu(), clplen(), clgfil()
double	dgseval()
errchk	open, lm_dbread

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (database, SZ_FNAME, TY_CHAR)
	call salloc (fitname, SZ_FNAME, TY_CHAR)

	# Get parameters.
	call clgstr ("input", Memc[input], SZ_FNAME)
	call clgstr ("output", Memc[output], SZ_FNAME)
	fitnames = clpopnu ("fitnames")
	call clgstr ("database", Memc[database], SZ_FNAME)

	# Open the input and output files.
	in = open (Memc[input], READ_ONLY, TEXT_FILE)
	out = open (Memc[output], NEW_FILE, TEXT_FILE)

	# Read the solutions.
	i = max (1, clplen (fitnames))
	call salloc (sf[1], i, TY_INT)
	call salloc (sf[2], i, TY_INT)

	nsf[1] = 0; nsf[2] = 0; un[1] = NULL; un[2] = NULL
	while (clgfil (fitnames, Memc[fitname], SZ_FNAME) != EOF) {
	    call lm_dbread (Memc[database], Memc[fitname], j, un1, sf1)
	    if (un1 != NULL) {
		if (un[j] == NULL)
		    un[j] = un1
		else if (un_compare (un1, un[j]))
		    call un_close (un1)
		else
		    call error (1, "Input units disagree")
	    }

	    if (sf1 != NULL) {
		Memi[sf[j]+nsf[j]] = sf1
		nsf[j] = nsf[j] + 1
	    }
	}

	if (nsf[1] + nsf[2] == 0)
	    call error (0, "No user coordinates")

	# Evaluate the fits at each input coordinate.
	while (fscan (in) != EOF) {
	    call gargd (x[1])
	    call gargd (x[2])
	    if (nscan() != 2)
		next

	    do j = 1, 2 {
		if (nsf[j] == 0)
		    y[j] = x[j]
		else {
		    y[j] = dgseval (Memi[sf[j]], x[1], x[2])
		    do i = 2, nsf[1]
			y[j]  = y[j]  + dgseval (Memi[sf[j]+i-1], x[1], y[2])
		    y[j]  = y[j]  / nsf[j]
		}
	    }

	    call fprintf (out, "%g %g %g %g\n")
		call pargd (x[1])
		call pargd (x[2])
		call pargd (y[1])
		call pargd (y[2])
	    call flush (out)
	}

	# Free the surfaces and units structures.
	do j = 1, 2 {
	    for (i=1; i<=nsf[j]; i=i+1)
		call dgsfree (Memi[sf[j]+i-1])
	    if (un[j] != NULL)
		call un_close (un[j])
	}

	# Finish up.
	call clpcls (fitnames)
	call close (out)
	call close (in)
	call sfree (sp)
end
