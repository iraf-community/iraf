include	<fset.h>

# T_GALACTIC -- convert between equatorial and galactic coordinates.

procedure t_galactic ()

char	fname[SZ_FNAME]
int	filelist, prt_coords, in_coords
bool	streq(), clgetb()
int	clpopni(), clgfil(), btoi(), clgwrd()

begin
	# Input can come from the standard input, a file, or a list of files.
	# The following procedure makes both cases look like a list of files.

	filelist = clpopni ("input")

	# Get output option
	
	in_coords = clgwrd ("in_coords", fname, SZ_FNAME,
	    "|equatorial|galactic|")
	prt_coords = btoi (clgetb ("print_coords"))

	# Process each coordinate list.  If reading from the standard input,
	# set up the standard output to flush after every output line, so that
	# converted coords come back immediately when working interactively.

	while (clgfil (filelist, fname, SZ_FNAME) != EOF) {
	    if (streq (fname, "STDIN"))
		call fseti (STDOUT, F_FLUSHNL, YES)
	    else
		call fseti (STDOUT, F_FLUSHNL, NO)
	    switch (in_coords) {
	    case 1:
		call in_equatorial (STDOUT, fname, prt_coords)
	    case 2:
	        call in_galactic (STDOUT, fname, prt_coords)
	    }
	}

	call clpcls (filelist)
end


# IN_EQUITORIAL -- convert a list of equatorial coordinates read from the
# named file to galactic coordinates, writing the results on the output file.  

procedure in_equatorial (out, listfile, prt_coords)

int	out				# output stream
char	listfile[SZ_FNAME]		# input file
int	prt_coords			# print coordinates in output file?

int	in
double	ra, dec, year, lii, bii
int	fscan(), nscan(), open()
errchk	open, fscan, printf

begin
	in = open (listfile, READ_ONLY, TEXT_FILE)

	# Read successive RA,DEC coordinate pairs from the standard input,
	# converting and printing the result on the standard output.

	while (fscan (in) != EOF) {
	    call gargd (ra)
	    call gargd (dec)
	    call gargd (year)

	    # If there is something wrong with the input coords, print warning
	    # and skip the conversion.  If year is not given with entry
	    # assume 1950.0.

	    switch (nscan()) {
	    case 2:
		year = 1950.0
	    case 3:
	    default:
		call eprintf ("Bad entry in coordinate list\n")
		next
	    }

	    # Call routine to perform conversion and write lII, bII to
	    # the standard output.

	    call ast_galactic (ra, dec, year, lii, bii)

	    if (prt_coords == YES ) {
		call fprintf (out, "%13.2h %12.1h %8.2f")
			call pargd (ra)
			call pargd (dec)
			call pargd (year)
            }
		
	    call fprintf (out, "%13.4f %9.4f" )
		call pargd (lii)
		call pargd (bii)

	    call fprintf (out, "\n")
	}

	call close (in)
end


# IN_GALACTIC -- convert a list of galactic coordinates read from the
# named file to equatorial coordinates, writing the results on the output
# file.  

procedure in_galactic (out, listfile, prt_coords)

int	out				# output stream
char	listfile[SZ_FNAME]		# input file
int	prt_coords			# print coordinates in output file?

int	in
double	ra, dec, year, lii, bii
int	fscan(), nscan(), open()
errchk	open, fscan, printf

begin
	in = open (listfile, READ_ONLY, TEXT_FILE)

	# Read successive RA,DEC coordinate pairs from the standard input,
	# converting and printing the result on the standard output.

	while (fscan (in) != EOF) {
	    call gargd (lii)
	    call gargd (bii)
	    call gargd (year)

	    # If there is something wrong with the input coords, print warning
	    # and skip the conversion.  If year is not given with entry
	    # assume 1950.0.

	    switch (nscan()) {
	    case 2:
		year = 1950.0
	    case 3:
	    default:
		call eprintf ("Bad entry in coordinate list\n")
		next
	    }

	    # Call routine to perform conversion and write lII, bII to
	    # the standard output.

	    call ast_galtoeq (lii, bii, ra, dec, year)

	    if (prt_coords == YES ) {
	        call fprintf (out, "%13.4f %9.4f" )
		    call pargd (lii)
		    call pargd (bii)
            }
		
	    call fprintf (out, "%13.2h %12.1h %8.2f")
		call pargd (ra)
		call pargd (dec)
		call pargd (year)

	    call fprintf (out, "\n")
	}

	call close (in)
end
