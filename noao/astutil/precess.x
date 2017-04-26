include	<fset.h>

# PRECESS -- Precess a list of coordinates read from the standard input
# from startyear to endyear.

procedure t_precess()

char	fname[SZ_FNAME]
int	filelist
double	default_year1, default_year2, stdepoch
bool	streq()
double	clgetd()
int	clpopni(), clgfil()

begin
	# Input can come from the standard input, a file, or a list of files.
	# The following procedure makes both cases look like a list of files.

	filelist = clpopni ("input")

	# Fetch default startyear and endyear parameters from the CL.
	default_year1 = clgetd ("startyear")
	default_year2 = clgetd ("endyear")

	# Output coords for a standard epoch as well as for year2?
	stdepoch = clgetd ("stdepoch")

	# Process each coordinate list.  If reading from the standard input,
	# set up the standard output to flush after every output line, so that
	# precessed coords come back immediately when working interactively.

	while (clgfil (filelist, fname, SZ_FNAME) != EOF) {
	    if (streq (fname, "STDIN"))
		call fseti (STDOUT, F_FLUSHNL, YES)
	    else
		call fseti (STDOUT, F_FLUSHNL, NO)
	    call precess_coordinate_list (STDOUT, fname,
		default_year1, default_year2, stdepoch)
	}

	call clpcls (filelist)
end


# PRECESS_COORDINATE_LIST -- Precess a list of coordinates read from the
# named file, writing the results on the output file.  If stdepoch is not
# zero, print the coords precessed to the given standard epoch as well.

procedure precess_coordinate_list (out, listfile, defyear1, defyear2, stdepoch)

int	out					# output stream
char	listfile[SZ_FNAME]			# input file
double	defyear1, defyear2			# default years
double	stdepoch				# standard epoch

int	in
double	year1, year2
double	ra1, dec1, ra2, dec2
int	fscan(), nscan(), open()
errchk	open, fscan, printf

begin
	in = open (listfile, READ_ONLY, TEXT_FILE)

	# Read successive RA,DEC coordinate pairs from the standard input,
	# precessing and printing the result on the standard output.

	while (fscan (in) != EOF) {
	    call gargd (ra1)
	    call gargd (dec1)
	    call gargd (year1)
	    call gargd (year2)

	    # If there is something wrong with the input coords, print warning
	    # and skip the precession.  If years are not given with entry,
	    # use defaults (nscan returns the number of items successfully
	    # decoded from the input line)

	    if (nscan() < 2) {
		call eprintf ("Bad entry in coordinate list\n")
		next
	    } else if (nscan() == 2) {
		year1 = defyear1
		year2 = defyear2
	    } else if (nscan() == 3)
		year2 = defyear2

	    # Call precession routine to perform the precession, and write
	    # precessed coords to the standard output.

	    call ast_precess (ra1, dec1, year1, ra2, dec2, year2)
	    call fprintf (out, "%13.2h %13.2h %7.1f")
		call pargd (ra2)
		call pargd (dec2)
		call pargd (year2)

	    # Add output for the input coords precessed to the standard
	    # epoch, if desired.

	    if (stdepoch != 0) {
		call ast_precess (ra1, dec1, year1, ra2, dec2, stdepoch)
		call fprintf (out, "%18.2h %12.2h %7.1f")
		    call pargd (ra2)
		    call pargd (dec2)
		    call pargd (stdepoch)
	    }

	    call fprintf (out, "\n")
	}

	call close (in)
end
