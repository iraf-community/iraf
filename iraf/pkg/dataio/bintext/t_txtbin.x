# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# T_TXTBIN -- Procedure to convert text files to binary files.

procedure t_txtbin ()

bool	verbose
char	outfile[SZ_FNAME]

char	infile[SZ_FNAME], out_fname[SZ_FNAME]
int	list, len_list, in, out, file_number

bool	clgetb()
int	clpopni(), clplen(), clgfil(), strlen(), open()

begin
	# Get list of input files
	list = clpopni ("text_file")
	len_list = clplen (list)

	# Get output file name
	call clgstr ("binary_file", outfile, SZ_FNAME)

	verbose = clgetb ("verbose")

	# Loop over the files
	file_number = 1
	while (clgfil (list, infile, SZ_FNAME) != EOF) {

	    if (len_list > 1 ) {
		call strcpy (outfile, out_fname, SZ_FNAME)
		call sprintf (out_fname[strlen(out_fname) + 1], SZ_FNAME,
				"%03d")
		    call pargi (file_number)
	    } else
	        call strcpy (outfile, out_fname, SZ_FNAME)

	    iferr {

		if (verbose) {
		    call printf ("File: %s -> %s\n")
			call pargstr (infile)
			call pargstr (out_fname)
		    call flush (STDERR)
		}

		# Open input and output files, copy and close input and
		# output files.
		in = open (infile, READ_ONLY, TEXT_FILE)
		out = open (out_fname, NEW_FILE, BINARY_FILE)
		call fcopyo (in, out)
		call close (in)
		call close (out)

	    } then {
		call eprintf ("Cannot read file: %s\n")
		    call pargstr (infile)
	    } else
		file_number = file_number + 1
	}
	call clpcls (list)
end



