# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

define	MAX_RANGES	100

# T_BINTXT -- Procedure to convert binary files containing only text to text
# files.

procedure t_bintxt()

bool	verbose
char	outfile[SZ_FNAME]

char	infile[SZ_FNAME], out_fname[SZ_FNAME]
int	list, len_list, in, out, file_number

bool	clgetb()
int	strlen(), open(), clpopni(), clplen(), clgfil()

begin
        # Get input files
	list = clpopni ("binary_file")
	len_list = clplen (list)

	# Get output files
	call clgstr ("text_file", outfile, SZ_FNAME)

	verbose = clgetb ("verbose")

	file_number = 1
	while (clgfil (list, infile, SZ_FNAME) != EOF) {

	    if (len_list > 1) {
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
		}

		# Open input and output files, copy and close files
		in = open (infile, READ_ONLY, BINARY_FILE)
		out = open (out_fname, NEW_FILE, TEXT_FILE)
		call fcopyo (in, out)
		call close (in)
		call close (out)

	    } then {
		if (verbose) {
		    call eprintf ("Cannot read file %s\n")
			call pargstr (infile)
		}
	    } else
		file_number = file_number + 1
	}

	call clpcls (list)
end
