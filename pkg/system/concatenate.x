# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CONCATENATE -- Concatenate the named input files (or the standard input) to
# produce the output file.  The input files must all be of the same type.
# The type of the output file (text|binary) defaults to the type of the first
# input file, or to text if reading from the standard input.  The hidden
# parameter "out_type" may be used to force the file type of the copy operation.

procedure t_concatenate()

char	infile[SZ_FNAME], outfile[SZ_FNAME]
char	out_type_string[SZ_FNAME]
int	in, out, list, out_type

int	clpopni(), clgfil()
int	open(), access(), strmatch(), clgeti()
bool	clgetb()

begin
	# Open the input file list, and read in the name of the first file
	# (used to determine type of output file later).

	list = clpopni ("input_files")
	if (clgfil (list, infile, SZ_FNAME) == EOF) {
	    call clpcls (list)
	    return
	}

	# Determine whether the output file is text or binary, and open
	# the file.

	call clgstr ("out_type", out_type_string, SZ_FNAME)
	out_type = NULL
	if (strmatch (out_type_string, "^#{b}") > 0)
	    out_type = BINARY_FILE
	else if (strmatch (out_type_string, "^#{t}") > 0)
	    out_type = TEXT_FILE

	# Default to std output if output file not given on cmd line.

	if (clgeti ("$nargs") <= 1)
	    call strcpy ("STDOUT", outfile, SZ_FNAME)
	else
	    call clgstr ("output_file", outfile, SZ_FNAME)

	# If the output file type has been specified (param "out_type"),
	# use it, otherwise use type of first input file.

	if (out_type == NULL) {
	    if (strmatch (infile, "STDIN") > 0)
		out_type = TEXT_FILE
	    else if (access (infile, 0, TEXT_FILE) == YES)
		out_type = TEXT_FILE
	    else
		out_type = BINARY_FILE
	}

	# Finally, open the output file.
	if (clgetb ("append"))
	    out = open (outfile, APPEND, out_type)
	else
	    out = open (outfile, NEW_FILE, out_type)

	# Append the input files to the output file.
	repeat {
	    in = open (infile, READ_ONLY, out_type)
	    call fcopyo (in, out)
	    call close (in)
	} until (clgfil (list, infile, SZ_FNAME) == EOF)

	call close (out)
	call clpcls (list)
end
