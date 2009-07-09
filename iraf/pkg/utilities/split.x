# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# SPLIT -- Split a large file up into more manageable pieces.  If the operand
# is a text file, split the file into segments of at most NLINES lines, else
# if the file is a binary file, split it into segments of at most NBYTES bytes.
# Only a single input file can be processed.

procedure t_split()

bool	verbose
pointer	sp, p_input, output, fname, buf
int	maxfiles, nbytes, file_type, fileno, in, out
long	offset, n, nlines, nrecords
size_t	nchars
size_t	sz_val

bool	clgetb()
int	open(), getline(), access(), clgeti()
long	read(), clgetl()

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (p_input, sz_val, TY_CHAR)
	call salloc (output, sz_val, TY_CHAR)
	call salloc (fname, sz_val, TY_CHAR)

	# Get the input file name and the root name for the output files.
	call clgstr ("input", Memc[p_input], SZ_FNAME)
	call clgstr ("output", Memc[output], SZ_FNAME)

	# Determine the file type of the input file.
	if (access (Memc[p_input], READ_ONLY, TEXT_FILE) == YES)
	    file_type = TEXT_FILE
	else
	    file_type = BINARY_FILE

	# Determine the segment size and allocate the data buffer.
	if (file_type == TEXT_FILE) {
	    nlines = clgetl ("nlines")
	    sz_val = SZ_LINE
	    call salloc (buf, sz_val, TY_CHAR)
	} else {
	    nbytes = clgeti ("nbytes")
	    nchars = (nbytes + SZB_CHAR-1) / SZB_CHAR
	    sz_val = nchars
	    call salloc (buf, sz_val, TY_CHAR)
	}

	maxfiles = clgeti ("maxfiles")
	verbose  = clgetb ("verbose")

	# Split the input file.
	in = open (Memc[p_input], READ_ONLY, file_type)
	offset = 1

	for (fileno=1;  fileno <= maxfiles;  fileno=fileno+1) {
	    # Open the next segment file.
	    call sprintf (Memc[fname], SZ_FNAME, "%s.%0*d")
		call pargstr (Memc[output])
		call pargi (int (log10(real(maxfiles))) + 1)
		call pargi (fileno)

	    if (verbose) {
		call printf ("%s: ")
		    call pargstr (Memc[fname])
		call flush (STDOUT)
	    }
	    out = open (Memc[fname], NEW_FILE, file_type)

	    # Copy the segment.
	    if (file_type == BINARY_FILE) {
		n = read (in, Memc[buf], nchars)
		if (n != EOF) {
		    sz_val = n
		    call write (out, Memc[buf], sz_val)
		    nrecords = n
		} else
		    nrecords = 0
	    } else {
		for (nrecords=0;  nrecords < nlines;  nrecords=nrecords+1)
		    if (getline (in, Memc[buf]) == EOF)
			break
		    else
			call putline (out, Memc[buf])
	    }

	    # Close the file; delete it and exit if no data was written.
	    call close (out)
	    if (nrecords <= 0) {
		if (verbose)
		    call printf ("[deleted; exit]\n")
		call delete (Memc[fname])
		break
	    } else if (verbose) {
		call printf ("%d %s @ %d\n")
		    if (file_type == TEXT_FILE) {
			call pargl (nrecords)
			call pargstr ("lines")
		    } else {
			call pargl (nrecords * SZB_CHAR)
			call pargstr ("bytes")
		    }
		    call pargl (offset)
	    }

	    offset = offset + nrecords
	    call flush (STDOUT)
	}

	call sfree (sp)
end
