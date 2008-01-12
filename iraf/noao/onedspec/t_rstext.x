# T_RSTEXT -- This procedure replaces the following CL script code to
# make the RSPECTEXT script efficient.  It also determines whether there
# is a header rather than requiring the user to specify it.
#
#	# Separate the header and flux values for RTEXTIMAGE and the
#	# wavelengths for later use.
#
#	fd = specin
#	if (header) {
#	    while (fscan (fd, line) != EOF) {
#		print (line, >> temp1)
#		if (substr (line,1,3) == "END")
#		    break
#	    }
#	}
#	dim = 0
#	while (fscan (fd, x, y) != EOF) {
#	    if (nscan() == 2) {
#		print (y, >> temp1)
#		print (x, >> temp2)
#		dim = dim + 1
#	    }
#	}

procedure t_rstext ()

pointer	input		# Input RSPECTEXT text file
pointer	output1		# Output text file for RTEXTIMAGE
pointer	output2		# Output text file for DISPCOR

int	in, out1, out2, dim
bool	header
real	x, y
pointer	sp, line
int	open(), getline(), strncmp(), fscan(), nscan()
errchk	open

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output1, SZ_FNAME, TY_CHAR)
	call salloc (output2, SZ_FNAME, TY_CHAR)
	call salloc (line, SZ_LINE, TY_CHAR)

	call clgstr ("input", Memc[input], SZ_FNAME)
	call clgstr ("output1", Memc[output1], SZ_FNAME)
	call clgstr ("output2", Memc[output2], SZ_FNAME)

	in = open (Memc[input], READ_ONLY, TEXT_FILE)
	out1 = open (Memc[output1], NEW_FILE, TEXT_FILE)
	out2 = open (Memc[output2], NEW_FILE, TEXT_FILE)

	header = false
	while (getline (in, Memc[line]) != EOF) {
	    if (strncmp (Memc[line], "END", 3) == 0) {
		header = true
		break
	    }
	}
	call seek (in, BOF)

	if (header) {
	    while (getline (in, Memc[line]) != EOF) {
		call putline (out1, Memc[line])
		if (strncmp (Memc[line], "END", 3) == 0)
		    break
	    }
	}

	dim = 0
	while (fscan (in) != EOF) {
	    call gargr (x)
	    call gargr (y)
	    if (nscan() != 2)
		next
	    call fprintf (out1, "%g\n")
		call pargr (y)
	    call fprintf (out2, "%g\n")
		call pargr (x)
	    dim = dim + 1
	}

	call printf ("%b %d\n")
	    call pargb (header)
	    call pargi (dim)

	call close (out2)
	call close (out1)
	call close (in)
	call sfree (sp)
end
