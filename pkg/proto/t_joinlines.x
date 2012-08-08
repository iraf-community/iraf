# T_JOINLINES -- Join text files line by line.

procedure t_joinlines ()

int	list			# List of input files
int	out			# Output file descriptor
pointer	delim			# Delimiter string
pointer	missing			# Missing string
int	maxchars		# Maximum characters per line
bool	shortest		# Stop of shortest file?
bool	verbose			# Verbose warnings?

char	c
pointer	sp, fname, fds
int	i, j, in
int	nfiles, nlines, neof, nchars, ntruncate, nlong, ndelim, nmissing
int	fntopnb(), clplen(), clgfil(), clgeti(), open(), strlen()
char	getc()
bool	clgetb()

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (delim, SZ_FNAME, TY_CHAR)
	call salloc (missing, SZ_LINE, TY_CHAR)

	# Task parameters

	# This stuff is provided for backwards compatibility.
	# It would be better to just use an "input" parameter.
	call clgstr ("list1", Memc[fname], SZ_FNAME)
	if (clgeti ("$nargs") == 2) {
	    call clgstr ("list2", Memc[delim], SZ_FNAME)
	    call strcat (",", Memc[fname], SZ_FNAME)
	    call strcat (Memc[delim], Memc[fname], SZ_FNAME)
	}
	list = fntopnb (Memc[fname], NO)

#	list = clpopnu ("input")
	call clgstr ("output", Memc[fname], SZ_FNAME)
	call clgstr ("delim", Memc[delim], SZ_FNAME)
	call clgstr ("missing", Memc[missing], SZ_LINE)
	maxchars = clgeti ("maxchars") - 1
	shortest = clgetb ("shortest")
	verbose = clgetb ("verbose")

	# Open files.  Quit on an error.
	out = open (Memc[fname], APPEND, TEXT_FILE)
	nfiles = clplen (list)
	call malloc (fds, nfiles, TY_INT)
	do i = 1, nfiles {
	    j = clgfil (list, Memc[fname], SZ_FNAME)
	    Memi[fds+i-1] = open (Memc[fname], READ_ONLY, TEXT_FILE)
	}
	call clpcls (list)

	# Join the input lines.  First read a character from each file
	# to determine if we are at the EOF and take appropriate action
	# if one or more EOFs are found.

	ndelim = strlen (Memc[delim])
	nmissing = strlen (Memc[missing])
	ntruncate = 0
	nlong = 0
	for (nlines = 1; ; nlines = nlines + 1) {
	    nchars = 0
	    neof = 0
	    do i = 1, nfiles {
	        in = Memi[fds+i-1]
		if (getc (in, c) == EOF)
		    neof = neof + 1
		else
		    call ungetc (in, c)
	    }
	    if (neof == nfiles || (shortest && neof > 0))
		break

	    do i = 1, nfiles {
	        in = Memi[fds+i-1]
	        repeat {
	            if (getc (in, c) == EOF) {
		        do j = 1, nmissing {
		            if (nchars < maxchars)
		                call putc (out, Memc[missing+j-1])
	    	            nchars = nchars + 1
		        }
			break
		    } else if (c == '\n')
		        break
		    if (nchars < maxchars)
		        call putc (out, c)
		    nchars = nchars + 1
	        }

		# Add the delimiter and new line.  Count the delimiter also.
	        if (i < nfiles) {
		    do j = 1, ndelim {
		        if (nchars < maxchars)
		            call putc (out, Memc[delim+j-1])
	    	        nchars = nchars + 1
		    }
	        } else {
		    call fprintf (out, "\n")
		    break
	        }
	    }

	    # Accumulate warnings about line lengths.
	    if (nchars > maxchars)
		ntruncate = ntruncate + 1
	    if (min (nchars, maxchars + 1) > SZ_LINE)
		nlong = nlong + 1
	}

	# Finish up.
	if (verbose) {
	    if (ntruncate > 0) {
	        call eprintf ("WARNING: %d lines truncated at %d characters\n")
		    call pargi (ntruncate)
		    call pargi (maxchars + 1)
	    }
	    if (nlong > 0) {
	        call eprintf (
		    "WARNING: %d lines exceed IRAF limit of %d characters\n")
		    call pargi (nlong)
		    call pargi (SZ_LINE)
	    }
	    if (neof < nfiles) {
	        call eprintf ("WARNING: %d/%d files completed\n")
		    call pargi (neof)
		    call pargi (nfiles)
	    }
	}
		
	call close (out)
	do i = 1, nfiles
	    call close (Memi[fds+i-1])
	call sfree (sp)
end
