include	<error.h>


# STS_HELP -- Issue a help line

procedure sts_help (line, nlines, fname, ptr)

int	line		# Line to print
int	nlines		# Number of lines of help
char	fname[ARB]	# Help file
pointer	ptr		# Cache help

int	fd, open(), getline()

begin
	if (ptr == NULL) {
	    iferr (fd = open (fname, READ_ONLY, TEXT_FILE)) {
		call erract (EA_WARN)
		return
	    }
	    nlines = 0
	    call malloc (ptr, SZ_LINE, TY_CHAR)
	    while (getline (fd, Memc[ptr+nlines*SZ_LINE]) != EOF) {
		nlines = nlines + 1
		call realloc (ptr, (nlines+1)*SZ_LINE, TY_CHAR)
	    }
	    call close (fd)
	}

	if (line >= 1  && line <= nlines) {
	    call putline (STDOUT, Memc[ptr+(line-1)*SZ_LINE])
	    call flush (STDOUT)
	}
end
