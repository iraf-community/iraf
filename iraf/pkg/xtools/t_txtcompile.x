include	<fset.h>

task	txtcompile = t_txtcompile


# T_TXTCOMPILE -- Compile a text file into an SPP routine.

procedure t_txtcompile ()

char	input[SZ_FNAME]
char	output[SZ_FNAME]
char	procname[SZ_FNAME]

char	line[SZ_LINE]
int	i, in, out, fsize

int	open(), fstati(), fscan()
errchk	open, stropen

begin
	call clgstr ("input", input, SZ_FNAME)
	call clgstr ("output", output, SZ_FNAME)
	call clgstr ("procname", procname, SZ_FNAME)

	# Open files.
	in = open (input, READ_ONLY, TEXT_FILE)
	out = open (output, APPEND, TEXT_FILE)

	# Get input file size.
	fsize = 2 * fstati (in, F_FILESIZE)

	# Write preamble.
	call fprintf (out, "\n\nprocedure %s (xqzrkc)\n\n")
	    call pargstr (procname)
	call fprintf (out, "pointer\txqzrkc\n\n")
	call fprintf (out, "int\tfd, stropen()\n")
	call fprintf (out, "errchk\tmalloc\n\nbegin\n")
	call fprintf (out, "\tcall malloc (xqzrkc, %d, TY_CHAR)\n")
	    call pargi (fsize)
	call fprintf (out, "\tfd = stropen (Memc[xqzrkc], ARB, NEW_FILE)\n")

	# Write text.
	while (fscan (in) != EOF) {
	    call gargstr (line, SZ_LINE)
	    call fprintf (out, "\tcall fprintf (fd, """)
	    for (i=1; line[i]!=EOS; i=i+1) {
		switch (line[i]) {
		case '"', '%':
		    call putc (out, line[i])
		}
		call putc (out, line[i])
	    }
	    call fprintf (out, "\\\\n"")\n")
	}

	# Write postamble.
	call fprintf (out, "\tcall close (fd)\nend\n")

	# Close the files.
	call close (out)
	call close (in)
end
