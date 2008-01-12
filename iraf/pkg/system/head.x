# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>

# HEAD -- Print the head (first few lines) of each of the named text files on
# the standard output.  If more than one file is to be printed, a brief header
# is printed for each file.
# 
# Params:
# 	input_files		file matching template
# 	nlines [h,8]		number of lines to be printed

procedure t_head()

char	fname[SZ_FNAME]
bool	multiple_files
int	nlines, list
int	clpopni(), clplen(), clgfil(), clgeti()

begin
	list = clpopni ("input_files")
	multiple_files = (clplen (list) > 1)
	nlines = clgeti ("nlines")

	while (clgfil (list, fname, SZ_FNAME) != EOF)
	    iferr (call print_head (STDOUT, fname, nlines, multiple_files))
		call erract (EA_WARN)

	call clpcls (list)
end


# PRINT_HEAD -- Print the first few lines of a file on the output stream given
# as the first argument, optionally plus a header.

procedure print_head (out, fname, nlines, print_file_name)

int	out, nlines
bool	print_file_name
char	fname[SZ_FNAME], lbuf[SZ_LINE]
int	in, line, open(), getline()
errchk	open, getline, putline

begin
	in = open (fname, READ_ONLY, TEXT_FILE)

	if (print_file_name) {
	    call fprintf (out, "\n\n===> %s <===\n")
		call pargstr (fname)
	}

	for (line=1;  line <= nlines && getline(in,lbuf) != EOF;  line=line+1)
	    call putline (out, lbuf)

	call close (in)
end
