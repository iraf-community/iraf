# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>

# TYPE -- Type the named files on the standard output.  If more than one
# file, print a brief header identifying each file.

procedure t_type()

char	fname[SZ_FNAME], device[SZ_FNAME]
bool	multiple_files
int	map_cc, list
pointer	tty, ttyodes()
bool	clgetb()
int	clpopni(), clplen(), clgfil(), btoi()

begin
	list = clpopni ("input_files")
	multiple_files = (clplen (list) > 1)
	map_cc = btoi (clgetb ("map_cc"))
	call clgstr ("device", device, SZ_FNAME)
	tty = ttyodes (device)

	while (clgfil (list, fname, SZ_FNAME) != EOF)
	    iferr (call type_file (STDOUT, tty, fname, multiple_files, map_cc))
		call erract (EA_WARN)

	call ttycdes (tty)
	call clpcls (list)
end


# TYPE_FILE -- Print the named file on the output stream given as the first
# argument, optionally with a leading header.

procedure type_file (out, tty, fname, print_file_name, map_cc)

int	out
pointer	tty
char	fname[ARB]
bool	print_file_name
int	map_cc

int	in
pointer	sp, buf
int	open(), getline()
errchk	salloc, open, getline, ttyputline

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	in = open (fname, READ_ONLY, TEXT_FILE)

	if (print_file_name) {
	    call fprintf (out, "===> %s <===\n")
		call pargstr (fname)
	}

	while (getline (in, Memc[buf]) != EOF)
	    call ttyputline (out, tty, Memc[buf], map_cc)

	if (print_file_name)
	    call fprintf (out, "\n\n")

	call close (in)
	call sfree (sp)
end
