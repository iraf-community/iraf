# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# LROFF -- Text process a file.

procedure t_lroff()

char	fname[SZ_FNAME]
char	line[SZ_LINE]
int	fd, lmargin, rmargin
int	open(), getline(), strmatch(), clgeti()
extern	getline(), putline()

begin
	call clgstr ("input_file", fname, SZ_FNAME)
	fd = open (fname, READ_ONLY, TEXT_FILE)

	lmargin = clgeti ("lmargin")
	rmargin = clgeti ("rmargin")

	while (getline (fd, line) != EOF)
	    if (strmatch (line, "^.help") > 0)
		call lroff (getline, fd, putline, STDOUT, lmargin, rmargin,
		    YES, NO)
	
	call close (fd)
end
