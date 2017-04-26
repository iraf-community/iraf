# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# LROFF -- Text process a file.

procedure t_lroff()

char	fname[SZ_FNAME]
char	line[SZ_LINE]
char	format[SZ_FNAME]
int	fd, lmargin, rmargin
int	open(), getline(), strmatch(), clgeti()
extern	getline(), putline()

begin
	call clgstr ("input_file", fname, SZ_FNAME)
	fd = open (fname, READ_ONLY, TEXT_FILE)

	lmargin = clgeti ("lmargin")
	rmargin = clgeti ("rmargin")
	call clgstr ("format", format, SZ_FNAME)

	while (getline (fd, line) != EOF) {
	    if (strmatch (line, "^.help") > 0) {
		if (format[1] == 't')
		    call lroff (getline, fd, putline, STDOUT, lmargin, rmargin,
		        YES, NO)
		else if (format[1] == 'h')
		    call lroff2html (fd, STDOUT, fname, "", "", "", "")
		else if (format[1] == 'p')
		    call lroff2ps (fd, STDOUT, NULL, "", "")
	    }
	}
	
	call close (fd)
end
