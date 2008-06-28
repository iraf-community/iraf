# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# LROFF -- Text process a file.

procedure t_lroff()

char	fname[SZ_FNAME]
char	line[SZ_LINE]
char	format[SZ_FNAME]
int	fd, lmargin, rmargin
pointer	fd_p0, fd_p1
int	open(), getline(), getline_p(), strmatch(), clgeti()
extern	getline_p(), putline_p()

include <nullptr.inc>

begin
	call clgstr ("input_file", fname, SZ_FNAME)
	fd = open (fname, READ_ONLY, TEXT_FILE)

	lmargin = clgeti ("lmargin")
	rmargin = clgeti ("rmargin")
	call clgstr ("format", format, SZ_FNAME)

	while (getline (fd, line) != EOF) {
	    if (strmatch (line, "^.help") > 0) {
		if (format[1] == 't') {
		    fd_p0 = fd
		    fd_p1 = STDOUT
		    call lroff (getline_p, fd_p0, putline_p, fd_p1, lmargin, rmargin,
		        YES, NO)
		} else if (format[1] == 'h') {
		    call lroff2html (fd, STDOUT, fname, "", "", "", "")
		} else if (format[1] == 'p') {
		    call lroff2ps (fd, STDOUT, NULLPTR, "", "")
		}
	    }
	}
	
	call close (fd)
end

# wrappers

int procedure getline_p (fd_p, linebuf)

pointer	fd_p				# input file
char	linebuf[ARB]			# output line buffer (>= SZ_LINE)

int	fd
int	getline()

begin
	fd = fd_p
	return getline(fd,linebuf)
end

procedure putline_p (fd_p, linebuf)

int	fd_p				# output file
char	linebuf[ARB]			# line to be output

int	fd

begin
	fd = fd_p
	call putline(fd,linebuf)
end
