# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.


# ZZDEBUG.X -- Debug routines for the help formatting system.

task	lroff2html = t_lroff2html,
	lroff2ps   = t_lroff2ps


# LROFF2HTML -- Test program to convert an Lroff source file to HTML.

procedure t_lroff2html ()

int	fdi, fdo
char	iname[SZ_FNAME], oname[SZ_FNAME]

int	open()
errchk	open

begin
	call clgstr ("input",  iname, SZ_FNAME) 	# get parameters
	call clgstr ("output", oname, SZ_FNAME)

	fdi = open (iname, READ_ONLY, TEXT_FILE) 	# open the file
	fdo = open (oname, NEW_FILE, TEXT_FILE)

	# Process it.
	call lroff2html (fdi, fdo, iname, "", "", "", "")

	call close (fdi)
	call close (fdo)
end


# LROFF2PS -- Test program to convert an Lroff source file to Postscript.

procedure t_lroff2ps ()

int	fdi, fdo
char	iname[SZ_FNAME], oname[SZ_FNAME]

pointer	ps
int	open()
errchk	open

begin
	call clgstr ("input",  iname, SZ_FNAME) 	# get parameters
	call clgstr ("output", oname, SZ_FNAME)

	fdi = open (iname, READ_ONLY, TEXT_FILE) 	# open the files
	fdo = open (oname, READ_ONLY, TEXT_FILE)

	# Process it.
	ps = NULL
	call lroff2ps (fdi, fdo, ps, "", "")

	call close (fdi)
	call close (fdo)
end
