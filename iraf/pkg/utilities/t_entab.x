# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ENTAB -- Given a list of files and a list of tabstops, replace sequences
# of blanks in the files by equivalent sequences of tab characters and blanks.

procedure t_entab()

char	tablist[SZ_LINE], line[SZ_LINE], outline[SZ_LINE]
char	in_fname[SZ_FNAME]
int	list, in, nchars

int	clpopni(), decode_tablist(), tabs[SZ_LINE], clgfil(), getline()
int	gstrentab(), open()

begin
	list = clpopni ("files")
	call clgstr ("tablist", tablist, SZ_LINE)
	if (decode_tablist (tablist, tabs, SZ_LINE) == ERR)
	    call error (1, "Unable to decode list of tabs.")

	while (clgfil (list, in_fname, SZ_FNAME) != EOF) {
	    in = open (in_fname, READ_ONLY, TEXT_FILE)
	    while (getline (in, line) != EOF) {
		nchars = gstrentab (line, outline, SZ_LINE, tabs)
		call putline (STDOUT, outline)
	    }
	    call close (in)
	}

	call clpcls (list)
end
