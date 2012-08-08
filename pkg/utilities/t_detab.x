# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# DETAB -- Each file matching the file template is copied to the standard
# output, replacing tab characters by equivalent sequences of blanks.
# A special list of tabstops may optionally be specified.

procedure t_detab ()

char	tablist[SZ_LINE], line[SZ_LINE], outline[SZ_LINE], in_fname[SZ_FNAME]
int	list, nchars, in, tabs[SZ_LINE]
int	clpopni(), decode_tablist(), clgfil(), getline()
int	gstrdetab(), open()

begin
	list = clpopni ("files")
	call clgstr ("tablist", tablist, SZ_LINE)
	if (decode_tablist (tablist, tabs, SZ_LINE) == ERR)
	    call error (1, "Unable to decode list of tabs.")

	while (clgfil (list, in_fname, SZ_FNAME) != EOF) {
	    in = open (in_fname, READ_ONLY, TEXT_FILE)
	    while (getline (in, line) != EOF) {
		nchars = gstrdetab (line, outline, SZ_LINE, tabs)
		call putline (STDOUT, outline)
	    }
	    call close (in)
	}

	call clpcls (list)
end
