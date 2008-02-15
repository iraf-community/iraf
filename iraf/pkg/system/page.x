# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fset.h>

# PAGE -- Display a text file or files on the standard output (the user
# terminal) one screen at a time, pausing after each screen has been filled.
# The program is keystroke driven in raw mode, and currently recognizes the
# keystrokes defined above.

procedure t_page()

bool	redirin
pointer	sp, device, prompt, files
int	map_cc, clear_screen, first_page

bool	clgetb()
int	fstati(), clgeti(), btoi()

begin
	call smark (sp)
	call salloc (device, SZ_FNAME, TY_CHAR)
	call salloc (prompt, SZ_FNAME, TY_CHAR)
	call salloc (files, SZ_LINE, TY_CHAR)

	redirin = (fstati (STDIN, F_REDIR) == YES)
	if (redirin)
	    call strcpy ("STDIN", Memc[files], SZ_LINE)
	else
	    call clgstr ("files", Memc[files], SZ_LINE)

	map_cc = btoi (clgetb ("map_cc"))
	clear_screen = btoi (clgetb ("clear_screen"))
	first_page = clgeti ("first_page")
	call clgstr ("prompt", Memc[prompt], SZ_FNAME)
	call clgstr ("device", Memc[device], SZ_FNAME)

	call xpagefiles (Memc[files], Memc[device], Memc[prompt],
	    first_page, clear_screen, map_cc)

	call sfree (sp)
end
