# T_APSELECT -- Procedure to perform a relational select operation upon a set of
# records within a text file. Our function is to select all records from the
# input file matching some criterion, printing the listed fields on the
# standard output.

procedure t_apselect ()

int	list, fd, format, savepars
pointer	sp, textfile, fields, expr

bool	clgetb()
int	clpopnu(), clgfil(), open(), btoi()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (textfile, SZ_FNAME, TY_CHAR)
	call salloc (fields, SZ_LINE, TY_CHAR)
	call salloc (expr, SZ_LINE, TY_CHAR)

	# Get the parameters.
	list = clpopnu ("textfile")
	call clgstr ("fields", Memc[fields], SZ_LINE)
	call strupr (Memc[fields], Memc[fields], SZ_LINE)
	call clgstr ("expr", Memc[expr], SZ_LINE)
	savepars = btoi (clgetb ("savepars"))
	format = btoi (clgetb ("format"))

	# Select records.
	while (clgfil (list, Memc[textfile], SZ_FNAME) != EOF) {
	    fd = open (Memc[textfile], READ_ONLY, TEXT_FILE)
	    if (Memc[fields] != EOS)
	        call ap_select (fd, Memc[fields], Memc[expr], savepars, format)
	    call close (fd)
	    call printf ("\n")
	}

	call clpcls (list)
	call sfree (sp)
end
