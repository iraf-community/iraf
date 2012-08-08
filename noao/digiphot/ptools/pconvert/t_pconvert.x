include	<fset.h>

# T_PCONVERT -- Procedure to convert the records within a text file into
# an STSDAS Table.

procedure t_pconvert ()

pointer	text				# pointer to name of text file
pointer	fields				# pointer list of fields
pointer	table				# pointer to STSDAS table
pointer	expr				# Pointer to boolean expression
int	append				# open file in append mode

int	fd, td
pointer	sp
bool	clgetb()
int	open(), btoi(), fstati(), tbtopn()

begin

	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate working space.
	call smark (sp)
	call salloc (text, SZ_LINE, TY_CHAR)
	call salloc (fields, SZ_LINE, TY_CHAR)
	call salloc (table, SZ_LINE, TY_CHAR)
	call salloc (expr, SZ_LINE, TY_CHAR)

	# Get the parameters.
	call clgstr ("textfile", Memc[text], SZ_LINE)
	call clgstr ("table", Memc[table], SZ_LINE)
	call clgstr ("fields", Memc[fields], SZ_LINE)
	call strupr (Memc[fields])
	call clgstr ("expr", Memc[expr], SZ_LINE)
	append = btoi (clgetb ("append"))

	# Open the table.
	fd = open (Memc[text], READ_ONLY, TEXT_FILE)
	if (append == YES)
	    td = tbtopn (Memc[table], READ_WRITE, 0)
	else
	    td = tbtopn (Memc[table], NEW_FILE, 0)

	# Select records.
	call pt_convert (fd, td, Memc[fields], Memc[expr], append)

	# Close up.
	call close (fd)
	call tbtclo (td)
	call sfree (sp)
end
