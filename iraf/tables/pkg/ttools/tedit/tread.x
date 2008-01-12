# T_TREAD -- Read only table editor

procedure t_tread ()

#--
pointer	table		# SDAS table name
pointer	columns		# list of columns to edit
bool	silent		# don't ring bell when error occurs

bool	rdonly,inplace
pointer	sp

bool	clgetb()

begin
	call smark (sp)
	call salloc (table, SZ_FNAME, TY_CHAR)
	call salloc (columns, SZ_FNAME, TY_CHAR)

	call clgstr ("table", Memc[table], SZ_FNAME)
	call clgstr ("columns", Memc[columns], SZ_FNAME)

	silent = clgetb ("silent")
	rdonly = true
	inplace = true

	call edit (Memc[table], Memc[columns], silent, rdonly, inplace)
	call sfree (sp)

end

