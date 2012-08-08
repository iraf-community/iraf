#
#  TXIHC  --   Write basic column info into image header.
#
#
#
#
#  Revision history:
#  ----------------
#
#  26-Nov-96  -  Task created (I.Busko)
#  03-Jan-97  -  Revised after code review (IB)


procedure txihc (im, colnum, colname, colunits, colfmt, lenfmt)

pointer im		# i: pointer to image
int	colnum		# i: column number in input table
char	colname[ARB]	# i: column name
char	colunits[ARB]	# i: column units
char	colfmt[ARB]	# i: column format
int	lenfmt		# i: length of format string
#--
pointer	sp, cu, cf, text

begin
	call smark (sp)
	call salloc (text, SZ_LINE, TY_CHAR)
	call salloc (cu,   SZ_LINE, TY_CHAR)
	call salloc (cf,   SZ_LINE, TY_CHAR)

	# Empty units or format string are encoded as "default".
	if (colunits[1] == EOS)
	    call strcpy ("default", Memc[cu], SZ_LINE)
	else
	    call strcpy (colunits,  Memc[cu], SZ_LINE)
	if (colfmt[1] == EOS)
	    call strcpy ("default", Memc[cf], SZ_LINE)
	else
	    call strcpy (colfmt,   Memc[cf], SZ_LINE)

	# Assemble keyword value.
	call sprintf (Memc[text], SZ_LINE, "%d %s %s %s %d")
	    call pargi (colnum)
	    call pargstr (colname)
	    call pargstr (Memc[cu])
	    call pargstr (Memc[cf])
	    call pargi (lenfmt)

	# Write keyword into header.
	call imastr (im, "COLDATA", Memc[text])
	call sfree (sp)
end

