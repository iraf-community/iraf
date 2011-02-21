#
#  TXTHC  --   Write basic column info into header.
#
#
#
#
#  Revision history:
#  ----------------
#
#  25-Nov-96  -  Task created (I.Busko)
#  03-Jan-97  -  Revised after code review (IB)


procedure txthc (otp, colnum, colname, colunits, colfmt, 
                 datatype, lenfmt)

pointer otp		# i: pointer to descriptor of output table
int	colnum		# i: column number in input table
char	colname[ARB]	# i: column name
char	colunits[ARB]	# i: column units
char	colfmt[ARB]	# i: column format
int	datatype	# i: data type
int	lenfmt		# i: length of format string
#--
pointer	sp, cu, cf, keyword, text, dtype
int	lenstr

begin
	call smark (sp)
	call salloc (keyword,  SZ_LINE, TY_CHAR)
	call salloc (text,     SZ_LINE, TY_CHAR)
	call salloc (dtype,    SZ_LINE, TY_CHAR)
	call salloc (cu,       SZ_LINE, TY_CHAR)
	call salloc (cf,       SZ_LINE, TY_CHAR)

	# Use original column number to build keyword name.
	call sprintf (Memc[keyword], SZ_LINE, "TCD_%03d")
	    call pargi (colnum)

	# Data type is encoded as a human-readable character string.
	if (datatype < 0) {
	    lenstr   = -datatype
	    datatype = TY_CHAR
	}
	switch (datatype) {
	    case TY_BOOL:
	        call strcpy ("boolean", Memc[dtype], SZ_LINE)
	    case TY_SHORT:
	        call strcpy ("short",   Memc[dtype], SZ_LINE)
	    case TY_INT: 
	        call strcpy ("integer", Memc[dtype], SZ_LINE)
	    case TY_LONG: 
	        call strcpy ("long",    Memc[dtype], SZ_LINE)
	    case TY_REAL:
	        call strcpy ("real",    Memc[dtype], SZ_LINE)
	    case TY_DOUBLE:
	        call strcpy ("double",  Memc[dtype], SZ_LINE)
	    case TY_CHAR:
	        call sprintf (Memc[dtype], SZ_LINE, "character_%d")
	        call pargi (lenstr)
	}

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
	call sprintf (Memc[text], SZ_LINE, "%s %s %s %s %d")
	    call pargstr (colname)
	    call pargstr (Memc[cu])
	    call pargstr (Memc[cf])
	    call pargstr (Memc[dtype])
	    call pargi (lenfmt)

	# Write keyword into header.
	call tbhadt (otp, Memc[keyword], Memc[text])
	call sfree (sp)
end

