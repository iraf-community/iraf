include	<tbset.h>

#  TINEW  --  Opens and creates a new output table.
#
#
#
#  Revision history:
#  ----------------
#  20-Jan-1997  -  Task created (I.Busko)
#   8-Apr-1999  -  Call tbfpri (Phil Hodge)
#   8-Apr-2002  -  Remove the call to whatfile (P. Hodge)
#   8-Dec-2003  -  Call tcs_close for cpo.


procedure tinew (template, list, output, rowsel, colsel, colname, colunits, 
                 colfmt, otp, cpo, ncpo)

char	template[ARB]	# i: template table name
pointer	list		# i: input list
char	output[ARB]	# i: output table name
char	rowsel[ARB]	# i: work array for row selectors
char	colsel[ARB]	# i: work array for column selectors
char	colname[ARB]	# i: work array for column names
char	colunits[ARB]	# i: work array for column units
char	colfmt[ARB]	# i: work array for column format
pointer	otp		# o: table descriptor
pointer	cpo		# o: column descriptor
int	ncpo		# o: number of columns
#--
pointer	sp, itp, newcpo, root
int	nrows, ncols, nscalar
int	phu_copied	# set by tbfpri and ignored
bool	is_temp

errchk	tbfpri, tbtopn, tisetc

pointer	tbtopn()
int	tbpsta(), imtgetim(), tihnsc(), access()

begin
	call smark (sp)
	call salloc (root,  SZ_PATHNAME, TY_CHAR)

	# See if there is a template table.
	is_temp = true
	if (access (template, READ_ONLY, 0) == NO) {

	    # Get first table in input list as the template.
	    if (imtgetim (list, template, SZ_PATHNAME) == EOF)
	        call error (1, "Input list is empty.")
	    call imtrew (list)
	    is_temp = false
	}

	# Break template file name into bracketed selectors.
	call rdselect (template, Memc[root], rowsel, colsel, SZ_FNAME)

	# Open template table and get some info.
	itp   = tbtopn (Memc[root], READ_ONLY, NULL)
	nrows = tbpsta (itp, TBL_NROWS)
	ncols = tbpsta (itp, TBL_NCOLS)

	# There might be header-stored scalars that don't show up
	# with tbpsta, if the template is coming from the input list.
	# Examine the header to find how many of them there are and 
	# increment number of output columns.
	nscalar = tihnsc (itp)
	ncols = ncols + nscalar

	# Create arrays with colum info. Must be freed by caller.
	call malloc (cpo,    ncols, TY_INT)
	call malloc (newcpo, ncols, TY_INT)
	call tcs_open (itp, colsel, Memi[cpo], ncpo, ncols)

	# Exit if no column matches and no scalars.
	if (ncpo == 0 && nscalar == 0)
	        call error (1, "No columns selected.")

	# Open output table.
	call tbfpri (Memc[root], output, phu_copied)
	otp = tbtopn (output, NEW_FILE, 0)

	# Copy column information from input to output.
	call tisetc (cpo, newcpo, ncpo, nscalar, itp, otp, colname, colunits, 
                     colfmt, nrows, is_temp)

	# Point to new column array.
	call tcs_close (Memi[cpo], ncpo)
	call mfree (cpo, TY_INT)
	cpo = newcpo

	# Number of columns now is (selected columns from input) + scalars.
	ncpo = ncpo + nscalar

	# Create output table.
	call tbtcre (otp)

	# Cleanup.
	call tbtclo (itp)
	call sfree (sp)
end
