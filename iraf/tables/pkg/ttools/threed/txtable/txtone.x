include	<tbset.h>

#  TXTONE  --  Extract 2D tables from a single input 3D table.
#
#
#  This code is adapted from B.Simon's 04-Nov-94 version of tcopy.
#
#
#
#  Revision history:
#  ----------------
#
#  22-Nov-1996  -  Task created (I.Busko)
#  16-Dec-1996  -  Add ORIG_ROW keyword (IB).
#  03-Jan-1997  -  Revised after code review (IB)
#  17-Mar-1997  -  Added selrows call (IB)
#   8-Apr-1999  -  Call tbfpri (Phil Hodge)
#   8-Apr-2002  -  Remove the call to whatfile (P. Hodge)


procedure txtone (input, output, verbose, compact)

char	input[ARB]	# i: input table name
char	output[ARB]	# i: output table name
bool	compact		# i: put scalars in header ?
bool	verbose		# i: print operations ?
#--
int     numrow, numcol, numptr, type, irow, nrows
int	phu_copied	# set by tbfpri and ignored
pointer	sp, root, extend, rowselect, colselect, colname, colunits, colfmt
pointer errmsg, itp, otp, colptr, newcol, pcode 
pointer	newname
bool	suffix

string	nosect  "Sections not permitted on output table name (%s)"
string	nocols  "Column names not found (%s)"

errchk	tbfpri, tbtopn, tctexp, tbracket, trsopen, trseval

bool	trseval(), streq()
int	tbpsta(), selrows()
pointer	tbtopn(), trsopen()

begin
	# Allocate memory for temporary strings.
	call smark (sp)
	call salloc (root,      SZ_FNAME,    TY_CHAR)
	call salloc (newname,   SZ_FNAME,    TY_CHAR)
	call salloc (extend,    SZ_FNAME,    TY_CHAR)
	call salloc (rowselect, SZ_FNAME,    TY_CHAR)
	call salloc (colselect, SZ_FNAME,    TY_CHAR)
        call salloc (colname,   SZ_COLNAME,  TY_CHAR)
        call salloc (colunits,  SZ_COLUNITS, TY_CHAR)
        call salloc (colfmt,    SZ_COLFMT,   TY_CHAR)
	call salloc (errmsg,    SZ_LINE,     TY_CHAR)

	# Selectors are forbbiden on output.
	call rdselect (output, Memc[root], Memc[rowselect], 
                       Memc[colselect], SZ_FNAME)
	if (Memc[rowselect] != EOS || Memc[colselect] != EOS) {
	    call sprintf (Memc[errmsg], SZ_LINE, nosect)
	    call pargstr (output)
	    call error (1, Memc[errmsg])
	}

	# Break input file name into bracketed selectors.
	call rdselect (input, Memc[root], Memc[rowselect], 
                       Memc[colselect], SZ_FNAME)

	# Open input table and get some info about it.
	itp = tbtopn (Memc[root], READ_ONLY, NULL)
	numrow = tbpsta (itp, TBL_NROWS)
	numcol = tbpsta (itp, TBL_NCOLS)

	# Find how many rows were requested by row selector.
	# If only one, turn off suffixing. Also do it in case
	# ASCII output was requested.
	nrows = selrows (itp, Memc[rowselect])
	if (nrows == 1)
	    suffix = false
	else
	    suffix = true
	if (streq (output, "STDOUT"))
	    suffix = false

	# Create array of column pointers from column selector.
	call malloc (colptr, numcol, TY_INT)
	call malloc (newcol, numcol, TY_INT)
	call tcs_open (itp, Memc[colselect], Memi[colptr], numptr, numcol)

	# Take an error exit if no columns were matched.
	if (numptr == 0) {
	    call sprintf (Memc[errmsg], SZ_LINE, nocols)
	        call pargstr (input)
	        call error (1, Memc[errmsg])
	}

	# Loop over selected rows on input table, creating
	# a 2D output table for each row.
	pcode = trsopen (itp, Memc[rowselect])
	do irow = 1, numrow {
	    if (trseval (itp, irow, pcode)) {

	        # Append suffix to output name.
	        if (suffix)
	            call txtsuff (output, Memc[newname], irow)
	        else
	            call strcpy (output, Memc[newname], SZ_FNAME)

		if (verbose) {
		    call printf ("%s row=%d  -> %s\n")
			call pargstr (input)
			call pargi (irow)
			call pargstr (Memc[newname])
	                call flush (STDOUT)
		}

	        # Open output table and set its type.
		call tbfpri (Memc[root], Memc[newname], phu_copied)
	        otp = tbtopn (Memc[newname], NEW_FILE, NULL)
	        type = tbpsta (itp, TBL_WHTYPE)
	        if (streq (output, "STDOUT"))   # ASCII output.
	            type = TBL_TYPE_TEXT
	        call tbpset (otp, TBL_WHTYPE, type)

	        # Copy column information from input to output.
	        call txtcpyco (otp, colptr, newcol, numptr, colname, 
                              colunits, colfmt, compact)

	        # Create table and copy header.
	        call tbtcre (otp)
	        call tbhcal (itp, otp)

	        # Copy row number into header.
	        call tbhadi (otp, "ORIG_ROW", irow)

	        # Copy scalar columns into header.
	        if (compact)
	            call txtcpysc (otp, colptr, newcol, numptr, colname, 
                                   colunits, colfmt)

	        # Copy number of columns into header. This is used
	        # by task that reads back 2D tables into 3D format.
	        if (compact)
	            call tbhadi (otp, "TCTOTAL", numptr)

	        # Copy data to output table.
	        call txtcpy (itp, otp, irow, Memi[colptr], Memi[newcol],
                             numptr, compact)

	        # Close output.
	        call tbtclo (otp)
	    }
	}

	# Free arrays associated with columns.
	call tcs_close (Memi[colptr], numptr)
	call mfree (newcol, TY_INT)
	call mfree (colptr, TY_INT)

	# Close row selector structure and input table.
	call trsclose (pcode)
	call tbtclo (itp)

	call sfree (sp)
end




#  Appends sufix to output file name.

procedure txtsuff (filename, newname, row)

char	filename[ARB]	# i: output table name
char	newname[ARB]	# o: output table name with suffix
int	row		# i: row number

pointer	sp, ext, suffix
int	dot, i, j

int	strcmp(), strldxs()

begin
	call smark (sp)
	call salloc (suffix, SZ_LINE, TY_CHAR)
	call salloc (ext,    SZ_LINE, TY_CHAR)

	# Get rid of any appendages except the extension.
	call imgcluster (filename, newname, SZ_FNAME)

	# Valid extensions are .tab, .fit and .fits
	# Everything else is part of the root file name.

	# Detect extension.
	Memc[ext] = EOS
	dot = strldxs (".", newname)
	if (dot != 0) {
	    i = dot
	    j = 0
	    while (newname[i] != EOS) {
	        Memc[ext+j] = newname[i]
	        j = j + 1
	        i = i + 1
	    }
	    Memc[ext+j] = EOS
	}

	# If valid extension, remove it from name.
	if ( (strcmp (Memc[ext], ".tab")  == 0) ||
	     (strcmp (Memc[ext], ".fit")  == 0) ||
	     (strcmp (Memc[ext], ".fits") == 0) ) 
	    newname[dot] = EOS
	else
	    Memc[ext] = EOS

	# Build suffix.
	call sprintf (Memc[suffix], SZ_LINE, "_r%04d")
	    call pargi (row)

	# Append suffix and extension to root name.
	call strcat (Memc[suffix], newname, SZ_FNAME)
	call strcat (Memc[ext],    newname, SZ_FNAME)

	call sfree (sp)
end

