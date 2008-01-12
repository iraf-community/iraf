include	<tbset.h>
include	<imhdr.h>

#  TXIONE  --  Extract images from a single input 3D table.
#
#
#
#  This code is adapted from B.Simon's 04-Nov-94 version of tcopy.
#
#
#  Revision history:
#  ----------------
#
#  22-Nov-96  -  Task created (I.Busko)
#  16-Dec-96  -  Add ORIG_ROW keyword (IB).
#  03-Jan-97  -  Revised after code review (IB)
#  17-Mar-97  -  Added selrows call (IB)
#   8-Apr-02  -  Remove the call to whatfile (P. Hodge)


procedure txione (input, output, verbose)

char	input[ARB]	# i: input table name
char	output[ARB]	# i: output table name
bool	verbose		# i: print operations ?
#--
int     numrow, numcol, numptr, irow, nrows
int     colnum, datatype, lendata, lenfmt
pointer	sp, root, extend, rowselect, colselect, colname, colunits, colfmt
pointer errmsg, icp, itp, im, colptr, pcode 
pointer	newname
bool	suffix

string	noarray  "No valid image data in %s"
string	nocols   "Column name not found (%s)"
string	manycols "Too many columns (%s)"

errchk	tbtopn, trsopen, trseval

bool	trseval()
int	tbpsta(), tcs_totsize(), selrows()
pointer	tbtopn(), tcs_column, trsopen(), immap()

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

	# Break input file name into bracketed selectors.
	call rdselect (input, Memc[root], Memc[rowselect], 
                       Memc[colselect], SZ_FNAME)

	# Open input table and get some info about it.
	itp = tbtopn (Memc[root], READ_ONLY, NULL)
	numrow = tbpsta (itp, TBL_NROWS)
	numcol = tbpsta (itp, TBL_NCOLS)

	# Find how many rows were requested by row selector.
	# If only one, turn off suffixing. 
	nrows = selrows (itp, Memc[rowselect])
	if (nrows == 1)
	    suffix = false
	else
	    suffix = true

	# Create array of column pointers from column selector.
        # This is necessary to avoid segv in case more than one
        # column selector is passed to the task.
	call malloc (colptr, numcol, TY_INT)
	call tcs_open (itp, Memc[colselect], Memi[colptr], numptr, numcol)

	# Take an error exit if either no columns were matched or
        # more than one column was matched.
	if (numptr == 0) {
	    call sprintf (Memc[errmsg], SZ_LINE, nocols)
	        call pargstr (input)
	        call error (1, Memc[errmsg])
	} else if (numptr != 1) {
	    call sprintf (Memc[errmsg], SZ_LINE, manycols)
	        call pargstr (input)
	        call error (1, Memc[errmsg])
	}

	# Loop over selected rows on input table,
	# creating an image for each row.
	pcode = trsopen (itp, Memc[rowselect])
	do irow = 1, numrow {
	    if (trseval (itp, irow, pcode)) {

	        # Append suffix to output name.
	        if (suffix)
	            call txisuff (output, Memc[newname], irow)
	        else
	            call strcpy (output, Memc[newname], SZ_FNAME)

		if (verbose) {
		    call eprintf ("%s row=%d  -> %s\n")
			call pargstr (input)
			call pargi (irow)
			call pargstr (Memc[newname])
		}

	        # Get column information.
	        icp = tcs_column (Memi[colptr])
	        call tbcinf (icp, colnum, Memc[colname], Memc[colunits], 
                             Memc[colfmt], datatype, lendata, lenfmt)

	        # Take error exit if scalar or invalid type.
	        if ((lendata < 2) || (datatype < 0) || (datatype == TY_BOOL)){
	            call sprintf (Memc[errmsg], SZ_LINE, noarray)
	                call pargstr (input)
	                call error (1, Memc[errmsg])
	        }

	        # Open output image
	        im = immap (Memc[newname], NEW_IMAGE, NULL)
		IM_NDIM(im) = 1

	        # Copy array to image.
	        IM_LEN(im,1)   = tcs_totsize (Memi[colptr])
		IM_PIXTYPE(im) = datatype
	        call txicpy (itp, im, irow, Memi[colptr], datatype, 
                             IM_LEN(im,1))

	        # Write column data into header.
	        call txihc (im, colnum, Memc[colname], Memc[colunits], 
                            Memc[colfmt], lenfmt)

	        # Write row number into header.
	        call imaddi (im, "ORIG_ROW", irow)

	        # Close output.
	        call imunmap (im)
	    }
	}

	# Free memory associated with columns.
	call tcs_close (Memi[colptr], numptr)
	call mfree (colptr, TY_INT)

	# Close row selector structure and input table.
	call trsclose (pcode)
	call tbtclo (itp)

	call sfree (sp)
end




#  Appends sufix to output image name.

procedure txisuff (filename, newname, row)

char	filename[ARB]	# i: output image name
char	newname[ARB]	# o: output image name with suffix
int	row		# i: row number

pointer	sp, ext, suffix
int	dot, i, j

int	strcmp(), strldxs(), strlen()

begin
	call smark (sp)
	call salloc (suffix, SZ_LINE, TY_CHAR)
	call salloc (ext,    SZ_LINE, TY_CHAR)

	# Get rid of any appendages except the extension.
	call imgcluster (filename, newname, SZ_FNAME)

	# Valid extensions are .??h, .fit and .fits
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
	if ( ((strlen (Memc[ext]) == 4) && (Memc[ext+3] == 'h')) ||
	     (strcmp (Memc[ext], ".fit")  == 0)                  ||
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
