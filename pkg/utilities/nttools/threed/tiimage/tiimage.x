include <tbset.h>
include "tiimage.h"

#  TIIMAGE  --  Insert 1D images into 3D table rows.
#
#  Input images are given by a filename template list. The output is a 
#  3D table with optional column selector. 
#
#
#
#  Revision history:
#  ----------------
#  30-Jan-97  -  Task created (I.Busko)


procedure t_tiimage()

char	imlist[SZ_LINE]			# Input image list
char	output[SZ_PATHNAME]		# Output table name
char	template[SZ_PATHNAME]		# Template table name
int	row				# Row where to begin insertion
bool	verbose				# Print operations ?
#--
char	root[SZ_FNAME]			# String storage areas used
char	rs[SZ_FNAME]			# by row/column selector
char	cs[SZ_FNAME]			# mechanism
char	cn[SZ_COLNAME]
char	cu[SZ_COLUNITS]
char	cf[SZ_COLFMT]
pointer	sp, otp, ttp, ocp, tcp, newocp, tempp, list
int	nocp, mode, numcol, dtyp, lend, lenf, cnum, i

pointer	tbtopn(), tcs_column(), imtopen()
int	clgeti(), tbpsta(), tm_mode(), imtlen()
bool	clgetb(), streq()

begin
	# Get task parameters.
	call clgstr ("input", imlist, SZ_LINE)
	call clgstr ("outtable", output, SZ_PATHNAME)
	call clgstr ("template", template, SZ_PATHNAME)
	row     = clgeti ("row")
	verbose = clgetb ("verbose")

	# Abort if invalid output name.
	if (streq (output, "STDOUT"))
	    call error (1, "Invalid output file name.")

	# Decide which mode to use.
	mode = tm_mode (output, template, root, rs, cs, cn, cu, cf)

	call smark (sp)
	switch (mode) {

	case MODE_OUT_SINGLE,MODE_OUT_ALL:

	    # Break output table name into bracketed selectors.
	    call rdselect (output, root, rs, cs, SZ_PATHNAME)

	    # Open output table.
	    otp = tbtopn (root, READ_WRITE, 0)

	    # Create arrays with selected column pointer(s).
	    numcol = tbpsta (otp, TBL_NCOLS)
	    call salloc (ocp,    numcol, TY_INT)
	    call salloc (newocp, numcol, TY_INT)
	    call tcs_open (otp, cs, Memi[ocp], nocp, numcol)

	    # Translate pointer to tbtables-compatible format.
	    do i = 1, nocp
	        Memi[newocp+i-1] = tcs_column (Memi[ocp+i-1])

	    # Do the insertion by looping over all input images.
	    call tm_loop (otp, newocp, nocp, row, imlist, mode, output,
                          verbose)

	    # Close output table.
	    call tbtclo (otp)

	case MODE_TEM_SINGLE,MODE_TEM_ALL:

	    # Get output table root name and open it.
	    call rdselect (output, root, rs, cs, SZ_PATHNAME)
	    otp = tbtopn (root, NEW_FILE, 0)

	    # Break template table name into bracketed 
	    # selectors and open it.
	    call rdselect (template, root, rs, cs, SZ_PATHNAME)
	    ttp = tbtopn (root, READ_ONLY, 0)

	    # Create arrays with selected column pointer(s).
	    numcol = tbpsta (ttp, TBL_NCOLS)
	    call salloc (tcp,    numcol, TY_INT)
	    call salloc (newocp, numcol, TY_INT)
	    call tcs_open (ttp, cs, Memi[tcp], nocp, numcol)

	    # Copy column info from template to output table.
	    do i = 1, nocp {
	        tempp = tcs_column (Memi[tcp+i-1])
	        call tbcinf (tempp, cnum, cn, cu, cf, dtyp, lend, lenf)
	        call tbcdef (otp, tempp, cn, cu, cf, dtyp, lend, 1)
	        Memi[newocp+i-1] = tempp
	    }

	    # Create output and close template.
	    call tbtcre (otp)
	    call tbtclo (ttp)

	    # Do the insertion by looping over all input images.
	    call tm_loop (otp, newocp, nocp, row, imlist, mode, output,
                          verbose)

	    # Close output table.
	    call tbtclo (otp)

	case MODE_SCRATCH:

	    # Alloc memory for column pointer array, assuming  
	    # the worst case of each input image in the list 
	    # belonging to a separate column.
	    list = imtopen (imlist)
	    numcol = imtlen (list)
	    call imtclose (list)
	    call salloc (newocp, numcol, TY_INT)

	    # Open output table.
	    call rdselect (output, root, rs, cs, SZ_PATHNAME)
	    otp = tbtopn (root, NEW_FILE, 0)

	    # Build column descriptor array from info in image headers.
	    ifnoerr (call tm_scan (otp, newocp, numcol, nocp, imlist)) {

	        # Pretend that template table exists and do the insertion. 
	        mode = MODE_TEM_ALL
	        call tm_loop (otp, newocp, nocp, row, imlist, mode, output,
                              verbose)
	    }

	    # Close output table.
	    call tbtclo (otp)

	case MODE_ERROR:
	    call error (1, "Cannot process.")
	}

	call sfree (sp)
end
