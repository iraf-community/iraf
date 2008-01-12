include <error.h>
include "tiimage.h"

#  TM_LOOP  --  Scan input list and insert each image in turn.
#
#
#
#
#  Revision history:
#  ----------------
#  30-Jan-97  -  Task created (I.Busko)


procedure tm_loop (tp, cp, ncp, row, imlist, mode, outname, verbose)

pointer	tp		# table pointer
pointer	cp		# column pointer array
int	ncp		# size of column pointer array
int	row		# row where to begin insertion
char	imlist[ARB]	# input image list
int	mode		# operating mode
char	outname[ARB]	# output table name (for listing only)
bool	verbose		# print info ?
#--
pointer	sp, im, list, fname
int	i, rowc, imc, image
bool	rflag

errchk	immap, tm_hc, tm_copy

pointer	immap(), imtopen()
int	imtlen(), imtgetim()

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)

	# Initialize row counter.
	rowc  = row
	rflag = false
	if (rowc <= 0 || IS_INDEFI(rowc)) rflag = true

	# Initialize successful image counter.
	imc = 0

	# Open input list.
	list = imtopen (imlist)	

	# Loop over input list.
	do image = 1, imtlen(list) {

	    # Get input image name and open it. Skip if error.
	    i = imtgetim (list, Memc[fname], SZ_PATHNAME)
	    iferr (im = immap (Memc[fname], READ_ONLY, 0)) {
	        call erract (EA_WARN)
	        next
	    }
	    if (verbose) {
	        call printf ("%s ")
	        call pargstr (Memc[fname])
	        call flush (STDOUT)
	    }

	    # Look into image header for columnar info and do the copy.
	    if (mode == MODE_OUT_ALL || mode == MODE_TEM_ALL) {
	        iferr (call tm_hc (tp, cp, ncp, rowc, rflag, im)) {
	            call erract (EA_WARN)
	            call imunmap (im)
	            next
	        }

	        # Bump row and image counters.
	        rowc = rowc + 1
	        imc  = imc  + 1

	    # Just copy into single column.
	    } else if (mode == MODE_OUT_SINGLE || mode == MODE_TEM_SINGLE) {
	        iferr (call tm_copy (tp, Memi[cp], rowc, rflag, im)) {
	            call erract (EA_WARN)
	            call imunmap (im)
	            next
	        }

	        # Bump row and image counters.
	        rowc = rowc + 1
	        imc  = imc  + 1
	    }

	    if (verbose) {
	        call printf ("-> %s row=%d \n")
	        call pargstr (outname)
	        call pargi (rowc-1)
	        call flush (STDOUT)
	    }

	    # Close current image.
	    call imunmap (im)
	}

	call imtclose (list)
	call sfree (sp)
	if (imc == 0)
	    call error (1, "No images were inserted.")
end
