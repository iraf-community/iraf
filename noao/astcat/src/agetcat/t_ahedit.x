include "../../lib/astrom.h"
include <pkg/cq.h>

procedure t_ahedit()

pointer	sp, images, str1, str2
pointer	at, cq, res, im
int	j, imlist, catno, wcstype
bool	hupdate, wcsedit, hdredit, update, verbose
bool	clgetb()
pointer	cq_map(), cq_fimquery(), immap()
int	imtopen(), imtlen(), cq_setcat(), imtrgetim(), imaccess(), strdic()
errchk	immap(), imaccess()

begin
	# Allocate some working memory.
	call smark (sp)
	call salloc (images, SZ_FNAME, TY_CHAR)
	call salloc (str1, SZ_FNAME, TY_CHAR)
	call salloc (str2, SZ_FNAME, TY_CHAR)

	# Get the iportant query parameters.
	call clgstr ("images", Memc[images], SZ_FNAME)

	# Get the editing parameters.
	hupdate = clgetb ("hupdate")
	wcsedit = clgetb ("wcsedit")
	call clgstr ("wcs", Memc[str1], SZ_FNAME)
	wcstype = strdic (Memc[str1], Memc[str1], SZ_FNAME, CQ_WTYPESTR)
	if (wcstype <= 0)
	    wcstype = CQ_WNONE
	hdredit = clgetb ("hdredit")
	update = clgetb ("update")
	if (hupdate)
	    verbose = clgetb ("verbose")
	else
	    verbose = true

	# Open the image list.
	imlist = imtopen (Memc[images])
	if (imtlen (imlist) <= 0) {
	    if (verbose)
		call printf ("The input image list is empty\n")
	    call imtclose (imlist)
	    call sfree (sp)
	    return
	}

	# Initalize the data structures
	call at_ahinit (at)

	# Allocate the astrometry structure and read in the algorithm
	# parameters.
	call at_hapars (at)

	# Print the default wcs parameters.
	#call at_wcshow (at)
	# Print the default image data parameters.
	#call at_imshow (at)

	# Set the i/o parameters.
	call clgstr ("imsurveys", Memc[str1], SZ_FNAME)
	call clgstr ("imdb", Memc[str2], SZ_FNAME)
	call at_sets (at, IMAGES, Memc[images])
	call at_sets (at, SURVEYS, Memc[str1])
	call at_sets (at, IMDB, Memc[str2])

	# Print the i/o parameters.
	#call at_ioshow (at)

	# Open the catalog database.
	cq = cq_map (Memc[str2], READ_ONLY)
	if (cq == NULL) {
	    if (verbose) {
	        call printf ("\nCannot opening surveys database %s\n")
		    call pargstr (Memc[str2])
	    }
	    call at_ahfree (at)
	    call imtclose (imlist)
	    call sfree (sp)
	    return
	} else if (verbose) {
	    call printf ("\nOpening surveys database %s\n")
		call pargstr (Memc[str2])
	    call flush (STDOUT)
	}

	# Get the catalog name and save it.
	catno = cq_setcat (cq, Memc[str1])
	if (Memc[str1] == EOS) {
	    catno = ERR
	} else if (catno == 0) {
	    if (verbose) {
	        call printf ("Cannot locate survey %s\n")
	            call pargstr (Memc[str1])
	    }
	} else {
	    if (verbose) {
	        call printf ("Selecting survey %s\n")
		    call pargstr (Memc[str1])
	    }
	    call at_sets (at, CATNAME, Memc[str1])
	}

	# Loop over the field centers.
	do j = 1, imtlen (imlist) {

	    # Get the output image name.
	    if (imtrgetim (imlist, j, Memc[str1], SZ_FNAME) == EOF)
	        break
	    call at_sets (at, IMNAME, Memc[str1])

	    # Query the image survey to get the header info even though the
	    # image already exists.
	    if (verbose) {
		call printf ("Getting image %s ...\n")
		    call pargstr (Memc[str1])
		    call flush (STDOUT)
	    }
	    if (catno <= 0)
		res = NULL
	    else
		res = cq_fimquery (cq, Memc[str1])

	    # Open the output file.
	    iferr {
		if (imaccess (Memc[str1], READ_WRITE) == YES) {
		    im = immap (Memc[str1], READ_WRITE, 0)
		    if (wcsedit) {
		        if (res != NULL)
		            call at_wedit (im, res, NULL, wcstype, hupdate,
			        verbose) 
		        else
		            call at_wedit (im, NULL, at, wcstype, hupdate,
			        verbose) 
		    }
		    if (hdredit) {
		        if (res != NULL)
		            call at_hedit (im, res, NULL, hupdate, verbose)
		        else
		            call at_hedit (im, NULL, at, hupdate, verbose)
		    }
	            call imunmap (im)
		} else
		    im = NULL
	    } then {
	        if (verbose) {
		    call printf ("    Warning %s is not a valid image name\n")
			call pargstr (Memc[str1])
		}
		im = NULL
	    }

	    # Close the query structure.
	    if (res != NULL)
		call cq_imclose (res)

	}

	# Close the catalog database.
	call cq_unmap (cq)

	# Update the algorithm parameters.
	if (update)
	    call at_hppars (at)

	# Close the image lists.
	call imtclose (imlist)

	# Free the astrometry structure.
	call at_ahfree (at)

	# Free the working memory.
	call sfree (sp)
end
