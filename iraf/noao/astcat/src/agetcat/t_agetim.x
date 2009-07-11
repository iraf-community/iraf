include "../../lib/astrom.h"
include <pkg/cq.h>

define	SZ_IMEXTN	10

procedure t_agetim()

pointer	sp, output, extn, str1, str2
pointer	cq, at, im, res
int	i, j, index, nfields, svlist, imlist, addext
char	period
bool	wcsedit, hdredit, update, verbose
pointer	cq_map(), immap(), cq_imquery()
int	at_rclist(), at_svlist(), at_osvlist(), fntlenb(), cq_setcat()
int	at_rcquery(), fntrfnb(), strldx(), imaccess(), imtlen(), imtrgetim()
int	open()
bool	clgetb(), streq()
data	period /'.'/
errchk	open(), immap(), imaccess(), cq_fgstr()

begin
	# Allocate some working memory.
	call smark (sp)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (str1, SZ_FNAME, TY_CHAR)
	call salloc (str2, SZ_FNAME, TY_CHAR)
	call salloc (extn, SZ_IMEXTN, TY_CHAR)

	# Initalize the data structures
	call at_aiginit (at)

	# Get the iportant query parameters.
	call clgstr ("regions", Memc[str1], SZ_FNAME)
	call clgstr ("images", Memc[output], SZ_FNAME)

	# Get the editing parameters.
	wcsedit = clgetb ("wcsedit")
	hdredit = clgetb ("hdredit")
	update = clgetb ("update")
	verbose = clgetb ("verbose")

	# Allocate the astrometry structure and read in the algorithm
	# parameters. This must be done before the field centers are
	# decoded.
	call at_giapars (at)

	# Print the field center parameters.
	#call at_rcshow (at)
	# Print the default wcs parameters.
	#call at_wcshow (at)
	# Print the default image data parameters.
	#call at_imshow (at)

	# Get the field center list.
	nfields = at_rclist (at, Memc[str1])
	if (nfields <= 0) {
	    if (verbose)
		call printf ("The field center list is empty\n")
	    call at_aigfree (at)
	    call sfree (sp)
	    return
	}

	# Print the field center symbol table.
	#call at_stshow (at)

	# Get the surverys list.
	call clgstr ("imsurveys", Memc[str1], SZ_FNAME)
	call clgstr ("imdb", Memc[str2], SZ_FNAME)
	svlist = at_svlist (Memc[str1],  Memc[str2])
	if (fntlenb (svlist) <= 0) {
	    if (verbose)
		call printf ("The image surveys list is empty\n")
	    call at_aigfree (at)
	    call fntclsb (svlist)
	    call sfree (sp)
	    return
	}
	call at_sets (at, SURVEYS, Memc[str1])
	call at_sets (at, IMDB, Memc[str2])

	# Print the i/o parameters.
	#call at_ioshow (at)

	# Create the output image list.
	imlist = at_osvlist (at, svlist, Memc[output], "default", "", NO) 
	if (imtlen (imlist) <= 0) {
	    if (verbose)
		call printf ("The output images list is empty\n")
	    call at_aigfree (at)
	    call imtclose (imlist)
	    call fntclsb (svlist)
	    call sfree (sp)
	    return
	}
	call at_sets (at, IMAGES, Memc[output])

	# Open the catalog database.
	cq = cq_map (Memc[str2], READ_ONLY)
	if (verbose) {
	    call printf ("\nOpening surveys database %s\n")
		call pargstr (Memc[str2])
	}

	# Loop over the catalog list.
	do i = 1, fntlenb (svlist) {

	    # Get the catalog name and save it.
	    if (fntrfnb (svlist, i, Memc[str1], SZ_FNAME) == EOF)
		break
	    if (cq_setcat (cq, Memc[str1]) <= 0) {
		if (verbose) {
		    call printf ("Skipping survey %s\n")
		        call pargstr (Memc[str1])
	            call flush (STDOUT)
		}
		next
	    } else {
	        call at_sets (at, SVNAME, Memc[str1])
	        if (verbose) {
		    call printf ("Selecting survey %s\n")
		        call pargstr (Memc[str1])
	            call flush (STDOUT)
	        }
	    }

	    # Loop over the field centers.
	    do j = 1, nfields {

		# Get the output file name.
	        if (imtrgetim (imlist, (i - 1) * nfields + j, Memc[str1],
		    SZ_FNAME) == EOF)
		    break

		# If the file is a fits file tack on the user extension. This
		# is not the correct way to do this but for the moment it will
		# work. Not sure there is a totally clean way to do this since
		# we are not going through imio.

		ifnoerr {
		    call cq_fgstr (cq, "type", Memc[extn], SZ_IMEXTN)
		} then {
		    addext = YES
		    index = strldx (period, Memc[str1])
		    if (index > 0) {
			if (streq (Memc[extn], Memc[str1+index]))
			    addext = NO
			else
			    addext = YES
		    }
		    if (addext == YES) {
			call strcpy (Memc[str1], Memc[str2], SZ_FNAME)
			call strcat (".", Memc[str2], SZ_FNAME)
			call strcat (Memc[extn], Memc[str2], SZ_FNAME)
			call strcpy (Memc[str2], Memc[str1], SZ_FNAME)
		    }
		} else {
		    if (verbose)
		        call printf (
			    "    Warning the image format is undefined\n")
		}
	        call at_sets (at, IMNAME, Memc[str1])

		# Can the output file be opened ?
		iferr {
		    im = open (Memc[str1], NEW_FILE, BINARY_FILE)
		} then {
	            if (verbose) {
		        call printf ("    Unable to write output image %s\n")
			    call pargstr (Memc[str1])
		    }
		    break
		} else {
		    call close (im)
		    call delete (Memc[str1])
		}

		# Format the query.
		if (at_rcquery (at, cq, j) == ERR) {
	            if (verbose)
		        call printf ("    Unable to format network query\n")
		    break
		}

		# Query the image surveys.
		if (verbose) {
		    call printf ("Getting image %s ...\n")
			call pargstr (Memc[str1])
			call flush (STDOUT)
		}
		res = cq_imquery (cq, Memc[str1])
		if (res == NULL) {
	            if (verbose)
		        call printf ("    Network query failed\n")
		    next
		} 

		# Open the output file.
		iferr {
		    if (imaccess (Memc[str1], READ_WRITE) == YES) {
			if (wcsedit || hdredit) {
		            im = immap (Memc[str1], READ_WRITE, 0)
		            if (wcsedit)
		                call at_wedit (im, res, NULL, CQ_WNONE, true,
			            verbose) 
		            if (hdredit) 
		                call at_hedit (im, res, NULL, true, verbose)
		            call imunmap (im)
			}
		    } else
			im = NULL
		} then {
	            if (verbose) {
		        call printf (
			    "    Warning %s is not a valid image\n")
			    call pargstr (Memc[str1])
		    }
		    im = NULL
		}

		if (verbose)
		    call flush (STDOUT)

		# Close the query structure.
		call cq_imclose (res)

	    }

	}

	# Close the catalog database.
	call cq_unmap (cq)

	# Update the algorithm parameters.
	if (update)
	    call at_gippars (at)

	# Close the file and image lists.
	call imtclose (imlist)
	call fntclsb (svlist)

	# Free the astrometry structure.
	call at_aigfree (at)

	# Free the working memory.
	call sfree (sp)
end
