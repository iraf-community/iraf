include <pkg/cq.h>
include "../../lib/astrom.h"

define	SZ_HDRTEXT	(5 * SZ_LINE)

procedure t_aimfind()

pointer	sp, images, output, imfile, catalog, catdb, hdrtext, str1
pointer	at, cq, cres, res, sym, im
int	i, j, nfields, nout, nlines, catlist, outlist, imfd, infd, outfd
bool	standard, filter, append, update, verbose
pointer	cq_map(), cq_query(), cq_fquery(), at_tquery(), at_rcsym(), immap()
int	at_rclist(), at_catlist(), at_ocatlist(), open(), access()
int	fntlenb(), fntrfnb(), cq_setcat(), at_rcquery()
int	at_gcathdr(), cq_rstati()
bool	clgetb()
errchk	open()

begin
	# Allocate some working memory.
	call smark (sp)
	call salloc (images, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (imfile, SZ_FNAME, TY_CHAR)
	call salloc (catalog, SZ_FNAME, TY_CHAR)
	call salloc (catdb, SZ_FNAME, TY_CHAR)
	call salloc (hdrtext, SZ_HDRTEXT, TY_CHAR)
	call salloc (str1, SZ_FNAME, TY_CHAR)

	# Initalize the data structures
	call at_aginit (at)

	# Get the iportant query parameters.
	call clgstr ("images", Memc[images], SZ_FNAME)
	call clgstr ("output", Memc[output], SZ_FNAME)
	call clgstr ("imfile", Memc[imfile], SZ_FNAME)

	# Get the mode parameters.
	standard = clgetb ("standard")
	filter = clgetb ("filter")
	append = clgetb ("append")
	update = clgetb ("update")
	verbose = clgetb ("verbose")

	# Allocate the astrometry structure and read in the algorithm
	# parameters. If filtering is turned off then the filtering
	# parameters are set to their default values. Probably need to
	# make high level wrapper routines for the parameter defaults
	# routines at some point.
	if (! filter)
	    call at_dfspset(at)
	else
	    call at_iapars (at)

	# Set the new field values and new field descriptions. At some
	# point these may be input from parameters. At present they are
	# hardwired.
	call at_nflist (at, 2, "xp,yp", "d,d", "pixels,pixels",
	    "%10.3f,%10.3f", append)

	# Print the field center parameters.
	#call at_rcshow (at)
	# Print the filtering parameters.
	#call at_fsshow (at)
	# Print the wcs parameters.
	#call at_wcshow (at)
	# Print the image parameters.
	#call at_imshow (at)

	# Create the region list from the image list. If an image does not
	# have a valid fits wcs it will not be included in the valid
	# region list. 
	nfields = at_rclist (at, Memc[images])
	if (nfields <= 0) {
	    if (verbose)
		call printf ("The image list is empty\n")
	    call at_agfree (at)
	    call sfree (sp)
	    return
	}

#	# Print the field center symbol table.
#	#call at_stshow (at)

	# Get the catalog. The catalog may be a catalog server or an
	# astrometry file.
	call clgstr ("catalogs", Memc[catalog], SZ_FNAME)
	call clgstr ("catdb", Memc[catdb], SZ_FNAME)
	catlist = at_catlist (Memc[catalog], Memc[catdb])
	if (fntlenb (catlist) != 1) {
	    if (verbose) {
		if (fntlenb (catlist) <= 0)
		    call printf ("The catalog is undefined\n")
		else
		    call printf ("More than one catalog is specified\n")
		    
	    }
	    call fntclsb (catlist)
	    call at_agfree (at)
	    call sfree (sp)
	    return
	}
	call at_sets (at, CATALOGS, Memc[catalog])
	call at_sets (at, CATDB, Memc[catdb])

	# Open the output image list file. If the output image file name
	# is imdefined then no image list file is written.
	if (Memc[imfile] == EOS) {
	    imfd = NULL
	} else {
	    iferr (imfd = open (Memc[imfile], NEW_FILE, TEXT_FILE))
		imfd = NULL
	}

#	# Print the i/o parameters.
#	#call at_ioshow (at)

	# Create the output astrometry file list. If the output astrometry
	# file list is empty no astrometry file is written.
	outlist = at_ocatlist (at, catlist, Memc[output], "default", "coo", NO) 
	call at_sets (at, OUTPUT, Memc[output])

	# Open the catalog database.
	cq = cq_map (Memc[catdb], READ_ONLY)
	if (verbose) {
	    call printf ("\nOpening catalog database %s\n")
	        call pargstr (Memc[catdb])
	}

	# Loop over the catalog list.
	nout = 0
	do i = 1, fntlenb (catlist) {

	    # Get the catalog name and save it.
	    if (fntrfnb (catlist, i, Memc[catalog], SZ_FNAME) == EOF)
		break

	    # Set the catalog.
	    if (access (Memc[catalog], READ_ONLY, TEXT_FILE) == YES) {
		if (cq_setcat (cq, "filename@noao") <= 0) {
		    if (verbose) {
		        call printf ("Skipping catalog %s\n")
		            call pargstr (Memc[catalog])
	                call flush (STDOUT)
		    }
		    next
		} else {
	            call at_sets (at, CATNAME, Memc[catalog])
	            if (verbose) {
		        call printf ("Selecting catalog %s\n")
		            call pargstr (Memc[catalog])
	                call flush (STDOUT)
	            }
		}
	    } else if (cq_setcat (cq, Memc[catalog]) <= 0) {
		if (verbose) {
		    call printf ("Skipping catalog %s\n")
		        call pargstr (Memc[catalog])
	            call flush (STDOUT)
		}
		next
	    } else {
	        call at_sets (at, CATNAME, Memc[catalog])
	        if (verbose) {
		    call printf ("Selecting catalog %s\n")
		        call pargstr (Memc[catalog])
	            call flush (STDOUT)
	        }
	    }

	    # Loop over the field centers.
	    do j = 1, nfields {

		# Get the output file name.
	        if (fntrfnb (outlist, (i - 1) * nfields + j, Memc[str1],
		    SZ_FNAME) == EOF)
		    call at_sets (at, OUTFNAME, "")
		    #break
		else
	            call at_sets (at, OUTFNAME, Memc[str1])

		# Query the catalog.
	        if (access (Memc[catalog], READ_ONLY, TEXT_FILE) == YES) {

		    # Read the catalog header.
		    infd = open (Memc[catalog], READ_ONLY, TEXT_FILE)
		    nlines = at_gcathdr (infd, Memc[hdrtext], SZ_HDRTEXT)
		    call close (infd)
		    if (nlines <= 0) {
			if (verbose)
		            call printf ("    Unable to read catalog header\n")
			break
		    }

		    # Copy the standard star catalog into the query structure.
		    cres = cq_fquery (cq, Memc[catalog], Memc[hdrtext])
		    if (cres == NULL) {
			if (verbose) 
			    call printf ("    Catalog query failed\n")
			break
		    }

		    # Extract the requested data.
		    res = at_tquery (at, cq, cres, Memc[hdrtext], nlines, j)
		    if (res == NULL) {
			if (verbose) 
			    call printf ("    Catalog query failed\n")
			break
		    }
		    call cq_rclose (cres)


		} else {

		    # Format the network query.
		    if (at_rcquery (at, cq, j) == ERR) {
	                if (verbose)
		            call printf ("    Unable to format network query\n")
		        break
		    }

		    # Query the catalog.
		    res = cq_query (cq)
		    if (res == NULL) {
	                if (verbose)
		            call printf ("    Network query failed\n")
		        break
		    }
		}

		# Get the region symbol.
		sym = at_rcsym (at, j)

		# If at least one object was detected in the image then
		# write out the catalog for that image, and add the image
		# name to the image file.
		if (cq_rstati (res, CQRNRECS) > 0) {

		    # Print the number of objects found.
		    if (verbose) {
			call printf (
			    "    Image %s contains %d catalog objects\n")
			    call pargstr (AT_RCSTNAME(sym))
			    call pargi (cq_rstati(res, CQRNRECS))
			call flush (STDOUT)
		    }

		    # Write the query results to the astrometry file.
		    outfd = NULL
		    if (fntlenb (outlist) > 0) {

		        # Open the output file.
		        outfd = open (Memc[str1], NEW_FILE, TEXT_FILE)
		        if (verbose) {
			    call printf ("    Writing catalog file %s\n")
				call pargstr (Memc[str1])
			}

			im = immap (AT_RCSTNAME(sym), READ_ONLY, 0)
		        call at_wifilrecs (outfd, im, at, res, standard)
			call imunmap (im)

		        # Close the output file.
		        call close (outfd)
		    }

		    # Write the image name to the image file.
		    if (imfd != NULL) {
			if (outfd != NULL) {
			    call fprintf (imfd, "%s  %s\n")
			        call pargstr (AT_RCSTNAME(sym))
			        call pargstr (Memc[str1])
			} else {
			    call fprintf (imfd, "%s\n")
			        call pargstr (AT_RCSTNAME(sym))
			}
		    }

		    # Count the number of non-empty files
		    nout = nout + 1

		} else if (verbose) {
		    call printf ("    Image %s contains no catalog objects\n")
			call pargstr (AT_RCSTNAME(sym))
		    call flush (STDOUT)
		}

		# Close the query structure.
		call cq_rclose (res)

	    }

	}

	# Close the catalog database.
	call cq_unmap (cq)

	# Update the algorithm parameters.
	if (update)
	    call at_ippars (at)

	# Close the catalog and output file lists.
	call fntclsb (outlist)
	call fntclsb (catlist)

	# Close the image list file. Delete it if it is empty.
	if (imfd != NULL) {
	    call close (imfd)
	    if (nout <= 0)
		call delete (Memc[imfile])
	}

	# Free the astrometry structure.
	call at_agfree (at)

	# Free the working memory.
	call sfree (sp)
end
