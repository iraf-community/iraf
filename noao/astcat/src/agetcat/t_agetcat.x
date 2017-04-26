include "../../lib/astrom.h"

define	SZ_HDRTEXT	(5 * SZ_LINE)

procedure t_agetcat()

pointer	sp, output, hdrtext, str1, str2, at, cq, res, cres
int	i, j, nfields, catlist, outlist, infd, outfd, nlines
bool	standard, filter, update, verbose
pointer	cq_map(), cq_query, cq_fquery(), at_tquery()
int	at_rclist(), at_ocatlist(), at_catlist(), fntlenb(), cq_setcat()
int	fntrfnb(), open(), at_rcquery(), access(), at_gcathdr()
bool	clgetb()
errchk	open()

begin
	# Allocate some working memory.
	call smark (sp)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (str1, SZ_FNAME, TY_CHAR)
	call salloc (str2, SZ_FNAME, TY_CHAR)
	call salloc (hdrtext, SZ_HDRTEXT, TY_CHAR)

	# Initalize the data structures
	call at_aginit (at)

	# Get the iportant query parameters.
	call clgstr ("regions", Memc[str1], SZ_FNAME)
	call clgstr ("output", Memc[output], SZ_FNAME)

	# Get the mode parameters.
	standard = clgetb ("standard")
	filter = clgetb ("filter")
	update = clgetb ("update")
	verbose = clgetb ("verbose")

	# Allocate the astrometry structure and read in the algorithm
	# parameters. This must be done before the field centers are
	# decoded.
	call at_gapars (at)

	# Print the field center parameters.
	#call at_rcshow (at)
	# Print the filtering parameters.
	#call at_fsshow (at)
	# Print the wcs parameters.
	#call at_wcshow (at)
	# Print the image parameters.
	#call at_imshow (at)

	# Get the field center list.
	nfields = at_rclist (at, Memc[str1])
	if (nfields <= 0) {
	    if (verbose)
		call printf ("The field center list is empty\n")
	    call at_agfree (at)
	    call sfree (sp)
	    return
	}

	# Print the field center symbol table.
	#call at_stshow (at)

	# Get the catalog list.
	call clgstr ("catalogs", Memc[str1], SZ_FNAME)
	call clgstr ("catdb", Memc[str2], SZ_FNAME)
	catlist = at_catlist (Memc[str1],  Memc[str2])
	if (fntlenb (catlist) <= 0) {
	    if (verbose)
		call printf ("The catalog list is empty\n")
	    call fntclsb (catlist)
	    call at_agfree (at)
	    call sfree (sp)
	    return
	}
	call at_sets (at, CATALOGS, Memc[str1])
	call at_sets (at, CATDB, Memc[str2])


	# Print the i/o parameters.
	#call at_ioshow (at)

	# Create the output catalog file list.
	outlist = at_ocatlist (at, catlist, Memc[output], "default", "cat", NO) 
	if (fntlenb (outlist) <= 0) {
	    if (verbose)
		call printf ("The output file list is empty\n")
	    call fntclsb (outlist)
	    call fntclsb (catlist)
	    call at_agfree (at)
	    call sfree (sp)
	    return
	}
	call at_sets (at, OUTPUT, Memc[output])

	# Open the catalog database.
	cq = cq_map (Memc[str2], READ_ONLY)
	if (verbose) {
	    call printf ("\nOpening catalog database %s\n")
	        call pargstr (Memc[str2])
	}

	# Loop over the catalog list.
	do i = 1, fntlenb (catlist) {

	    # Get the catalog name and save it.
	    if (fntrfnb (catlist, i, Memc[str2], SZ_FNAME) == EOF)
		break
	    if (access (Memc[str2], READ_ONLY, TEXT_FILE) == YES) {
		if (cq_setcat (cq, "filename@noao") <= 0) {
		    if (verbose) {
		        call printf ("Skipping catalog %s\n")
		            call pargstr (Memc[str2])
	                call flush (STDOUT)
		    }
		    next
		} else {
	            call at_sets (at, CATNAME, Memc[str2])
		    if (verbose) {
		        call printf ("Selecting catalog %s\n")
		            call pargstr (Memc[str2])
	                call flush (STDOUT)
		    }
		}
	    } else if (cq_setcat (cq, Memc[str2]) <= 0) {
		if (verbose) {
		    call printf ("Skipping catalog %s\n")
		        call pargstr (Memc[str2])
	            call flush (STDOUT)
		}
		next
	    } else {
	        call at_sets (at, CATNAME, Memc[str2])
	        if (verbose) {
		    call printf ("Selecting catalog %s\n")
		        call pargstr (Memc[str2])
	            call flush (STDOUT)
	        }
	    }

	    # Loop over the field centers.
	    do j = 1, nfields {

		# Get the output file name.
	        if (fntrfnb (outlist, (i - 1) * nfields + j, Memc[str1],
		    SZ_FNAME) == EOF)
		    break
	        call at_sets (at, OUTFNAME, Memc[str1])

		# Open the output file.
		iferr {
		    outfd = open (Memc[str1], NEW_FILE, TEXT_FILE)
		} then {
                    if (verbose) {
                        call printf ("    Unable to open output file %s\n")
                            call pargstr (Memc[str1])
                    }
                    break
		}

		if (access (Memc[str2], READ_ONLY, TEXT_FILE) == YES) {

		    # Read the catalog header.
		    infd = open (Memc[str2], READ_ONLY, TEXT_FILE)
		    nlines = at_gcathdr (infd, Memc[hdrtext], SZ_HDRTEXT)
		    call close (infd)
		    if (nlines <= 0) {
			if (verbose)
			    call printf ("    Unable to read catalog header\n")
			break
		    }

		    # Copy the catalog file into the query structure.
		    cres = cq_fquery (cq, Memc[str2], Memc[hdrtext])
		    if (cres == NULL) {
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

		} else {

		    # Format the query.
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

		# Write the output file.
		if (filter) {
		    if (verbose) {
			call printf ("    Filtering region %d to file %s\n")
			    call pargi (j)
			    call pargstr (Memc[str1])
			call flush (STDOUT)
		    }
		    call at_wfilrecs (outfd, at, res, standard)
		} else {
		    if (verbose) {
			call printf ("    Copying region %d to file %s\n")
			    call pargi (j)
			    call pargstr (Memc[str1])
			call flush (STDOUT)
		    }
		    call at_wnofilrecs (outfd, res, standard)
		}

		# Close the output file.
		call close (outfd)

		# Close the query structure.
		call cq_rclose (res)

	    }

	}

	# Close the catalog database.
	call cq_unmap (cq)

	# Update the algorithm parameters.
	if (update)
	    call at_gppars (at)

	# Close the catalog and output file lists.
	call fntclsb (outlist)
	call fntclsb (catlist)

	# Free the astrometry structure.
	call at_agfree (at)

	# Free the working memory.
	call sfree (sp)
end
