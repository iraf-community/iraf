include "../../lib/astrom.h"

define	SZ_HDRTEXT	5 * SZ_LINE

# T_AFILTCAT -- Filter existing astrometry catalogs.

procedure t_afiltcat()

pointer	sp, input, output, catdb, catname, infname, outfname, tmpfname, hdrtext
pointer	at, cq, res
int	icatlist, ocatlist, catno, infd, outfd, nlines
bool	standard, filter, update, verbose
pointer	cq_map(), cq_fquery()
int	fntopnb(), fntlenb(), fntgfnb(), cq_setcat(), open(), at_gcathdr()
int	at_pcathdr()
bool	streq(), clgetb()
errchk	open()

begin
	# Allocate some working space.
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (catdb, SZ_FNAME, TY_CHAR)
	call salloc (catname, SZ_FNAME, TY_CHAR)
	call salloc (infname, SZ_FNAME, TY_CHAR)
	call salloc (outfname, SZ_FNAME, TY_CHAR)
	call salloc (tmpfname, SZ_FNAME, TY_CHAR)
	call salloc (hdrtext, SZ_HDRTEXT, TY_CHAR)

	# Get the important query parameters.
	call clgstr ("input", Memc[input], SZ_FNAME)
	call clgstr ("output", Memc[output], SZ_FNAME)
	call clgstr ("catdb", Memc[catdb], SZ_FNAME)
	call clgstr ("catalogs", Memc[catname], SZ_FNAME)

	standard = clgetb ("standard")
	filter = clgetb ("filter")
	update = clgetb ("update")
	verbose = clgetb ("verbose")

	# Open the input catalog list.
	icatlist = fntopnb (Memc[input], NO)
	ocatlist = fntopnb (Memc[output], NO)

	# Check that the input and output catalogs are the same size.
	if (fntlenb (icatlist) != fntlenb (ocatlist)) {
	    if (verbose) {
	        call printf (
		    "Input and output file lists lengths are different\n")
		call flush (STDOUT)
	    }
	    call fntclsb (icatlist)
	    call fntclsb (ocatlist)
	    call sfree (sp)
	    return
	}

	# Map the database.
	cq = cq_map (Memc[catdb], READ_ONLY)
	if (cq == NULL) {
	    if (verbose) {
	        call printf ("Cannot open catalog configuration file %s\n")
	            call pargstr (Memc[catdb])
		call flush (STDOUT)
	    }
	    call fntclsb (icatlist)
	    call fntclsb (ocatlist)
	    call sfree (sp)
	    return
	} else {
	    if (verbose) {
		call printf ("\nOpening catalog configuration file %s ...\n")
		    call pargstr (Memc[catdb])
		call flush (STDOUT)
	    }
	}

	# Locate the dummy record, usually called "stext".
	catno = cq_setcat (cq, Memc[catname])
	if (catno <= 0) {
	    if (verbose) {
	        call printf ("Cannot locate dummy catalog %s\n")
		    call pargstr (Memc[catname])
		call flush (STDOUT)
	    }
	    call cq_unmap (cq)
	    call fntclsb (icatlist)
	    call fntclsb (ocatlist)
	    call sfree (sp)
	    return
	} else {
	    if (verbose) {
	        call printf ("Selecting dummy catalog %s\n")
		    call pargstr (Memc[catname])
		call flush (STDOUT)
	    }
	}

	# Initilize the astrometry data structure.
	call at_afinit (at)

	# Initialize the algorithm parameters.
	call at_fapars (at)

	# Store the input and output templates.
	call at_sets (at, CATALOGS, Memc[catname])
	call at_sets (at, INPUT, Memc[input])
	call at_sets (at, OUTPUT, Memc[output])
	call at_sets (at, CATDB, Memc[catdb])
	call at_sets (at, CATNAME, Memc[catname])

	# Loop over the input and output files.
	while (fntgfnb (icatlist, Memc[infname], SZ_FNAME) != EOF &&
	    fntgfnb (ocatlist, Memc[outfname], SZ_FNAME) != EOF) {

	    # Store the input and output catalog names.
	    call at_sets (at, INFNAME, Memc[infname])
	    call at_sets (at, OUTFNAME, Memc[outfname])

	    # Create a temporary name and open the output file.
	    if (streq (Memc[infname], Memc[outfname]))
		call mktemp ("tmp", Memc[tmpfname], SZ_FNAME)
	    else
		call strcpy (Memc[outfname], Memc[tmpfname], SZ_FNAME)
	    iferr {
	        outfd = open (Memc[tmpfname], NEW_FILE, TEXT_FILE)	
	    } then {
		if (verbose) {
		    call printf ("    Cannot open output file %s\n")
		        call pargstr (Memc[outfname])
		    call flush (STDOUT)
		    next
		}
	    }

	    # Read the input catalog header.
	    infd = open (Memc[infname], READ_ONLY, TEXT_FILE) 
	    nlines = at_gcathdr (infd, Memc[hdrtext], SZ_HDRTEXT)
	    call close (infd)
	    if (nlines <= 0)
		nlines = at_pcathdr ("acatpars", Memc[hdrtext], SZ_HDRTEXT)

	    # Read in the catalog and make it look like the results
	    # of a query.
	    if (nlines > 0) {
	        res = cq_fquery (cq, Memc[infname], Memc[hdrtext])
	        if (res != NULL) {
                    if (filter) {
                	if (verbose) {
                    	    call printf (
			        "    Filtering catalog %s to catalog %s\n")
                        	call pargstr (Memc[infname])
                        	call pargstr (Memc[outfname])
			}
                	call at_wfilrecs (outfd, at, res, standard)
                    } else {
                	if (verbose) {
                    	    call printf (
			    "    Copying catalog %s to catalog %s\n")
                        	call pargstr (Memc[infname])
                        	call pargstr (Memc[outfname])
                	}
                	call at_wnofilrecs (outfd, res, standard)
            	    }
	        } else {
		    if (verbose) {
		        call printf ("    Cannot read catalog %s\n")
			    call pargstr (Memc[infname])
		        call flush (STDOUT)
		    }
		}
	    } else {
		if (verbose) {
		    call printf ("    Cannot decode catalog %s\n")
			call pargstr (Memc[infname])
		    call flush (STDOUT)
		}
	        res = NULL
	    }
		
	    # Close the results structure.
	    if (res != NULL)
	        call cq_rclose (res)

	    # Close the output file.
	    call close (outfd)

	    # Replace the existing file with the temporary one.
	    if (streq (Memc[infname], Memc[outfname])) {
		call delete (Memc[infname])
		call rename (Memc[tmpfname], Memc[infname])
	    }
	}

	# Free the database.
	call cq_unmap (cq)

	# Update the algorithm parameters.
	if (update)
	    call at_fppars (at)

	# Free the astrometry data structure.
	call at_affree (at)

	# Free the input catalog list.
	call fntclsb (icatlist)
	call fntclsb (ocatlist)

	call sfree (sp)
end
