# T_ACLIST -- List the supported catalogs.

procedure t_aclist() 

pointer	sp, str1, str2, line, cq
int	i, j, catlist, nquery, nheader, nfields
bool	verbose
pointer	cq_map()
int	at_catlist(), fntlenb(), fntrfnb(), cq_setcat(), cq_fgeti(), cq_scan()
bool	clgetb()
errchk	cq_fgeti()

begin
        # Allocate some working memory.
        call smark (sp)
        call salloc (str1, SZ_FNAME, TY_CHAR)
        call salloc (str2, SZ_FNAME, TY_CHAR)
        call salloc (line, SZ_LINE, TY_CHAR)

	# Get the parameters.
        call clgstr ("catalogs", Memc[str1], SZ_FNAME)
        call clgstr ("catdb", Memc[str2], SZ_FNAME)
        verbose = clgetb ("verbose")

        # Get the catalog list.
        catlist = at_catlist (Memc[str1],  Memc[str2])
        if (fntlenb (catlist) <= 0) {
	    if (verbose)
                call printf ("The catalog list is empty\n")
            call fntclsb (catlist)
            call sfree (sp)
            return
        }

        # Open the catalog database.
        cq = cq_map (Memc[str2], READ_ONLY)
	if (verbose) {
            call printf ("\nScanning catalog database %s\n")
                call pargstr (Memc[str2])
	}

	# Loop over the catalogs.
	if (verbose)
	    call printf ("Listing the supported catalogs\n")
        do i = 1, fntlenb (catlist) {

            # Get the catalog name and set the current catalog.
            if (fntrfnb (catlist, i, Memc[str1], SZ_FNAME) == EOF)
                break
            if (cq_setcat (cq, Memc[str1]) <= 0) {
                next
            } else {
                call printf ("%s\n")
                    call pargstr (Memc[str1])
            }

	    # Do a detailed listing.
	    if (verbose) {
		iferr (nquery = cq_fgeti (cq, "nquery"))
		    nquery = 0
		call printf ("nquery %d\n")
		    call pargi (nquery)
		if (nquery > 0) {
		    do j = 1, nquery {
			if (cq_scan (cq) == EOF)
			    break
			call gargstr (Memc[line], SZ_LINE)
			call printf ("%s\n")
			    call pargstr (Memc[line])
		    }
		}
		iferr (nheader = cq_fgeti (cq, "nheader"))
		    nheader = 0
		call printf ("nheader %d\n")
		    call pargi (nheader)
		if (nheader > 0) {
		    do j = 1, nheader {
			if (cq_scan (cq) == EOF)
			    break
			call gargstr (Memc[line], SZ_LINE)
			call printf ("%s\n")
			    call pargstr (Memc[line])
		    }
		}
		iferr (nfields = cq_fgeti (cq, "nfields"))
		    nfields = 0
		call printf ("nfields %d\n")
		    call pargi (nfields)
		if (nfields > 0) {
		    do j = 1, nfields {
			if (cq_scan (cq) == EOF)
			    break
			call gargstr (Memc[line], SZ_LINE)
			call printf ("%s\n")
			    call pargstr (Memc[line])
		    }
		}
		if (nquery > 0 || nheader > 0 || nfields > 0)
		    call printf ("\n")
	    }
	}


        # Close the catalog database.
        call cq_unmap (cq)

        # Close the catalog list.
        call fntclsb (catlist)

	# Free working memory.
	call sfree (sp)
end
