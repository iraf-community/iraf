# T_ASLIST -- List the support image surveys.

procedure t_aslist() 

pointer	sp, str1, str2, line, cq
int	i, j, svlist, nwcs, nkeys
bool	verbose
pointer	cq_map()
int	at_svlist(), fntlenb(), fntrfnb(), cq_setcat(), cq_fgeti(), cq_scan()
bool	clgetb()
errchk	cq_fgstr(), cq_fgeti()

begin
        # Allocate some working memory.
        call smark (sp)
        call salloc (str1, SZ_FNAME, TY_CHAR)
        call salloc (str2, SZ_FNAME, TY_CHAR)
        call salloc (line, SZ_LINE, TY_CHAR)

	# Get the parameters.
        call clgstr ("imsurveys", Memc[str1], SZ_FNAME)
        call clgstr ("imdb", Memc[str2], SZ_FNAME)
        verbose = clgetb ("verbose")

        # Get the catalog list.
        svlist = at_svlist (Memc[str1],  Memc[str2])
        if (fntlenb (svlist) <= 0) {
	    if (verbose)
                call printf ("The image surveys list is empty\n")
            call fntclsb (svlist)
            call sfree (sp)
            return
        }

        # Open the catalog database.
        cq = cq_map (Memc[str2], READ_ONLY)
	if (verbose) {
            call printf ("\nScanning image surveys database %s\n")
                call pargstr (Memc[str2])
	}

	# Loop over the catalogs.
	if (verbose)
	    call printf ("Listing the supported image surveys\n")
        do i = 1, fntlenb (svlist) {

            # Get the catalog name and set the current catalog.
            if (fntrfnb (svlist, i, Memc[str1], SZ_FNAME) == EOF)
                break
            if (cq_setcat (cq, Memc[str1]) <= 0) {
                next
            } else {
                call printf ("%s\n")
                    call pargstr (Memc[str1])
            }

	    # Do a detailed listing.
	    if (verbose) {
		iferr (call cq_fgstr (cq, "wcs", Memc[line], SZ_LINE))
		    call strcpy ("none", Memc[line], SZ_LINE)
		call printf ("wcs %s\n")
		    call pargstr (Memc[line])
		iferr (nwcs = cq_fgeti (cq, "nwcs"))
		    nwcs = 0
		call printf ("nwcs %d\n")
		    call pargi (nwcs)
		if (nwcs > 0) {
		    do j = 1, nwcs {
			if (cq_scan (cq) == EOF)
			    break
			call gargstr (Memc[line], SZ_LINE)
			call printf ("%s\n")
			    call pargstr (Memc[line])
		    }
		}
		iferr (nkeys = cq_fgeti (cq, "nkeys"))
		    nkeys = 0
		call printf ("nkeys %d\n")
		    call pargi (nkeys)
		if (nkeys > 0) {
		    do j = 1, nkeys {
			if (cq_scan (cq) == EOF)
			    break
			call gargstr (Memc[line], SZ_LINE)
			call printf ("%s\n")
			    call pargstr (Memc[line])
		    }
		}
		if (nwcs > 0 || nkeys > 0)
		    call printf ("\n")
	    }
	}

        # Close the image surveys database.
        call cq_unmap (cq)

        # Close the image surveys list.
        call fntclsb (svlist)

	# Free working memory.
	call sfree (sp)
end
