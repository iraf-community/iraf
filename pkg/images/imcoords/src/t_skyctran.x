include <fset.h>
include <pkg/skywcs.h>

procedure t_skyctran()

bool	verbose, transform, first_file
int	inlist, outlist, linlist, loutlist, lngcolumn, latcolumn, infd, outfd
int	ilngunits, ilatunits, olngunits, olatunits, min_sigdigits, optype
int	instat, outstat, nilng, nilat, plngcolumn, platcolumn, pxcolumn
int	rvcolumn
double	ilngmin, ilngmax, ilatmin, ilatmax
int	fstati()
pointer	sp, inname, outname, insystem, outsystem, olngformat, olatformat
pointer	ilngformat, ilatformat, str, mwin, mwout, cooin, cooout

bool	clgetb(), streq()
double	clgetd()
int	clpopnu(), clplen(), clgfil(), open(), sk_decwcs()
int	clgeti(), clgwrd(), sk_stati()
errchk	clgwrd()

begin
	call smark (sp)
	call salloc (inname, SZ_FNAME, TY_CHAR)
	call salloc (outname, SZ_FNAME, TY_CHAR)
	call salloc (insystem, SZ_FNAME, TY_CHAR)
	call salloc (outsystem, SZ_FNAME, TY_CHAR)
	call salloc (ilngformat, SZ_FNAME, TY_CHAR)
	call salloc (ilatformat, SZ_FNAME, TY_CHAR)
	call salloc (olngformat, SZ_FNAME, TY_CHAR)
	call salloc (olatformat, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Open the input and output file lists.
	inlist = clpopnu ("input")
	linlist = clplen (inlist)
	outlist = clpopnu ("output")
	loutlist = clplen (outlist)
	call clgstr ("insystem", Memc[insystem], SZ_FNAME)
	call clgstr ("outsystem", Memc[outsystem], SZ_FNAME)
	transform = clgetb ("transform")

	# Fetch the file formatting parameters.
	lngcolumn = clgeti ("lngcolumn")
	latcolumn = clgeti ("latcolumn")
	plngcolumn = clgeti ("plngcolumn")
	platcolumn = clgeti ("platcolumn")
	pxcolumn = clgeti ("pxcolumn")
	rvcolumn = clgeti ("rvcolumn")
	ilngmin = clgetd ("ilngmin")
	ilngmax = clgetd ("ilngmax")
	ilatmin = clgetd ("ilatmin")
	ilatmax = clgetd ("ilatmax")
	nilng = clgeti ("nilng")
	nilat = clgeti ("nilat")
	iferr (ilngunits = clgwrd ("ilngunits", Memc[str], SZ_FNAME,
	    SKY_LNG_UNITLIST))
	    ilngunits = 0
	iferr (ilatunits = clgwrd ("ilatunits", Memc[str], SZ_FNAME,
	    SKY_LAT_UNITLIST))
	    ilatunits = 0
	call clgstr ("ilngformat", Memc[ilngformat], SZ_FNAME)
	call clgstr ("ilatformat", Memc[ilatformat], SZ_FNAME)

	iferr (olngunits = clgwrd ("olngunits", Memc[str], SZ_FNAME,
	    SKY_LNG_UNITLIST))
	    olngunits = 0
	iferr (olatunits = clgwrd ("olatunits", Memc[str], SZ_FNAME,
	    SKY_LAT_UNITLIST))
	    olatunits = 0
	call clgstr ("olngformat", Memc[olngformat], SZ_FNAME)
	call clgstr ("olatformat", Memc[olatformat], SZ_FNAME)
	#min_sigdigits = clgeti ("min_sigdigits")
	min_sigdigits = 7
	verbose = clgetb ("verbose")

	# Test the length of the input coordinate list.
	if (linlist < 1)
	    call error (0, "The input coordinate file list is empty")
	if (loutlist < 1)
	    call error (0, "The output coordinate file list is empty")
	if (loutlist > 1 && loutlist != linlist)
	    call error (0,
	        "The number of input and output files are not the same")

	# Determine the input coordinate system.
	instat = sk_decwcs (Memc[insystem], mwin, cooin, NULL)

	# Determine the output coordinate system.
	outstat = sk_decwcs (Memc[outsystem], mwout, cooout, NULL)

	# Loop over the input files.
	first_file = true
	while (clgfil (inlist, Memc[inname], SZ_FNAME) != EOF) {

	    # Open the input coordinate file. The string "imcursor" is
	    # reserved for the image display cursor.
	    if (streq (Memc[inname], "imcursor") && mwin != NULL) {
		infd = NULL
		optype = sk_stati (cooin, S_PIXTYPE)
		call sk_seti (cooin, S_PIXTYPE, PIXTYPE_TV)
	    } else if (streq (Memc[inname], "grid")) {
		optype = sk_stati (cooin, S_PIXTYPE)
		infd = NULL
	    } else
		infd = open (Memc[inname], READ_ONLY, TEXT_FILE)

	    # Open the output coordinate file.
	    if (clgfil (outlist, Memc[outname], SZ_FNAME) != EOF) {
		outfd = open (Memc[outname], NEW_FILE, TEXT_FILE)
		if (streq (Memc[outname], "STDOUT") || outfd == STDOUT)
		    call fseti (outfd, F_FLUSHNL, YES)
		call fprintf (outfd, "\n")
		if (instat == ERR)
		    call fprintf (outfd,
			"# Error decoding the input coordinate system\n")
		call sk_iiwrite (outfd, "Insystem", Memc[insystem], mwin,
	            cooin)
		if (outstat == ERR)
		    call fprintf (outfd,
		        "# Error decoding the output coordinate system\n")
	        call sk_iiwrite (outfd, "Outsystem", Memc[outsystem], mwout,
	            cooout)
	    } 

	    # Print information about the input and output coordinate system
	    # and the input and output files to the standard output.
	    if (verbose && outfd != STDOUT) {
		if (first_file) {
		    call printf ("\n")
	            if (instat == ERR)
		        call printf (
		            "Error decoding the input coordinate system\n")
	            call sk_iiprint ("Insystem", Memc[insystem], mwin, cooin)
	            if (outstat == ERR)
		        call printf (
		            "Error decoding the output coordinate system\n")
	            call sk_iiprint ("Outsystem", Memc[outsystem], mwout,
		        cooout)
		    call printf ("\n")
		}
		call printf ("Input file: %s  Output file: %s\n")
		    call pargstr (Memc[inname])
		    call pargstr (Memc[outname])
		call flush (STDOUT)
	    }


	    # Print the input and output file name banner.
	    call fprintf (outfd, "\n# Input file: %s  Output file: %s\n")
		call pargstr (Memc[inname])
		call pargstr (Memc[outname])
	    call fprintf (outfd, "\n")

	    # Transform the coordinate list.
	    if (infd == NULL) {
		if (streq ("imcursor", Memc[inname]))
		    call sk_curtran (outfd, mwin, mwout, cooin, cooout,
		        olngunits, olatunits, Memc[olngformat],
			Memc[olatformat], transform)
		else if (instat == ERR || outstat == ERR)
		    call sk_grcopy (outfd, cooin, cooout, ilngmin, ilngmax,
		        nilng, ilatmin, ilatmax, nilat, ilngunits,
		        ilatunits, olngunits, olatunits, Memc[ilngformat],
		        Memc[ilatformat], Memc[olngformat],
		        Memc[olatformat], transform) 
		else
		    call sk_grtran (outfd, mwin, mwout, cooin, cooout,
		        ilngmin, ilngmax, nilng, ilatmin, ilatmax, nilat,
		        ilngunits, ilatunits, olngunits, olatunits,
			Memc[ilngformat], Memc[ilatformat], Memc[olngformat],
			Memc[olatformat], transform)
	    } else {
		if (infd == STDIN && fstati(STDIN, F_REDIR) == NO)
	            call sk_ttytran (infd, outfd, mwin, mwout, cooin, cooout,
	                ilngunits, ilatunits, olngunits, olatunits,
			Memc[olngformat], Memc[olatformat])
		else if (instat == ERR || outstat == ERR)
		    call sk_copytran (infd, outfd, lngcolumn, latcolumn,
		        transform)
		else
	            call sk_listran (infd, outfd, mwin, mwout, cooin, cooout,
	                lngcolumn, latcolumn, plngcolumn, platcolumn,
			pxcolumn, rvcolumn, ilngunits, ilatunits, olngunits,
		        olatunits, Memc[olngformat], Memc[olatformat],
		        min_sigdigits, transform)
	    }

	    # Close the output coordinate file.
	    if (linlist == loutlist)
	        call close (outfd)

	    # Close the input coordinate file.
	    if (infd != NULL)
	        call close (infd)
	    else
		call sk_seti (cooin, S_PIXTYPE, optype)

	    first_file = false
	}

	# Close the image wcs if one was opened.
	if (loutlist < linlist)
	    call close (outfd)
	if (mwin != NULL)
	    call mw_close (mwin)
	if (mwout != NULL)
	    call mw_close (mwout)
	#call mfree (cooin, TY_STRUCT)
	call sk_close (cooin)
	#call mfree (cooout, TY_STRUCT)
	call sk_close (cooout)

	# Close up the lists.
	call clpcls (inlist)
	call clpcls (outlist)

	call sfree (sp)
end


