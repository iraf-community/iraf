include "linmatch.h"

# RG_LSHOW -- Print the LINMATCH task parameters.

procedure rg_lshow (ls)

pointer	ls		#I pointer to linmatch structure

pointer	sp, str1, str2
int	rg_lstati()
real	rg_lstatr()

begin
	call smark (sp)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)

	call printf ("\nIntensity Matching Parameters\n")
	if (rg_lstati (ls, BSALGORITHM) != LS_PHOTOMETRY &&  rg_lstati(ls,
	    BZALGORITHM) != LS_PHOTOMETRY) {
	    call rg_lstats (ls, IMAGE, Memc[str1], SZ_FNAME)
	    call printf ("    %s: %s")
	        call pargstr (KY_IMAGE)
	        call pargstr (Memc[str1])
	    call rg_lstats (ls, REFIMAGE, Memc[str1], SZ_FNAME)
	    call printf ("  %s: %s\n")
	        call pargstr (KY_REFIMAGE)
	        call pargstr (Memc[str1])
	    call rg_lstats (ls, REGIONS, Memc[str1], SZ_FNAME)
	    call printf ("    %s: %s\n")
	        call pargstr (KY_REGIONS)
	        call pargstr (Memc[str1])
	    call rg_lstats (ls, CCDGAIN, Memc[str1], SZ_LINE)
	    call rg_lstats (ls, CCDREAD, Memc[str2], SZ_LINE)
	    call printf ("    %s: %s  %s: %s\n")
	        call pargstr (KY_GAIN)
	        call pargstr (Memc[str1])
	        call pargstr (KY_READNOISE)
	        call pargstr (Memc[str2])
	} else {
	    call rg_lstats (ls, IMAGE, Memc[str1], SZ_FNAME)
	    call printf ("    %s: %s\n")
	        call pargstr (KY_IMAGE)
	        call pargstr (Memc[str1])
	    call rg_lstats (ls, PHOTFILE, Memc[str1], SZ_FNAME)
	    call printf ("    %s: %s")
		call pargstr (KY_IMAGE)
	        call pargstr (Memc[str1])
	    call rg_lstats (ls, REFIMAGE, Memc[str1], SZ_FNAME)
	    call printf ("  %s: %s\n")
		call pargstr (KY_REFIMAGE)
	        call pargstr (Memc[str1])
	}
	call rg_lstats (ls, SHIFTSFILE, Memc[str1], SZ_FNAME)
	if (Memc[str1] != EOS) {
	    call printf ("    %s: %s\n")
	        call pargstr (KY_SHIFTSFILE)
	        call pargstr (Memc[str1])
	} else {
	    call printf ("    %s: %g  %s: %g\n")
	        call pargstr (KY_XSHIFT)
	        call pargr (rg_lstatr(ls,XSHIFT))
	        call pargstr (KY_YSHIFT)
	        call pargr (rg_lstatr(ls,YSHIFT))
	}
	call printf ("    %s: %d  %s: %d\n")
	    call pargstr (KY_DNX)
	    call pargi (rg_lstati(ls,DNX))
	    call pargstr (KY_DNY)
	    call pargi (rg_lstati(ls,DNY))

	call rg_lstats (ls, DATABASE, Memc[str1], SZ_FNAME)
	call printf ("    %s: %s")
	    call pargstr (KY_DATABASE)
	    call pargstr (Memc[str1])
	call rg_lstats (ls, OUTIMAGE, Memc[str1], SZ_FNAME)
	call printf ("  %s: %s\n")
	    call pargstr (KY_OUTIMAGE)
	    call pargstr (Memc[str1])

	call rg_lstats (ls, BSSTRING, Memc[str1], SZ_LINE)
	call rg_lstats (ls, BZSTRING, Memc[str2], SZ_LINE)
	call printf ("    %s:  %s %s\n")
	    call pargstr ("scaling")
	    call pargstr (Memc[str1])
	    call pargstr (Memc[str2])
	call printf ("    %s = %g   %s = %g")
	    call pargstr (KY_DATAMIN)
	    call pargr (rg_lstatr (ls, DATAMIN))
	    call pargstr (KY_DATAMAX)
	    call pargr (rg_lstatr (ls, DATAMAX))
	call printf ("  %s: %d\n")
	    call pargstr (KY_MAXITER)
	    call pargi (rg_lstati(ls,MAXITER))
	call printf ("    %s: %d")
	    call pargstr (KY_NREJECT)
	    call pargi (rg_lstati(ls,NREJECT))
	call printf ("  %s = %g   %s = %g\n")
	    call pargstr (KY_LOREJECT)
	    call pargr (rg_lstatr (ls, LOREJECT))
	    call pargstr (KY_HIREJECT)
	    call pargr (rg_lstatr (ls, HIREJECT))

	call printf ("\n")

	call sfree (sp)
end
