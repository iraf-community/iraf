include "psfmatch.h"

# RG_PSHOW -- Print the PSFMATCH task parameters.

procedure rg_pshow (pm)

pointer	pm		#I pointer to psfmatch structure

pointer	sp, str
bool	itob()
int	rg_pstati()
real	rg_pstatr()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	call rg_pstats (pm, CSTRING, Memc[str], SZ_FNAME)
	call printf ("\nConvolution: %s\n")
	    call pargstr (Memc[str])
	if (rg_pstati (pm, CONVOLUTION) == PM_CONIMAGE) {
	    call rg_pstats (pm, IMAGE, Memc[str], SZ_FNAME)
	    call printf ("    %s: %s\n")
	        call pargstr (KY_IMAGE)
	        call pargstr (Memc[str])
	    call rg_pstats (pm, REFIMAGE, Memc[str], SZ_FNAME)
	    call printf ("    %s: %s\n")
	        call pargstr (KY_REFIMAGE)
	        call pargstr (Memc[str])
	    call rg_pstats (pm, PSFDATA, Memc[str], SZ_FNAME)
	    call printf ("    %s: %s\n")
	        call pargstr (KY_PSFDATA)
	        call pargstr (Memc[str])
	} else if (rg_pstati (pm, CONVOLUTION) == PM_CONPSF) {
	    call rg_pstats (pm, IMAGE, Memc[str], SZ_FNAME)
	    call printf ("    %s: %s\n")
	        call pargstr (KY_IMAGE)
	        call pargstr (Memc[str])
	    call rg_pstats (pm, PSFIMAGE, Memc[str], SZ_FNAME)
	    call printf ("    input psf: %s\n")
	        call pargstr (Memc[str])
	    call rg_pstats (pm, REFIMAGE, Memc[str], SZ_FNAME)
	    call printf ("    reference psf: %s\n")
	        call pargstr (Memc[str])
	} else {
	    call rg_pstats (pm, IMAGE, Memc[str], SZ_FNAME)
	    call printf ("    %s: %s\n")
	        call pargstr (KY_IMAGE)
	        call pargstr (Memc[str])
	}

	call rg_pstats (pm, KERNEL, Memc[str], SZ_FNAME)
	call printf ("    %s: %s\n")
	    call pargstr (KY_KERNEL)
	    call pargstr (Memc[str])
	call rg_pstats (pm, OUTIMAGE, Memc[str], SZ_FNAME)
	if (Memc[str] != EOS) {
	    call printf ("    %s: %s\n")
	        call pargstr (KY_OUTIMAGE)
	        call pargstr (Memc[str])
	}

	call printf ("Centering and background fitting\n")
	call printf ("    %s: %b\n")
	    call pargstr (KY_CENTER)
	    call pargb (itob(rg_pstati(pm,CENTER)))
	call rg_pstats (pm, BSTRING, Memc[str], SZ_LINE)
	call printf ("    %s:  %s\n")
	    call pargstr (KY_BACKGRD)
	    call pargstr (Memc[str])
	call printf ("    %s = %g   %s = %g\n")
	    call pargstr (KY_LOREJECT)
	    call pargr (rg_pstatr (pm, LOREJECT))
	    call pargstr (KY_HIREJECT)
	    call pargr (rg_pstatr (pm, HIREJECT))
	call printf ("    %s = %g\n")
	    call pargstr (KY_APODIZE)
	    call pargr (rg_pstatr (pm, APODIZE))

	call printf ("Filtering:\n")
	call rg_pstats (pm, FSTRING, Memc[str], SZ_LINE)
	call printf ("    %s:  %s\n")
	    call pargstr (KY_FILTER)
	    call pargstr (Memc[str])
	if (rg_pstati(pm,FILTER) == PM_FCOSBELL) {
	    call printf ("    %s:  %g    %s: %g\n")
	        call pargstr (KY_SXINNER)
	        call pargr (rg_pstatr (pm, SXINNER))
	        call pargstr (KY_SXOUTER)
	        call pargr (rg_pstatr (pm, SXOUTER))
	    call printf ("    %s:  %g    %s: %g\n")
	        call pargstr (KY_SYINNER)
	        call pargr (rg_pstatr (pm, SYINNER))
	        call pargstr (KY_SYOUTER)
	        call pargr (rg_pstatr (pm, SYOUTER))
	    call printf ("    %s: %b\n")
	        call pargstr (KY_RADSYM)
	        call pargb (itob(rg_pstati(pm,RADSYM)))
	} else {
	    call printf ("    %s: %g\n")
	        call pargstr (KY_UFLUXRATIO)
	        call pargr (rg_pstatr (pm, UFLUXRATIO))
	    call printf ("    %s: %g\n")
	        call pargstr (KY_THRESHOLD)
	        call pargr (rg_pstatr(pm,THRESHOLD))
	}

	call printf ("Normalization\n")
	call printf ("    %s: %g\n")
	    call pargstr (KY_NORMFACTOR)
	    call pargr (rg_pstatr (pm, NORMFACTOR))

	call printf ("\n")

	call sfree (sp)
end
