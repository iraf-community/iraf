include "xregister.h"

# RG_XSHOW -- Show the XREGISTER parameters.

procedure rg_xshow (xc)

pointer	xc		#I pointer to the main xregister structure

begin
	call rg_xnshow (xc)
	call printf ("\n")
	call rg_xbshow (xc)
	call printf ("\n")
	call rg_xxshow (xc)
	call printf ("\n")
	call rg_xpshow (xc)
end


# RG_XNSHOW -- Show the input/output data XREGISTER parameters.

procedure rg_xnshow (xc)

pointer	xc	#I pointer to the main xregister structure

pointer	sp, str
int	rg_xstati()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Set the object characteristics.
	call printf ("\nInput/output data\n")
	call rg_xstats (xc, IMAGE, Memc[str], SZ_FNAME)
	call printf ("    %s: %s\n")
	    call pargstr (KY_IMAGE)
	    call pargstr (Memc[str])
	call rg_xstats (xc, REFIMAGE, Memc[str], SZ_FNAME)
	call printf ("    %s: %s\n")
	    call pargstr (KY_REFIMAGE)
	    call pargstr (Memc[str])
	call rg_xstats (xc, REGIONS, Memc[str], SZ_FNAME)
	call printf ("    %s: %s\n")
	    call pargstr (KY_REGIONS)
	    call pargstr (Memc[str])
	call printf ("    %s = %d    %s = %d\n")
	    call pargstr (KY_XLAG)
	    call pargi (rg_xstati (xc, XLAG))
	    call pargstr (KY_YLAG)
	    call pargi (rg_xstati (xc, YLAG))
	call printf ("    %s = %d    %s = %d\n")
	    call pargstr (KY_DXLAG)
	    call pargi (rg_xstati (xc, DXLAG))
	    call pargstr (KY_DYLAG)
	    call pargi (rg_xstati (xc, DYLAG))
	call rg_xstats (xc, DATABASE, Memc[str], SZ_FNAME)
	call printf ("    %s: %s\n")
	    call pargstr (KY_DATABASE)
	    call pargstr (Memc[str])
	call rg_xstats (xc, RECORD, Memc[str], SZ_FNAME)
	call printf ("    %s: %s\n")
	    call pargstr (KY_RECORD)
	    call pargstr (Memc[str])
	call rg_xstats (xc, REFFILE, Memc[str], SZ_FNAME)
	call printf ("    %s: %s\n")
	    call pargstr (KY_REFFILE)
	    call pargstr (Memc[str])
	call rg_xstats (xc, OUTIMAGE, Memc[str], SZ_FNAME)
	call printf ("    %s: %s\n")
	    call pargstr (KY_OUTIMAGE)
	    call pargstr (Memc[str])

	call sfree (sp)
end


# RG_XBSHOW -- Show the background fitting parameters.

procedure rg_xbshow (xc)

pointer	xc		#I pointer to the main xregister structure

int	back
pointer	sp, str
int	rg_xstati()
real	rg_xstatr()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	back = rg_xstati (xc, BACKGRD)
	call printf ("Background fitting parameters:\n")
	call rg_xstats (xc, BSTRING, Memc[str], SZ_LINE)
	call printf ("    %s:  %s\n")
	    call pargstr (KY_BACKGROUND)
	    call pargstr (Memc[str])
	call printf ("    %s = %d\n")
	    call pargstr (KY_BORDER)
	    call pargi (rg_xstati (xc, BORDER))
	call printf ("    %s = %g   %s = %g\n")
	    call pargstr (KY_LOREJECT)
	    call pargr (rg_xstatr (xc, LOREJECT))
	    call pargstr (KY_HIREJECT)
	    call pargr (rg_xstatr (xc, HIREJECT))
	call printf ("    %s = %g\n")
	    call pargstr (KY_APODIZE)
	    call pargr (rg_xstatr (xc, APODIZE))
	call rg_xstats (xc, FSTRING, Memc[str], SZ_LINE)
	call printf ("    %s:  %s\n")
	    call pargstr (KY_FILTER)
	    call pargstr (Memc[str])

	call sfree (sp)
end


# RG_XXSHOW -- Show the cross-correlation function parameters.

procedure rg_xxshow (xc)

pointer	xc		#I pointer to the main xregister structure

pointer	sp, str
int	rg_xstati()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	call printf ("Cross correlation function:\n")
	call rg_xstats (xc, CSTRING, Memc[str], SZ_LINE)
	call printf ("    %s:  %s\n")
	    call pargstr (KY_CORRELATION)
	    call pargstr (Memc[str])
	call printf ("    %s = %d    %s = %d\n")
	    call pargstr (KY_XWINDOW)
	    call pargi (rg_xstati (xc, XWINDOW))
	    call pargstr (KY_YWINDOW)
	    call pargi (rg_xstati (xc, YWINDOW))

	call sfree (sp)
end


# RG_XPSHOW -- Show the peak centering parameters.

procedure rg_xpshow (xc)

pointer	xc		#I pointer to the main xregister structure

pointer	sp, str
int	rg_xstati()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	call printf ("Peak centering parameters:\n")
	call rg_xstats (xc, PSTRING, Memc[str], SZ_LINE)
	call printf ("    %s:  %s\n")
	    call pargstr (KY_PEAKCENTER)
	    call pargstr (Memc[str])
	call printf ("    %s = %d    %s = %d\n")
	    call pargstr (KY_XCBOX)
	    call pargi (rg_xstati (xc, XCBOX))
	    call pargstr (KY_YCBOX)
	    call pargi (rg_xstati (xc, YCBOX))

	call sfree (sp)
end
