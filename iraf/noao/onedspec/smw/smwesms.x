include	<mwset.h>
include	<smw.h>


# SMW_ESMS -- Convert EQUISPEC WCS into MULTISPEC WCS.
# This is called by SMW_SWATTRS when the equal linear coordinate system
# requirement of the EQUISPEC WCS is violated.

procedure smw_esms (smw)

pointer	smw			#U SMW pointer

size_t	sz_val
long	i, j, k, ap, nw, c_1
int	pdim1, pdim2, beam, dtype, axes[2]
double	w1, dw, z
real	aplow, aphigh
pointer	sp, key, str, lterm, mw, mw1, mw2, apid
pointer	mw_open()
data	axes/1,2/

include	<nullptr.inc>

begin
	c_1 = 1

	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (key, sz_val, TY_CHAR)
	sz_val = SZ_LINE
	call salloc (str, sz_val, TY_CHAR)
	sz_val = 12
	call salloc (lterm, sz_val, TY_DOUBLE)

	# Set the basic MWCS
	mw1 = SMW_MW(smw,0)
	pdim1 = SMW_PDIM(smw)
	pdim2 = max (2, pdim1)
	mw2 = mw_open (NULLPTR, pdim2)
	call mw_newsystem (mw2, "multispec", pdim2)
	call mw_swtype (mw2, axes, 2, "multispec", "")
	if (pdim2 > 2)
	    call mw_swtype (mw2, 3, 1, "linear", "")
	call mw_gltermd (mw1, Memd[lterm+pdim2], Memd[lterm], pdim1)
	if (pdim1 == 1) {
	    Memd[lterm+1] = 0.
	    Memd[lterm+3] = 0.
	    Memd[lterm+4] = 0.
	    Memd[lterm+5] = 1.
	}
	call mw_sltermd (mw2, Memd[lterm+pdim2], Memd[lterm], pdim2)
	ifnoerr (call mw_gwattrs (mw1, 1, "label", Memc[str], SZ_LINE))
	    call mw_swattrs (mw2, 1, "label", Memc[str])
	ifnoerr (call mw_gwattrs (mw1, 1, "units", Memc[str], SZ_LINE))
	    call mw_swattrs (mw2, 1, "units", Memc[str])
	ifnoerr (call mw_gwattrs (mw1, 1, "units_display", Memc[str], SZ_LINE))
	    call mw_swattrs (mw2, 1, "units_display", Memc[str])

	 # Write the MULTISPEC WCS
	dtype = SMW_DTYPE(smw)
	w1 = SMW_W1(smw)
	dw = SMW_DW(smw)
	nw = SMW_NW(smw)
	z = SMW_Z(smw)
	if (dtype == DCLOG) {
	    dw = log10 ((w1+(nw-1)*dw)/w1)/(nw-1)
	    w1 = log10 (w1)
	}

         call smw_open (mw2, smw, NULLPTR)
         do i = 1, SMW_NSPEC(smw) {
	    ap = Meml[SMW_APS(smw)+i-1]
	    beam = Memi[SMW_BEAMS(smw)+i-1]
	    aplow = Memr[SMW_APLOW(smw)+2*i-2]
	    aphigh = Memr[SMW_APHIGH(smw)+2*i-2]
	    apid = Memp[SMW_APIDS(smw)+i-1]
	    
	    call smw_mw (mw2, i, c_1, mw, j, k)
	    call sprintf (Memc[key], SZ_FNAME, "spec%d")
		call pargl (j)
	    call sprintf (Memc[str], SZ_LINE,
		"%d %d %d %.14g %.14g %d %.14g %.2f %.2f")
		call pargl (ap)
		call pargi (beam)
		call pargi (dtype)
		call pargd (w1)
		call pargd (dw)
		call pargl (nw)
		call pargd (z)
		call pargr (aplow)
		call pargr (aphigh)
	    call mw_swattrs (mw, 2, Memc[key], Memc[str])

	    if (apid != NULL)
		call smw_sapid (mw2, i, c_1, Memc[apid])

	    # This is used if there is a split MULTISPEC WCS.
	    if (SMW_APS(mw2) != NULL)
		Meml[SMW_APS(mw2)+i-1] = ap
	}

	call smw_close (smw)
	smw = mw2

	call sfree (sp)
end
