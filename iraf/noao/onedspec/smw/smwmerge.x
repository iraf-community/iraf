include	<mwset.h>
include	<smw.h>


# SMW_MERGE -- Merge split MWCS array to a single MWCS.

procedure smw_merge (smw)

pointer	smw			#U Input split WCS, output single WCS

size_t	sz_val
int	pdim, format, beam, dtype, dtype1, ii
long	ap, naps, nw, nw1, i, c_1
int	axes[3]
double	w1, dw, z, w11, dw1, z1
real	aplow[2], aphigh[2]
pointer	sp, key, val, term, coeff, mw, mw1
pointer	mw_open()
data	axes/1,2,3/

include	<nullptr.inc>

begin
	c_1 = 1

	if (SMW_NMW(smw) == 1)
	    return

	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (key, sz_val, TY_CHAR)
	sz_val = SZ_LINE
	call salloc (val, sz_val, TY_CHAR)
	sz_val = 15
	call salloc (term, sz_val, TY_DOUBLE)
	coeff = NULL

	pdim = SMW_PDIM(smw)
	naps = SMW_NSPEC(smw)
	mw1 = SMW_MW(smw,0)

	# Determine output WCS format.
	format = SMW_ES
	do i = 1, naps {
	    call smw_gwattrs (smw, i, c_1, ap, beam, dtype, w1, dw, nw, z,
		aplow, aphigh, coeff)
	    if (i == 1) {
		dtype1 = dtype
		w11 = w1
		dw1 = dw
		z1 = z
		nw1 = nw
	    }
	    if (dtype>1||dtype!=dtype1||w1!=w11||dw!=dw1||nw!=nw1||z!=z1) {
		format = SMW_MS
		break
	    }
	}

	# Setup WCS.
	switch (format) {
	case SMW_ES:
	    mw = mw_open (NULLPTR, pdim)
	    call mw_newsystem (mw, "equispec", pdim)
	    call mw_swtype (mw, axes, pdim, "linear", "")

	case SMW_MS:
	    mw = mw_open (NULLPTR, pdim)
	    call mw_newsystem (mw, "multispec", pdim)
	    call mw_swtype (mw, axes, pdim, "multispec", "")
	    if (pdim > 2)
		call mw_swtype (mw, 3, 1, "linear", "")
	}

	ifnoerr (call mw_gwattrs (mw1, 1, "label", Memc[val], SZ_LINE))
	    call mw_swattrs (mw, 1, "label", Memc[val])
	ifnoerr (call mw_gwattrs (mw1, 1, "units", Memc[val], SZ_LINE))
	    call mw_swattrs (mw, 1, "units", Memc[val])
	ifnoerr (call mw_gwattrs (mw1, 1, "units_display", Memc[val], SZ_LINE))
	    call mw_swattrs (mw, 1, "units_display", Memc[val])
	call mw_gltermd (mw1, Memd[term+pdim], Memd[term], pdim)
	call mw_sltermd (mw, Memd[term+pdim], Memd[term], pdim)
	call mw_gwtermd (mw1, Memd[term], Memd[term+pdim],
	    Memd[term+2*pdim], pdim)
	Memd[term] = 1.
	Memd[term+pdim] = w1 / (1 + z)
	Memd[term+2*pdim] = dw / (1 + z)
	call mw_swtermd (mw, Memd[term], Memd[term+pdim],
	    Memd[term+2*pdim], pdim)

	# Set the SMW structure.
	call smw_open (mw, smw, NULLPTR)
	if (format == SMW_MS) {
	    do ii = 1, SMW_NMW(mw) - 1
		call mw_close (SMW_MW(mw,ii))
	    SMW_NMW(mw) = 1
	}
	do i = 1, naps {
	    call smw_gwattrs (smw, i, c_1, ap, beam, dtype, w1, dw, nw, z,
		aplow, aphigh, coeff)
	    call smw_swattrs (mw, i, c_1, ap, beam, dtype, w1, dw, nw, z,
		aplow, aphigh, Memc[coeff])
	    call smw_gapid (smw, i, c_1, Memc[val], SZ_LINE)
	    call smw_sapid (mw, i, c_1, Memc[val])
	}

	call smw_close (smw)
	smw = mw

	call mfree (coeff, TY_CHAR)
	call sfree (sp)
end
