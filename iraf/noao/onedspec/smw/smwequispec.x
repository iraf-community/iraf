include <imhdr.h>
include	<mwset.h>
include	<smw.h>


# SMW_EQUISPEC -- Setup the EQUISPEC SMW parameters.
# The aperture information is in the APNUM and APID keywords and the
# WCS information is in the linear MWCS.

procedure smw_equispec (im, smw)

pointer	im			#I IMIO pointer
pointer	smw			#U MWCS pointer input SMW pointer output

size_t	sz_val
long	nw, ap, i, j, k, c_1
int	nchar, beam, dtype, ii, ik
double	w1, dw, z
real	aplow[2], aphigh[2]
pointer	sp, key, val, wterm, mw, ct
int	imgeti(), mw_stati(), ctoi(), ctol(), ctor()
pointer	mw_sctran()
real	mw_c1tranr()
errchk	imgstr, mw_gwtermd, mw_sctran
errchk	smw_open, smw_saxes, smw_swattrs, smw_sapid

include	<nullptr.inc>

begin
	c_1 = 1

	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (key, sz_val, TY_CHAR)
	sz_val = SZ_LINE
	call salloc (val, sz_val, TY_CHAR)

	# Determine the dispersion parameters
	mw = smw
	ii = mw_stati (mw, MW_NDIM)
	sz_val = 2*ii+ii*ii
	call salloc (wterm, sz_val, TY_DOUBLE)
	call mw_gwtermd (mw, Memd[wterm], Memd[wterm+ii], Memd[wterm+2*ii], ii)
	w1 = Memd[wterm+ii]
	dw = Memd[wterm+2*ii]

	# Determine the number of physical pixels.
	ct = mw_sctran (mw, "logical", "physical", 1)
	nw = max (mw_c1tranr (ct, 1.), mw_c1tranr (ct, real(IM_LEN(im,1))))
	call mw_ctfree (ct)

	# Determine the dispersion type.
	iferr (dtype = imgeti (im, "DC-FLAG"))
	    dtype = DCNO
	if (dtype==DCLOG) {
	    if (dabs(w1)>20. || dabs(w1+(nw-1)*dw)>20.)
		dtype = DCLINEAR
	    else {
		w1 = 10D0 ** w1
		dw = w1 * (10D0 ** ((nw-1)*dw) - 1) / (nw - 1)
	    }
	}

	# Set the SMW data structure.
	call smw_open (smw, NULLPTR, im)
	do i = 1, SMW_NSPEC(smw) {
	    call smw_mw (smw, i, c_1, mw, j, k)
	    if (j < 1000)
		call sprintf (Memc[key], SZ_FNAME, "APNUM%d")
	    else
		call sprintf (Memc[key], SZ_FNAME, "AP%d")
		call pargl (j)
	    call imgstr (im, Memc[key], Memc[val], SZ_LINE)
	    ik = 1
	    nchar = ctol (Memc[val], ik, ap)
	    nchar = ctoi (Memc[val], ik, beam)
	    if (ctor (Memc[val], ik, aplow[1]) == 0)
		aplow[1] = INDEF
	    if (ctor (Memc[val], ik, aphigh[1]) == 0)
		aphigh[1] = INDEF
	    if (ctor (Memc[val], ik, aplow[2]) == 0)
		aplow[2] = INDEF
	    if (ctor (Memc[val], ik, aphigh[2]) == 0)
		aphigh[2] = INDEF
	    z = 0.

	    call smw_swattrs (smw, i, c_1, ap, beam, dtype, w1, dw, nw, z,
		aplow, aphigh, "")

	    call sprintf (Memc[key], SZ_FNAME, "APID%d")
		call pargl (j)
	    ifnoerr (call imgstr (im, Memc[key], Memc[val], SZ_LINE))
		call smw_sapid (smw, i, c_1, Memc[val])
	}

	call sfree (sp)
end
