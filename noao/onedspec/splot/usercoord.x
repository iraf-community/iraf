include	<error.h>
include	<smw.h>
include <units.h>

# USERCOORD -- Set user coordinates

procedure usercoord (sh, key, w1, u1, w2, u2)

pointer	sh
int	key
double	w1, u1, w2, u2

int	i, format, ap, beam, dtype, nw
double	shift, wa, wb, ua, ub, w0, dw, z, smw_c1trand()
real	aplow[2], aphigh[2]
pointer	coeff, smw, mw, ct, smw_sctran()
errchk	smw_sctran

begin
	coeff = NULL
	smw = MW(sh)
	mw = SMW_MW(smw,0)
	format = SMW_FORMAT(smw)

	iferr {
	    call un_ctrand (UN(sh), MWUN(sh), w1, wa, 1)
	    call un_ctrand (UN(sh), MWUN(sh), u1, ua, 1)

	    call smw_gwattrs (MW(sh), APINDEX(sh), LINDEX(sh,2),
		ap, beam, dtype, w0, dw, nw, z, aplow, aphigh, coeff)

	    switch (key) {
	    case 'd':
		wa = wa * (1 + z)
		switch (UN_CLASS(MWUN(sh))) {
		case UN_WAVE:
		    z = (wa - ua) / ua
		case UN_FREQ, UN_ENERGY:
		    z = (ua - wa) / wa
		default:
		    call error (1, "Inappropriate coordinate units")
		}
	    case 'z':
		shift = ua - wa
		w0 = w0 + shift
		if (dtype == 2)
		    call sshift1 (shift, coeff)
	    case 'l':
		call un_ctrand (UN(sh), MWUN(sh), w2, wb, 1)
		call un_ctrand (UN(sh), MWUN(sh), u2, ub, 1)

		switch (format) {
		case SMW_ND:
		    i = 2 ** (SMW_PAXIS(smw,1) - 1)
		    ct = smw_sctran (smw, "world", "physical", i)
		    wa = smw_c1trand (ct, wa)
		    wb = smw_c1trand (ct, wb)
		case SMW_ES, SMW_MS:
		    ct = smw_sctran (smw, "world", "physical", 3)
		    call smw_c2trand (ct, wa, double (ap), wa, shift)
		    call smw_c2trand (ct, wb, double (ap), wb, shift)
		}
		call smw_ctfree (ct)

		dw = (ub - ua) / (wb - wa)
		w0 = ua - (wa - 1) * dw
		dtype = 0
		if (UNITS(sh) == EOS) {
		    call mw_swattrs (mw, SMW_PAXIS(smw,1),
			"label", "Wavelength")
		    call mw_swattrs (mw, SMW_PAXIS(smw,1),
			"units", "angstroms")
		}
	    default:
		call error (1, "Unknown correction")
	    }

	    call smw_swattrs (smw, LINDEX(sh,1), 1, ap, beam, dtype, w0,
		dw, nw, z, aplow, aphigh, Memc[coeff])
	    if (smw != MW(sh)) {
		CTLW1(sh) = NULL
		CTWL1(sh) = NULL
		MW(sh) = smw
	    }

	    DC(sh) = dtype
	    call shdr_system (sh, "world")
	    if (UN_CLASS(UN(sh)) == UN_UNKNOWN)
		call un_copy (MWUN(sh), UN(sh))
	} then
	    call erract (EA_WARN)

	call mfree (coeff, TY_CHAR)
end
