include <imhdr.h>
include	<smw.h>


# SMW_ONEDSPEC -- Convert old "onedspec" format to EQUISPEC.

procedure smw_onedspec (im, smw)

pointer	im			#I IMIO pointer
pointer	smw			#U MWCS pointer input SMW pointer output

int	i, dtype, ap, beam, nw, imgeti(), imofnlu(), imgnfn()
real	aplow[2], aphigh[2], imgetr(), mw_c1tranr()
double	ltm, ltv, r, w, dw, z, imgetd()
pointer	sp, key, mw, ct, mw_openim(), mw_sctran()
bool	fp_equald()
errchk	smw_open, smw_saxes, mw_gwtermd, mw_sctran

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)

	# Convert old W0/WPC keywords if needed.
	mw = smw
	iferr (w = imgetd (im, "CRVAL1")) {
	    ifnoerr (w = imgetd (im, "W0")) {
		dw = imgetd (im, "WPC")
		iferr (ltm = imgetd (im, "LTM1_1"))
		    ltm = 1
		iferr (ltv = imgetd (im, "LTV1"))
		    ltv = 0
		r = ltm + ltv
		dw = dw / ltm
		call imaddd (im, "CRPIX1", r)
		call imaddd (im, "CRVAL1", w)
		call imaddd (im, "CD1_1", dw)
		call imaddd (im, "CDELT1", dw)
		call mw_close(mw)
		mw = mw_openim (im)
	    }
	}

	# Get dispersion and determine number of valid pixels.
	call mw_gwtermd (mw, r, w, dw, 1)
	w = w - (r - 1) * dw
	r = 1
	call mw_swtermd (mw, r, w, dw, 1)
	ct = mw_sctran (mw, "logical", "physical", 1)
	nw = max (mw_c1tranr (ct, 1.), mw_c1tranr (ct, real (IM_LEN(im,1))))
	call mw_ctfree (ct)

	iferr (dtype = imgeti (im, "DC-FLAG")) {
	    if (fp_equald (1D0, w) || fp_equald (1D0, dw))
		dtype = DCNO
	    else
		dtype = DCLINEAR
	}
	if (dtype==DCLOG) {
	    if (abs(w)>20. || abs(w+(nw-1)*dw)>20.)
		dtype = DCLINEAR
	    else {
		w = 10D0 ** w
		dw = w * (10D0 ** ((nw-1)*dw) - 1) / (nw - 1)
	    }
	}

	# Convert to EQUISPEC system.
	call mw_swattrs (mw, 0, "system", "equispec")
	if (dtype != DCNO) {
	    iferr (call mw_gwattrs (mw, 1, "label", Memc[key], SZ_FNAME)) {
		iferr (call mw_gwattrs (mw, 1, "units", Memc[key], SZ_FNAME)) {
		    call mw_swattrs (mw, 1, "units", "angstroms")
		    call mw_swattrs (mw, 1, "label", "Wavelength")
		}
	    }
	}

	# Set the SMW data structure.
	call smw_open (smw, NULL, im)

	# Determine the aperture parameters.
	iferr (beam = imgeti (im, "BEAM-NUM"))
	    beam = 1
	iferr (ap = imgeti (im, "APNUM"))
	    ap = beam
	iferr (aplow[1] = imgetr (im, "APLOW"))
	    aplow[1] = INDEF
	iferr (aphigh[1] = imgetr (im, "APHIGH"))
	    aphigh[1] = INDEF
	iferr (z = imgetd (im, "DOPCOR"))
	    z = 0.

	call smw_swattrs (smw, 1, 1, ap, beam, dtype, w, dw, nw, z,
	    aplow, aphigh, "")

        # Delete old parameters
        i = imofnlu (im,
            "BEAM-NUM,APNUM,APLOW,APHIGH,DOPCOR,DC-FLAG,W0,WPC,NP1,NP2")
        while (imgnfn (i, Memc[key], SZ_FNAME) != EOF) {
            iferr (call imdelf (im, Memc[key]))
		;
	}
	call imcfnl (i)

        # Update MWCS
        call smw_saveim (smw, im)

	call sfree (sp)
end
