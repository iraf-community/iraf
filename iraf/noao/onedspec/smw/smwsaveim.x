include <imhdr.h>
include	<imio.h>
include	<smw.h>


# SMW_SAVEIM -- Save spectral WCS in image header.
# The input and output formats are EQUISPEC and MULTISPEC.  A split input
# MULTISPEC WCS is first merged to a single EQUISPEC or MULTISPEC WCS.
# An input MULTISPEC WCS is converted to EQUISPEC output if possible.

procedure smw_saveim (smw, im)

pointer	smw			# SMW pointer
pointer	im			# Image pointer

size_t	sz_val
int	format, pdim, pdim1, beam, dtype, dtype1, ii
int	axes[3]
long	ap, nl, nw, nw1, i, j, c_1
real	aplow[2], aphigh[2]
double	v, m, w1, dw, z, w11, dw1, z1
pointer	sp, key, str1, str2, axmap, lterm, coeff, mw, mw1

bool	strne(), fp_equald()
int	imaccf(), imgeti()
pointer	mw_open()
errchk	smw_merge, imdelf
data	axes/1,2,3/

include	<nullptr.inc>

begin
	c_1 = 1

	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (key, sz_val, TY_CHAR)
	sz_val = SZ_LINE
	call salloc (str1, sz_val, TY_CHAR)
	call salloc (str2, sz_val, TY_CHAR)
	sz_val = 6
	call salloc (axmap, sz_val, TY_INT)
	sz_val = 15
	call salloc (lterm, sz_val, TY_DOUBLE)
	coeff = NULL

	# Merge split WCS into a single WCS.
	call smw_merge (smw)

	mw = SMW_MW(smw,0)
	pdim = SMW_PDIM(smw)
	format = SMW_FORMAT(smw)
	if (IM_NDIM(im) == 1)
	    nl = 1
	else
	    nl = IM_LEN(im,2)

	# If writing to an existing image we must follow IM_NPHYSDIM
	# but in a NEW_COPY header we may really want a lower dimension.
	# Since IM_NPHYSDIM is outside the interface we only violate
	# it here and use a temporary keyword to communicate from the
	# routine setting up the WCS.

	pdim1 = max (IM_NDIM(im), IM_NPHYSDIM(im))
	ifnoerr (ii = imgeti (im, "SMW_NDIM")) {
	    pdim1 = ii
	    call imdelf (im, "SMW_NDIM")
	}

	# Check if MULTISPEC WCS can be converted to EQUISPEC.
	if (format == SMW_MS) {
	    format = SMW_ES
	    do i = 1, nl {
		call smw_gwattrs (smw, i, c_1, ap, beam, dtype, w1, dw, nw, z,
		    aplow, aphigh, coeff)
		if (i == 1) {
		    dtype1 = dtype
		    w11 = w1
		    dw1 = dw
		    z1 = z
		    nw1 = nw
		}
		if (dtype>1||dtype!=dtype1||!fp_equald(w1,w11)||
		    !fp_equald(dw,dw1)||nw!=nw1||!fp_equald(z,z1)) {
		    format = SMW_MS
		    break
		}
	    }
	}

	# Save WCS in desired format.
	switch (format) {
	case SMW_ND:
	    if (SMW_DTYPE(smw) != -1)
		call imaddi (im, "DC-FLAG", SMW_DTYPE(smw))
	    else if (imaccf (im, "DC-FLAG") == YES)
		call imdelf (im, "DC-FLAG")
	    if (imaccf (im, "DISPAXIS") == YES)
		call imaddi (im, "DISPAXIS", SMW_PAXIS(smw,1))

	    call smw_gapid (smw, c_1, c_1, IM_TITLE(im), SZ_IMTITLE)
	    call mw_saveim (mw, im)

	case SMW_ES:
	    # Save aperture information.
	    do i = 1, nl {
		call smw_gwattrs (smw, i, c_1, ap, beam, dtype, w1, dw, nw, z,
		    aplow, aphigh, coeff)
		if (i < 1000)
		    call sprintf (Memc[key], SZ_FNAME, "APNUM%d")
		else
		    call sprintf (Memc[key], SZ_FNAME, "AP%d")
		    call pargl (i)
		call sprintf (Memc[str1], SZ_LINE, "%d %d")
		    call pargl (ap)
		    call pargi (beam)
		if (!IS_INDEF(aplow[1]) || !IS_INDEF(aphigh[1])) {
		    call sprintf (Memc[str2], SZ_LINE, " %.2f %.2f")
			call pargr (aplow[1])
			call pargr (aphigh[1])
		    call strcat (Memc[str2], Memc[str1], SZ_LINE)
		    if (!IS_INDEF(aplow[2]) || !IS_INDEF(aphigh[2])) {
			call sprintf (Memc[str2], SZ_LINE, " %.2f %.2f")
			    call pargr (aplow[2])
			    call pargr (aphigh[2])
			call strcat (Memc[str2], Memc[str1], SZ_LINE)
		    }
		}
		call imastr (im, Memc[key], Memc[str1])
		if (i == 1) {
		    iferr (call imdelf (im, "APID1"))
			;
		}
		call smw_gapid (smw, i, c_1, Memc[str1], SZ_LINE)
		if (Memc[str1] != EOS) {
		    if (strne (Memc[str1], IM_TITLE(im))) {
			if (nl == 1) {
			    call imastr (im, "MSTITLE", IM_TITLE(im))
			    call strcpy (Memc[str1], IM_TITLE(im), SZ_IMTITLE)
			} else {
			    call sprintf (Memc[key], SZ_FNAME, "APID%d")
				call pargl (i)
			    call imastr (im, Memc[key], Memc[str1])
			}
		    }
		}
	    }

	    # Delete unnecessary aperture information.
	    do i = nl+1, MAX_LONG {
		if (i < 1000)
		    call sprintf (Memc[key], SZ_FNAME, "APNUM%d")
		else
		    call sprintf (Memc[key], SZ_FNAME, "AP%d")
		    call pargl (i)
		iferr (call imdelf (im, Memc[key]))
		    break
		call sprintf (Memc[key], SZ_FNAME, "APID%d")
		    call pargl (i)
		iferr (call imdelf (im, Memc[key]))
		    ;
	    }

	    # Add dispersion parameters to image.
	    if (dtype != -1)
		call imaddi (im, "DC-FLAG", dtype)
	    else if (imaccf (im, "DC-FLAG") == YES)
		call imdelf (im, "DC-FLAG")
	    if (nw < IM_LEN(im,1))
		call imaddl (im, "NP2", nw)
	    else if (imaccf (im, "NP2") == YES)
		call imdelf (im, "NP2")

	    # Setup EQUISPEC WCS.

	    mw1 = mw_open (NULLPTR, pdim1)
	    call mw_newsystem (mw1, "equispec", pdim1)
	    call mw_swtype (mw1, axes, pdim1, "linear", "")
	    ifnoerr (call mw_gwattrs (mw, 1, "label", Memc[str1], SZ_LINE))
		call mw_swattrs (mw1, 1, "label", Memc[str1])
	    ifnoerr (call mw_gwattrs (mw, 1, "units", Memc[str1], SZ_LINE))
		call mw_swattrs (mw1, 1, "units", Memc[str1])
	    ifnoerr (call mw_gwattrs (mw, 1, "units_display", Memc[str1],
		SZ_LINE))
		call mw_swattrs (mw1, 1, "units_display", Memc[str1])
	    call mw_gltermd (mw, Memd[lterm+pdim], Memd[lterm], pdim)
	    v = Memd[lterm]
	    m = Memd[lterm+pdim]
	    call mw_gltermd (mw1, Memd[lterm+pdim1], Memd[lterm], pdim1)
	    Memd[lterm] = v
	    Memd[lterm+pdim1] = m
	    call mw_sltermd (mw1, Memd[lterm+pdim1], Memd[lterm], pdim1)
	    call mw_gwtermd (mw1, Memd[lterm], Memd[lterm+pdim1],
		Memd[lterm+2*pdim1], pdim1)
	    Memd[lterm] = 1.
	    w1 = w1 / (1 + z)
	    dw = dw / (1 + z)
	    if (dtype == DCLOG) {
		dw = log10 ((w1 + (nw - 1) * dw) / w1) / (nw - 1)
		w1 = log10 (w1)
	    }
	    Memd[lterm+pdim1] = w1
	    Memd[lterm+2*pdim1] = dw
	    call mw_swtermd (mw1, Memd[lterm], Memd[lterm+pdim1],
		Memd[lterm+2*pdim1], pdim1)
	    call mw_saveim (mw1, im)
	    call mw_close (mw1)

	case SMW_MS:
	    # Delete any APNUM keywords.  If there is only one spectrum
	    # define the axis mapping.

	    do j = 1, MAX_LONG {
		if (j < 1000)
		    call sprintf (Memc[key], SZ_FNAME, "APNUM%d")
		else
		    call sprintf (Memc[key], SZ_FNAME, "AP%d")
		    call pargl (j)
		iferr (call imdelf (im, Memc[key]))
		    break
	    }
	    if (IM_NDIM(im) == 1) {
		sz_val = 2*pdim
		call aclri (Memi[axmap], sz_val)
		Memi[axmap] = 1
		call mw_saxmap (mw, Memi[axmap], Memi[axmap+pdim], pdim)
	    }

	    # Set aperture ids.
	    do i = 1, nl {
		if (i == 1) {
		    iferr (call imdelf (im, "APID1"))
			;
		}
		call smw_gapid (smw, i, c_1, Memc[str1], SZ_LINE)
		if (Memc[str1] != EOS) {
		    if (strne (Memc[str1], IM_TITLE(im))) {
			if (nl == 1) {
			    call imastr (im, "MSTITLE", IM_TITLE(im))
			    call strcpy (Memc[str1], IM_TITLE(im), SZ_IMTITLE)
			} else {
			    call sprintf (Memc[key], SZ_FNAME, "APID%d")
				call pargl (i)
			    call imastr (im, Memc[key], Memc[str1])
			}
		    }
		}
	    }

	    do i = nl+1, MAX_LONG {
		call sprintf (Memc[key], SZ_FNAME, "APID%d")
		    call pargl (i)
		iferr (call imdelf (im, Memc[key]))
		    break
	    }

	    call mw_saveim (mw, im)
	}

	call mfree (coeff, TY_CHAR)
	call sfree (sp)
end
