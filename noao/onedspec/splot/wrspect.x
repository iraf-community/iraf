include	<error.h>
include	<imhdr.h>
include	"../shdr.h"

# WRSPECT -- Write spectrum to the same image or another image.

procedure wrspect (sh1)

pointer	sh1		# Spectrum pointer to be written

int	i, j, axis[2]
pointer	sp, str, in, out, mw2, sh2
data	axis/1,2/

bool	clgetb(), xt_imnameeq()
pointer immap(), mwopen(), smw_openim(), imgl3r(), impl3r()
errchk	immap,  mwopen, smw_openim, shdr_open, wrspect1

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	in = IM(sh1)

	iferr {
10	    call clgstr ("new_image", Memc[str], SZ_LINE)

	    # If replacing the spectum in the current image verify whether
	    # to overwrite.  If overwriting, reopen the image READ_WRITE.
	    # If this is not possible it is an error.  If not overwriting,
	    # get a new output name.
	    #
	    # If writing to another image determine if the image exists.
	    # If not, make a NEW_COPY of the image and copy all spectra
	    # and associated data.  Non-MULTISPEC format spectra, that
	    # is long slit or 2D images, are copied to a 1D spectrum.
	    #
	    # If the image exists, verify whether to overwrite.  If overwriting,
	    # open the image READ_WRITE and return an error if this is not
	    # possible.  If the output image has only one spectrum delete
	    # the image and create a NEW_COPY of the current spectrum image.
	    # Otherwise we will be replacing only the current spectrum so
	    # copy all spectra from the current image.
	    #
	    # When the input and output images are not the same open the
	    # output WCS and select the spectrum of the same aperture
	    # to replace.  It is an error if the output spectrum does not
	    # contain a spectrum of the same aperture.  It is also an error
	    # if the output spectrum is not MULTISPEC; i.e. if it is a
	    # TWODSPEC image.

	    if (xt_imnameeq (SPECTRUM(sh1), Memc[str])) {
		if (!clgetb ("overwrite"))
		    goto 10

		call imunmap (in)
		iferr (in = immap (SPECTRUM(sh1), READ_WRITE, 0)) {
		    in = immap (SPECTRUM(sh1), READ_ONLY, 0)
		    call erract (EA_WARN)
		    goto 10
		}
		IM(sh1) = in
		out = in
		mw2 = MW(sh1)
		sh2 = sh1

	    } else {
		iferr (out = immap (Memc[str], NEW_COPY, in)) {
		    if (!clgetb ("overwrite"))
			goto 10
		    iferr (out = immap (Memc[str], READ_WRITE, 0)) {
			call erract (EA_WARN)
			goto 10
		    }
		    if (IM_LEN(out,2) == 1) {
			call imunmap (out)
			call imdelete (Memc[str])
			out = immap (Memc[str], NEW_COPY, in)
			IM_PIXTYPE(out) = TY_REAL
			do j = 1, IM_LEN(out,3)
			    do i = 1, IM_LEN(out,2)
				call amovr (Memr[imgl3r(in,i,j)],
				    Memr[impl3r(out,i,j)], IM_LEN(out,1))
		    }

		    mw2 = smw_openim (out)
		    call shdr_open (out, mw2, INDEX1(sh1), INDEX2(sh1), AP(sh1),
			SHHDR, sh2)

		} else {
		    IM_PIXTYPE(out) = TY_REAL
		    if (FORMAT(sh1) != MULTISPEC) {
			IM_NDIM(out) = 1
			IM_LEN(out,1) = SN(sh1)

			# Set WCS
			mw2 = mwopen (NULL, 2)
			call mw_newsystem (mw2, "multispec", 2)
			call mw_swtype (mw2, axis, 2, "multispec", "")
			call strcpy (TITLE(sh1), IM_TITLE(out), SZ_IMTITLE)
			if (LABEL(sh1) != EOS)
			    call mw_swattrs (mw2, 1, "label", LABEL(sh1))
			if (UNITS(sh1) != EOS)
			    call mw_swattrs (mw2, 1, "units", UNITS(sh1))
			call shdr_swattrs (mw2, 1, AP(sh1), BEAM(sh1), DC(sh1),
			    double(W0(sh1)), double(WP(SH1)), SN(sh1), 0D0,
			    double (APLOW(sh1)), double (APHIGH(sh1)), "")
			call shdr_open (out, mw2, INDEX1(sh1), 1, AP(sh1),
			    SHHDR, sh2)
		    } else {
			mw2 = smw_openim (out)
			call shdr_open (out, mw2, INDEX1(sh1), INDEX2(sh1),
			    AP(sh1), SHHDR, sh2)

			do j = 1, IM_LEN(out,3)
			    do i = 1, IM_LEN(out,2)
				call amovr (Memr[imgl3r(in,i,j)],
				    Memr[impl3r(out,i,j)], IM_LEN(out,1))
		    }
		}
	    }

	    # Check the output format and aperture numbers.
	    if (FORMAT(sh2) != MULTISPEC)
		call error (1, "Cannot write to output image format")
	    if (AP(sh2) != AP(sh1) || INDEX2(sh2) != INDEX2(sh1))
		call error (1, "Aperture not found in output spectrum")

	    # Check, set, and update the WCS information.
	    call wrspect1 (sh1, sh2)
	    call smw_saveim (mw2, out)

	    # Copy the spectrum.
	    i =  max (1, INDEX1(sh2))
	    j =  max (1, INDEX2(sh2))
	    call amovr (Memr[SY(sh1)], Memr[impl3r(out,i,j)], SN(sh1))
	    if (SN(sh1) < IM_LEN(out,1))
		call aclrr (Memr[impl3r(out,i,j)+SN(sh1)],
		    IM_LEN(out,1)-SN(sh1))

	    # Close output image if not the same as the input image.
	    if (out != in) {
		call shdr_close (sh2)
		call mw_close (mw2)
		call imunmap (out)
	    }
	} then {
	    if (out != in) {
		if (sh2 != NULL)
		    call shdr_close (sh2)
		if (mw2 != NULL)
		    call mw_close (mw2)
		call imunmap (out)
	    }
	    call erract (EA_WARN)
	}
end


# WRSPECT1 -- Write spectrum header to MULTISPEC output image.
# This requires checking compatibility of the WCS with the other spectra.
 
procedure wrspect1 (sh1, sh2)

pointer	sh1			# Input
pointer	sh2			# Output

int	j, k, ap, beam, dtype, nw
double	w1, dw, z, aplow, aphigh, a, b
pointer	in, out, mw1, mw2
pointer	sp, key, ltm, ltv, coeff
bool	strne(), fp_equald()
errchk	shdr_gwattrs, shdr_swattrs

begin
	if (FORMAT(sh2) != MULTISPEC)
	    call error (1, "Cannot write to output image format")

	call smark (sp)
	call salloc (key, SZ_LINE, TY_CHAR)
	call salloc (ltm, 3*3, TY_DOUBLE)
	call salloc (ltv, 3, TY_DOUBLE)
	call malloc (coeff, SZ_LINE, TY_CHAR)

	in = IM(sh1)
	out = IM(sh2)
	mw1 = MW(sh1)
	mw2 = MW(sh2)

	# Check dispersion function compatibility.
	# Nonlinear functions can't be copied to a different physical
	# coordinate system though the linear dispersion can be
	# adjusted.

	call mw_gltermd (mw2, Memd[ltm], Memd[ltv], max (2, PNDIM(out)))
	a = Memd[ltv]
	b = Memd[ltm]
	if (DC(sh1) == DCFUNC) {
	    call mw_gltermd (mw1, Memd[ltm], Memd[ltv], PNDIM(sh1))
	    if (DAXIS(sh1) == 2) {
		Memd[ltv] = Memd[ltv+1]
		Memd[ltm] = Memd[ltm+PNDIM(sh1)+1]
	    }
	   if (!fp_equald (a, Memd[ltv]) || !fp_equald (b ,Memd[ltm])) {
		call error (1,
		"Physical basis for nonlinear dispersion functions don't match")
	    }
	}

	switch (FORMAT(sh1)) {
	case MULTISPEC:
	    call shdr_gwattrs (mw1, PINDEX1(sh1), ap, beam, dtype,
		w1, dw, nw, z, aplow, aphigh, coeff)
	case TWODSPEC:
	    ap = AP(sh1)
	    beam = BEAM(sh1)
	    dtype = DC(sh1)
	    z = 0.
	    aplow = APLOW(sh1)
	    aphigh = APHIGH(sh1)
	    Memc[coeff] = EOS
	}

	j = nint ((1. - a) / b)
	k = nint ((SN(sh1) - a) / b)
	nw = min (min (j ,k) + IM_LEN(out,1), max (j ,k))
	if (dtype == DCLOG) {
	    dw = log10 (W1(sh1) / W0(sh1)) / (k - j)
	    w1 = log10 (W0(sh1) / (1 - z)) - (j - 1) * dw
	} else {
	    dw = (W1(sh1) - W0(sh1)) / (k - j) / (1 - z)
	    w1 = W0(sh1) / (1 - z) - (j - 1) * dw
	}

	call shdr_swattrs (mw2, PINDEX1(sh2), ap, beam, dtype,
	    w1, dw, nw, z, aplow, aphigh, Memc[coeff])

	# Copy title
	if (strne (IM_TITLE(out), TITLE(sh1))) {
	    call sprintf (Memc[key], SZ_LINE, "APID%d")
		call pargi (PINDEX1(sh2))
	    call imastr (out, Memc[key], TITLE(sh1))
	}

	call mfree (coeff, TY_CHAR)
	call sfree (sp)
end
