include	<error.h>
include	<syserr.h>
include	<imhdr.h>
include	<imio.h>
include	<smw.h>
include	<units.h>

# SP_WRSPECT -- Write spectrum to the same image or another image.

procedure sp_wrspect (sh1)

pointer	sh1		# Spectrum pointer to be written

bool	overwrite
pointer	sp, str
int	nowhite(), errcode()
bool	clgetb(), xt_imnameeq()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Initially set overwrite to false in order to warn the user.
	overwrite = false

	# Get new image name.
	call clgstr ("new_image", Memc[str], SZ_LINE)
	if (nowhite (Memc[str], Memc[str], SZ_LINE) == 0) {
	    call sfree (sp)
	    return
	}

	# Check for overwriting the current file.
	if (xt_imnameeq (IMNAME(sh1), Memc[str])) {
	    overwrite = clgetb ("overwrite")
	    if (!overwrite) {
		call sfree (sp)
		return
	    }
	}

	# Write spectrum.
	iferr (call wrspect (sh1, Memc[str], overwrite)) {
	    switch (errcode()) {
	    case SYS_IKICLOB:
		call erract (EA_WARN)
		# Try again if overwrite is requested.
		if (!overwrite)
		    overwrite = clgetb ("overwrite")
		if (overwrite) {
		    iferr (call wrspect (sh1, Memc[str], overwrite))
			call erract (EA_WARN)
		}
	    default:
		call erract (EA_WARN)
	    }
	}
	call sfree (sp)
end


# WRSPECT -- Write spectrum to the same image or another image.
#
# If overwriting reopen the image READ_WRITE.  If this is not possible it is
# an error which may be trapped by the calling routine if desired.
# 
# If writing to another image determine if the image exists.  If not make a
# NEW_COPY of the image and copy all spectra and associated data.  NDSPEC
# format spectra, i.e. 2D or 3D images, are copied to a 1D spectrum.
# 
# If the image exists check the overwrite parameter.  If overwriting, open the
# image READ_WRITE and return an error if this is not possible.  If the
# output image has only one spectrum delete the image and create a NEW_COPY
# of the current spectrum image.  Otherwise we will be replacing only the
# current spectrum so copy all spectra from the current image.
# 
# When the input and output images are not the same open the output WCS and
# select the spectrum of the same aperture to replace.  It is an error if the
# output spectrum does not contain a spectrum of the same aperture.  It is
# also an error if the output spectrum is an NDSPEC image.

procedure wrspect (sh1, output, overwrite)

pointer	sh1		# Spectrum pointer to be written
char	output[ARB]	# Output spectrum filename
bool	overwrite	# Overwrite existing spectrum?

bool	delim
char	errstr[SZ_LINE]
int	i, j, np1, np2, dtype[2], nw[2], err
real	r[2]
double	w1[2], dw[2], z[2]
pointer	coeff, im, in, out, mw1, mw2, sh2, outbuf, ptr

int	imaccf(), errget()
bool	xt_imnameeq(), fp_equald()
pointer immap(), smw_openim(), imgl3r(), impl3r(), imps3r()
errchk	immap,  imgl3r, impl3r, imps3r, imdelf, shdr_open, wrspect1
errchk	smw_openim, smw_gwattrs, smw_swattrs, smw_saveim

begin
	in = IM(sh1)
	mw1 = MW(sh1)
	out = NULL
	mw2 = NULL
	sh2 = NULL
	ptr = NULL
	delim = false

	iferr {
	    # Open and initialize the output image.
	    if (xt_imnameeq (IMNAME(sh1), output)) {
		if (!overwrite) {
		    call sprintf (errstr, SZ_LINE, "No overwrite set (%s)")
			call pargstr (output)
		    call error (1, errstr)
		}

		call imunmap (in)
		iferr (im = immap (IMNAME(sh1), READ_WRITE, 0)) {
		    in = immap (IMNAME(sh1), READ_ONLY, 0)
		    call erract (EA_ERROR)
		}
		in = im
		IM(sh1) = in
		out = in
		mw2 = MW(sh1)
		sh2 = sh1

	    } else {
		iferr (im = immap (output, NEW_COPY, in)) {
		    if (!overwrite)
			call erract (EA_ERROR)
		    im = immap (output, READ_WRITE, 0); out = im

		    if (IM_LEN(out,2) == 1) {
			call imunmap (out)
			call imdelete (output)
			im = immap (output, NEW_COPY, in); out = im
			if (IM_PIXTYPE(out) != TY_DOUBLE)
			    IM_PIXTYPE(out) = TY_REAL
			do j = 1, IM_LEN(out,3)
			    do i = 1, IM_LEN(out,2)
				call amovr (Memr[imgl3r(in,i,j)],
				    Memr[impl3r(out,i,j)], IM_LEN(out,1))
		    }

		    im = smw_openim (out); mw2 = im
		    switch (SMW_FORMAT(mw1)) {
		    case SMW_ND:
			if (SMW_FORMAT(mw2) != SMW_ND)
			    call error (1, "Incompatible spectral formats")
			if (IM_NDIM(in) != IM_NDIM(out))
			    call error (2, "Incompatible dimensions")
			do i = 1, IM_NDIM(in)
			    if (IM_LEN(in,i) != IM_LEN(out,i))
				call error (2, "Incompatible dimensions")
			coeff = NULL
			call smw_gwattrs (mw1, 1, 1, i, i,
			    dtype[1], w1[1], dw[1], nw[1], z, r, r, coeff)
			call smw_gwattrs (mw2, 1, 1, i, i,
			    dtype[2], w1[2], dw[2], nw[2], z, r, r, coeff)
			call mfree (coeff, TY_CHAR)
			if (dtype[1]!=dtype[2] || !fp_equald (w1[1],w1[2]) ||
			    !fp_equald (dw[1],dw[2]))
			    call error (3,
				"Incompatible dispersion coordinates")
			call shdr_open (out, mw2, APINDEX(sh1), LINDEX(sh1,2),
			    AP(sh1), SHHDR, ptr)
			sh2 = ptr
		    case SMW_ES, SMW_MS:
			if (SMW_FORMAT(mw2) == SMW_ND)
			    call error (1, "Incompatible spectral formats")
			call shdr_open (out, mw2, APINDEX(sh1), LINDEX(sh1,2),
			    AP(sh1), SHHDR, ptr)
			sh2 = ptr
		    }

		} else {
		    delim = true
		    out = im
		    IM_PIXTYPE(out) = TY_REAL
		    im = smw_openim (out); mw2 = im
		    call shdr_open (out, mw2, APINDEX(sh1), LINDEX(sh1,2),
			AP(sh1), SHHDR, ptr)
		    sh2 = ptr

		    do j = 1, IM_LEN(out,3)
			do i = 1, IM_LEN(out,2)
			    call amovr (Memr[imgl3r(in,i,j)],
				Memr[impl3r(out,i,j)], IM_LEN(out,1))
		}
	    }

	    # Check, set, and update the WCS information.  Note that
	    # wrspect1 may change the smw pointers.

	    call wrspect1 (sh1, sh2)
	    mw1 = MW(sh1)
	    mw2 = MW(sh2)
	    call smw_saveim (mw2, out)

	    # Update spectrum calibration parameters.
	    if (EC(sh1) == ECYES)
		call imaddi (out, "EX-FLAG", EC(sh1))
	    else if (imaccf (out, "EX-FLAG") == YES)
		call imdelf (out, "EX-FLAG")
	    if (FC(sh1) == FCYES)
		call imaddi (out, "CA-FLAG", FC(sh1))
	    else if (imaccf (out, "CA-FLAG") == YES)
		call imdelf (out, "CA-FLAG")
	    if (RC(sh1) != EOS)
		call imastr (out, "DEREDDEN", RC(sh1))
	    else if (imaccf (out, "DEREDDEN") == YES)
		call imdelf (out, "DEREDDEN")

	    # Copy the spectrum.
	    i =  max (1, LINDEX(sh2,1))
	    j =  max (1, LINDEX(sh2,2))
	    np1 = NP1(sh1)
	    np2 = NP2(sh1)
	    switch (SMW_FORMAT(mw1)) {
	    case SMW_ND:
		switch (SMW_LAXIS(mw1,1)) {
		case 1:
		    outbuf = imps3r (out, np1, np2, i, i, j, j)
		case 2:
		    outbuf = imps3r (out, i, i, np1, np2, j, j)
		case 3:
		    outbuf = imps3r (out, i, i, j, j, np1, np2)
		}
		call amovr (Memr[SY(sh1)], Memr[outbuf], SN(sh1))
	    case SMW_ES, SMW_MS:
		outbuf = impl3r (out, i, j)
		call amovr (Memr[SY(sh1)], Memr[outbuf+np1-1], SN(sh1))
		if (np1 > 1)
		    call amovkr (Memr[outbuf+np1-1], Memr[outbuf], np1-1)
		if (np2 < IM_LEN(out,1))
		    call amovkr (Memr[outbuf+np2-1], Memr[outbuf+np2],
			IM_LEN(out,1)-np2)
	    }

	    # Close output image if not the same as the input image.
	    if (out != in) {
		call shdr_close (sh2)
		call smw_close (mw2)
		call imunmap (out)
	    }
	} then {
	    err = errget (errstr, SZ_LINE)
	    if (out != in) {
		if (sh2 != NULL)
		    call shdr_close (sh2)
		if (mw2 != NULL)
		    call smw_close (mw2)
		if (out != NULL) {
		    call imunmap (out)
		    if (delim)
			iferr (call imdelete (output))
			    ;
		}
	    }
	    call error (err, errstr)
	}

end


# WRSPECT1 -- Set output WCS attributes.
# This requires checking compatibility of the WCS with other spectra
# in the image.
 
procedure wrspect1 (sh1, sh2)

pointer	sh1			# Input
pointer	sh2			# Output

int	i, j, beam, dtype, nw
double	w1, wb, dw, z, a, b, p1, p2, p3, shdr_lw()
real	aplow[2], aphigh[2]
pointer	in, out, smw1, smw2, mw, smw_sctran()
pointer	sp, key, str, ltm, ltv, coeff
bool	fp_equald(), strne()
errchk	mw_glterm, smw_gwattrs, smw_swattrs, smw_sctran

begin
	in = IM(sh1)
	out = IM(sh2)
	smw1 = MW(sh1)
	smw2 = MW(sh2)
	mw = SMW_MW(smw2,0)

	# The output format must not be NDSPEC and there must be a
	# matching aperture in the output image.

	if (AP(sh2) != AP(sh1) || LINDEX(sh2,1) != LINDEX(sh1,1))
	    call error (6, "Matching aperture not found in output image")

	call smark (sp)
	call salloc (key, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (ltm, 3*3, TY_DOUBLE)
	call salloc (ltv, 3, TY_DOUBLE)
	call malloc (coeff, SZ_LINE, TY_CHAR)

	# Check dispersion function compatibility.
	# Nonlinear functions can't be copied to a different physical
	# coordinate system though the linear dispersion can be
	# adjusted.

	i = SMW_PDIM(smw2)
	j = SMW_PAXIS(smw2,1)
	call mw_gltermd (mw, Memd[ltm], Memd[ltv], SMW_PDIM(smw2))
	a = Memd[ltv+(j-1)]
	b = Memd[ltm+(i+1)*(j-1)]
	if (DC(sh1) == DCFUNC) {
	    i = SMW_PDIM(smw1)
	    j = SMW_PAXIS(smw1,1)
	    call mw_gltermd (SMW_MW(smw1,0), Memd[ltm], Memd[ltv], i)
	    Memd[ltv] = Memd[ltv+(j-1)]
	    Memd[ltm] = Memd[ltm+(i+1)*(j-1)]
	   if (!fp_equald (a, Memd[ltv]) || !fp_equald (b ,Memd[ltm])) {
		call error (7,
		"Physical basis for nonlinear dispersion functions don't match")
	    }
	}

	call smw_gwattrs (smw1, LINDEX(sh1,1), LINDEX(sh1,2),
	    AP(sh1), beam, dtype, w1, dw, nw, z, aplow, aphigh, coeff)

	w1 = shdr_lw (sh1, 1D0)
	wb = shdr_lw (sh1, double(SN(sh1)))
	iferr {
	    call un_ctrand (UN(sh1), MWUN(sh1), w1, w1, 1)
	    call un_ctrand (UN(sh1), MWUN(sh1), wb, wb, 1)
	} then
	    ;

	p1 = (NP1(sh1) - a) / b
	p2 = (NP2(sh1) - a) / b
	p3 = (IM_LEN(out,1) - a) / b
	nw = nint (min (max (p1 ,p3), max (p1, p2))) + NP1(sh1) - 1
	if (dtype == DCLOG) {
	    if (p1 != p2)
		dw = (log10(wb*(1+z)) - log10(w1*(1+z))) / (p2 - p1)
	    w1 = log10 (w1*(1+z)) - (p1 - 1) * dw
	    w1 = 10. ** w1
	    dw = (w1 * 10D0 ** ((nw-1)*dw) - w1) / (nw - 1)
	} else {
	    if (p1 != p2)
		dw = (wb - w1) / (p2 - p1) * (1 + z)
	    w1 = w1 * (1 + z) - (p1 - 1) * dw
	}

	# Note that this may change the smw pointer.
	call smw_swattrs (smw2, LINDEX(sh2,1), 1, AP(sh2), beam, dtype,
	    w1, dw, nw, z, aplow, aphigh, Memc[coeff])
	if (smw2 != MW(sh2)) {
	    switch (SMW_FORMAT(smw2)) {
	    case SMW_ND, SMW_ES:
	        i = 2 ** (SMW_PAXIS(smw2,1) - 1)
	    case SMW_MS:
		i = 3B
	    }
	    CTLW1(sh2) = smw_sctran (smw2, "logical", "world", i)
	    CTWL1(sh2) = smw_sctran (smw2, "world", "logical", i)
	    CTLW(sh2) = CTLW1(sh2)
	    CTWL(sh2) = CTWL1(sh2)
	    MW(sh2) = smw2
	    mw = SMW_MW(smw2,0)
	}

	# Copy title
	call smw_sapid (smw2, LINDEX(sh2,1), 1, TITLE(sh1))
	if (Memc[SID(sh1,1)] != EOS) {
	    call sprintf (Memc[key], SZ_LINE, "BANDID%d")
		call pargi (LINDEX(sh1,2))
	    iferr (call imgstr (out, Memc[key], Memc[str], SZ_LINE))
		call imastr (out, Memc[key], Memc[SID(sh1,1)])
	    else {
		if (strne (Memc[SID(sh1,1)], Memc[str]))
		    call eprintf (
			"Warning: Input and output types (BANDID) differ\n")
	    }
	}

	# Copy label and units
	if (UN_LABEL(MWUN(sh1)) != EOS)
	    call mw_swattrs (mw, 1, "label", UN_LABEL(MWUN(sh1)))
	if (UN_UNITS(MWUN(sh1)) != EOS)
	    call mw_swattrs (mw, 1, "units", UN_UNITS(MWUN(sh1)))
	if (UN_USER(UN(sh1)) != EOF)
	    call mw_swattrs (mw, 1, "units_display", UN_USER(UN(sh1)))

	call mfree (coeff, TY_CHAR)
	call sfree (sp)
end
