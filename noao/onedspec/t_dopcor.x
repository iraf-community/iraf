include	<error.h>
include	<imhdr.h>
include	<smw.h>

define	EXTN_LOOKUP	10	# Interp index for de-extinction
define	VLIGHT	2.997925e5	# Speed of light, Km/sec

# T_DOPCOR -- Apply doppler correction to spectra.

procedure t_dopcor ()

int	inlist			# List of input spectra
int	outlist			# List of output spectra
double	z			# Doppler redshift or velocity
bool	isvel			# Is redshift parameter a velocity?
bool	add			# Add to existing correction?
bool	dcor			# Apply dispersion correction?
bool	fcor			# Apply flux correction?
real	ffac			# Flux correction factor (power of 1+z)
pointer	aps			# Apertures
bool	verbose			# Verbose?

real	fcval
bool	wc, fc, aplow[2], aphigh[2]
int	i, j, ap, beam, nw, dtype
double	w1, dw, zold, znew, zvel
pointer	ptr, in, out, mw, sh, inbuf, outbuf
pointer	sp, input, output, vkey, apstr, key, log, coeff

real	clgetr()
double	imgetd()
bool	clgetb(), streq(), rng_elementi()
int	imtopenp(), imtgetim(), ctod()
pointer	rng_open(), immap(), smw_openim(), imgl3r(), impl3r()
errchk	immap, imgetd, imgstr,imgl3r, impl3r
errchk	smw_openim, shdr_open, smw_gwattrs

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (vkey, SZ_FNAME, TY_CHAR)
	call salloc (apstr, SZ_FNAME, TY_CHAR)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (log, SZ_LINE, TY_CHAR)
	coeff = NULL

	# Parameters
	inlist = imtopenp ("input")
	outlist = imtopenp ("output")
	call clgstr ("redshift", Memc[vkey], SZ_FNAME)
	isvel = clgetb ("isvelocity")
	add = clgetb ("add")
	dcor = clgetb ("dispersion") 
	fcor = clgetb ("flux")
	if (fcor)
	    ffac = clgetr ("factor")
	else
	    ffac = 0.
	call clgstr ("apertures", Memc[apstr], SZ_FNAME)
	verbose = clgetb ("verbose")

	# Parameter checks
	if (!dcor && !fcor)
	    call error (1, "No correction specified")
	iferr (aps = rng_open (Memc[apstr], INDEF, INDEF, INDEF))
	    call error (1, "Bad aperture list")
	if (Memc[apstr] == EOS)
	    call strcpy ("all", Memc[apstr], SZ_LINE)

	# Loop over input images.
	while (imtgetim (inlist, Memc[input], SZ_FNAME) != EOF) {
	    if (imtgetim (outlist, Memc[output], SZ_FNAME) == EOF)
		call strcpy (Memc[input], Memc[output], SZ_FNAME)

	    iferr {
		in = NULL
		out = NULL
		mw = NULL
		sh = NULL

		# Map and check input image.
		if (streq (Memc[input], Memc[output]))
		    ptr = immap (Memc[input], READ_WRITE, 0)
		else
		    ptr = immap (Memc[input], READ_ONLY, 0)
		in = ptr

		ptr = smw_openim (in); mw = ptr
		call shdr_open (in, mw, 1, 1, INDEFI, SHHDR, sh)
                if (DC(sh) == DCNO) {
                    call sprintf (Memc[output], SZ_LINE,
                        "[%s] has no dispersion function")
                        call pargstr (Memc[input])
                    call error (1, Memc[output])
                }

		# Map output image.
		if (streq (Memc[input], Memc[output]))
		    ptr = in
		else
		    ptr = immap (Memc[output], NEW_COPY, in)
		out = ptr

		# Set velocity and flux correction
		i = 1
		if (Memc[vkey] == '-' || Memc[vkey] == '+') {
		    if (ctod (Memc[vkey+1], i, z) == 0) {
			z = imgetd (in, Memc[vkey+1])
			if (Memc[vkey] == '-') {
			    if (isvel)
				z = -z
			    else
				z = 1 / (1 + z) - 1
			}
		    } else if (Memc[vkey] == '-')
			z = -z
		} else {
		    if (ctod (Memc[vkey], i, z) == 0)
			z = imgetd (in, Memc[vkey])
		}
		zvel = z
		if (isvel) {
		    z = z / VLIGHT
		    if (abs (z) >= 1.)
			call error (1, "Impossible velocity")
		    z = sqrt ((1 + z) / (1 - z)) - 1
		}
		if (z <= -1.)
		    call error (1, "Impossible redshift")

		if (fcor) {
		    fcval = (1 + z) ** ffac
		    if (in != out && IM_PIXTYPE(out) != TY_DOUBLE)
			IM_PIXTYPE(out) = TY_REAL
		}

		# Go through spectrum and apply corrections.
		switch (SMW_FORMAT(mw)) {
		case SMW_ND:
		    if (dcor) {
			call smw_gwattrs (mw, 1, 1, ap, beam, dtype,
			    w1, dw, nw, zold, aplow, aphigh, coeff)
			if (add)
			    znew = (1+z) * (1+zold) - 1
			else
			    znew = z
			call smw_swattrs (mw, 1, 1, ap, beam, dtype,
			    w1, dw, nw, znew, aplow, aphigh, Memc[coeff])
		    }

		    if (fcor || in != out) {
			do j = 1, IM_LEN(in,3) {
			    do i = 1, IM_LEN(in,2) {
				inbuf = imgl3r (in, i, j)
				outbuf = impl3r (out, i, j)
				if (fcor)
				    call amulkr (Memr[inbuf], fcval,
					Memr[outbuf], IM_LEN(in,1))
				else
				    call amovr (Memr[inbuf], Memr[outbuf],
					IM_LEN(in,1))
			    }
			}
		    }
		case SMW_ES, SMW_MS:
		    do i = 1, IM_LEN(in,2) {
			call shdr_open (in, mw, i, 1, INDEFI, SHHDR, sh)
			if (rng_elementi (aps, AP(sh))) {
			    wc = dcor
			    fc = fcor
			} else {
			    wc = false
			    fc = false
			}

			if (wc) {
			    call smw_gwattrs (mw, i, 1, ap, beam, dtype,
				w1, dw, nw, zold, aplow, aphigh, coeff)
			    if (add)
				znew = (1+z) * (1+zold) - 1
			    else
				znew = z
			    call smw_swattrs (mw, i, 1, ap, beam, dtype, w1,
				dw, nw, znew, aplow, aphigh, Memc[coeff])
			    if (mw != MW(sh)) {
				MW(sh) = NULL
				call shdr_close (sh)
			    }
			}

			# Correct fluxes
			# Note that if the operation is in-place we can skip
			# this step if there is no corrections.  Otherwise we
			# still have to copy the data even if there is no
			# correction.

			if (fc || in != out) {
			    do j = 1, IM_LEN(in,3) {
				call shdr_open (in, mw, i, j, INDEFI,
				    SHDATA, sh)
				outbuf = impl3r (out, i, j)
				if (fc)
				    call amulkr (Memr[SY(sh)], fcval,
					Memr[outbuf], SN(sh))
				else
				    call amovr (Memr[SY(sh)], Memr[outbuf],
					SN(sh))
				if (IM_LEN(out,1) > SN(sh))
				    call amovkr (Memr[outbuf+SN(sh)-1],
					Memr[outbuf+SN(sh)],
					IM_LEN(out,1)-SN(sh))
			    }
			}
		    }
		}

		# Document header.
		do i = 1, 98 {
		    call sprintf (Memc[key], SZ_FNAME, "DOPCOR%02d")
			call pargi (i)
		    iferr (call imgstr (out, Memc[key], Memc[log], SZ_LINE))
			break
		}
		if (fcor) {
		    call sprintf (Memc[log], SZ_LINE, "%8g %g %s")
		        if (isvel)
			    call pargd (zvel)
			else
			    call pargd (z)
			call pargr (ffac)
			call pargstr (Memc[apstr])
		} else {
		    call sprintf (Memc[log], SZ_LINE, "%8g %s")
		        if (isvel)
			    call pargd (zvel)
			else
			    call pargd (z)
			call pargstr (Memc[apstr])
		}
		call imastr (out, Memc[key], Memc[log])


		# Verbose output
		if (verbose) {
		    call printf ("%s: Doppler correction -")
			call pargstr (Memc[output])
		    if (SMW_FORMAT(mw) != SMW_ND) {
			call printf (" apertures=%s,")
			    call pargstr (Memc[apstr])
		    }
		    if (isvel) {
			call printf (" velocity=%8g,")
			    call pargd (zvel)
		    }
		    call printf (" redshift=%8g, flux factor=%g\n")
			call pargd (z)
			call pargr (ffac)
		    if (add && zold != 0.) {
			call printf ("    Correction added: %g + %g = %g\n")
			    call pargd (zold)
			    call pargd (z)
			    call pargd (znew)
		    }
		    call flush (STDOUT)
		}

	    } then {
		call erract (EA_WARN)
		if (out != NULL && out != in) {
		    call imunmap (out)
		    call imdelete (Memc[output])
		}
	    }

	    if (mw != NULL && out != NULL)
		call smw_saveim (mw, out)
	    if (sh != NULL)
		call shdr_close (sh)
	    if (mw != NULL)
		call smw_close (mw)
	    if (out != NULL && out != in)
		call imunmap (out)
	    if (in != NULL)
		call imunmap (in)
	}

	call rng_close (aps)
	call imtclose (inlist)
	call imtclose (outlist)
	call mfree (coeff, TY_CHAR)
	call sfree (sp)
end
