include	<error.h>
include	<imhdr.h>
include	<smw.h>

define	DEREDTYPES	"|A(V)|E(B-V)|c|"


# T_DEREDDEN -- Apply interstellar extinction correction to spectra.
# The extinction function is taken from Cardelli, Clayton, and Mathis,
# ApJ 345:245.  The input parameters are A(V)/E(B-V) and one of A(V),
# E(B-V), or c.

procedure t_deredden ()

pointer	inlist		# Input list
pointer	outlist		# Output list
real	av		# Extinction parameter: A(V), E(B-V), c
real	rv		# A(V)/E(B-V)

int	i, j, n
real	w, avold, rvold
pointer	sp, input, output, temp, log, aps
pointer	in, out, mw, sh, tmp, inbuf, outbuf

long	clktime()
real	clgetr()
double	shdr_lw()
bool	clgetb(), streq(), rng_elementi()
int	clgwrd(), imtgetim(), imtlen(), imaccf(), nscan(), strncmp(), ctor()
pointer	imtopenp(), rng_open(), immap(), smw_openim(), imgl3r(), impl3r()
errchk	immap, smw_openim, shdr_open, deredden, deredden1

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (temp, SZ_LINE, TY_CHAR)
	call salloc (log,  SZ_LINE, TY_CHAR)

	call cnvdate (clktime(0), Memc[log], SZ_LINE)

	# Get task parameters.
	inlist = imtopenp ("input")
	outlist = imtopenp ("output")
	call clgstr ("records", Memc[input], SZ_FNAME)
	call odr_openp (inlist, Memc[input])
	call odr_openp (outlist, Memc[input])

	av = clgetr ("value")
	rv = clgetr ("R")

	# Convert input extinction type to A(V)
	switch (clgwrd ("type", Memc[input], SZ_FNAME, DEREDTYPES)) {
	case 1:
	    call sprintf (Memc[log], SZ_LINE, "%s A(V)=%g R=%g")
		call pargstr (Memc[log])
		call pargr (av)
		call pargr (rv)
	case 2:
	    call sprintf (Memc[log], SZ_LINE, "%s E(B-V)=%g A(V)=%g R=%g")
		call pargstr (Memc[log])
		call pargr (av)
		call pargr (rv * av)
		call pargr (rv)
	    av = rv * av
	case  3:
	    call sprintf (Memc[log], SZ_LINE, "%s c=%g A(V)=%g R=%g")
		call pargstr (Memc[log])
		call pargr (av)
		call pargr (rv * av * (0.61 + 0.024 * av))
		call pargr (rv)
	    av = rv * av * (0.61 + 0.024 * av)
	}

	call clgstr ("apertures", Memc[temp], SZ_LINE)
	iferr (aps = rng_open (Memc[temp], INDEF, INDEF, INDEF))
	    call error (0, "Bad aperture list")
	if (Memc[temp] != EOS) {
	    call sprintf (Memc[log], SZ_LINE, "%s ap=%s")
		call pargstr (Memc[log])
		call pargstr (Memc[temp])
	}

	# Loop over all input images.
	in = NULL
	out = NULL
	mw = NULL
	sh = NULL
	while (imtgetim (inlist, Memc[input], SZ_FNAME) != EOF) {
	    if (imtlen (outlist) > 0) {
		if (imtgetim (outlist, Memc[output], SZ_FNAME) == EOF)
		    break
	    } else
		call strcpy (Memc[input], Memc[output], SZ_FNAME)

	    iferr {
		# Map the image and check its calibration status.
		tmp = immap (Memc[input], READ_ONLY, 0); in = tmp
		tmp = smw_openim (in); mw = tmp

		call shdr_open (in, mw, 1, 1, INDEFI, SHHDR, sh)
		if (DC(sh) == DCNO) {
		    call sprintf (Memc[temp], SZ_LINE,
			"[%s] has no dispersion function")
			call pargstr (Memc[input])
		    call error (1, Memc[temp])
		}
		call shdr_units (sh, "angstroms")

		rvold = rv
		avold = 0.
		if (imaccf (in, "DEREDDEN") == YES) {
		    if (!clgetb ("override")) {
			call sprintf (Memc[temp], SZ_LINE,
			    "[%s] has already been corrected")
			call pargstr (Memc[input])
			call error (1, Memc[temp])
		    } else {
			if (clgetb ("uncorrect")) {
			    call imgstr (in, "DEREDDEN", Memc[temp], SZ_LINE)
			    call sscan (Memc[temp])
			    for (i=1;; i=i+1) {
				call gargwrd (Memc[temp], SZ_LINE)
				if (nscan() < i)
				    break
				if (strncmp (Memc[temp], "A(V)=", 5) == 0) {
				    j = 6
				    j = ctor (Memc[temp], j, avold)
				} else if (strncmp (Memc[temp], "R=", 2) == 0) {
				    j = 3
				    j = ctor (Memc[temp], j, rvold)
				}
			    }
			}
		    }
		}

		# Map the output image.
		if (streq (Memc[input], Memc[output]))
		    call mktemp ("temp", Memc[temp], SZ_LINE)
		else
		    call strcpy (Memc[output], Memc[temp], SZ_LINE)
		tmp = immap (Memc[temp], NEW_COPY, in); out = tmp
		if (IM_PIXTYPE(out) != TY_DOUBLE)
		    IM_PIXTYPE(out) = TY_REAL
		call imastr (out, "DEREDDEN", Memc[log])

		# Initialize for NDSPEC data.
		if (SMW_FORMAT(mw) == SMW_ND) {
		    if (SX(sh) == NULL)
			call malloc (SX(sh), SN(sh), TY_REAL)
		    else
			call realloc (SX(sh), SN(sh), TY_REAL)
		    do i = 1, SN(sh)
			Memr[SX(sh)+i-1] = shdr_lw (sh, double(i))
		}

		# Log operation.
		call printf ("[%s]: %s\n  %s\n")
		    call pargstr (Memc[output])
		    call pargstr (IM_TITLE(in))
		    call pargstr (Memc[log])

		# Deredden data.
		n = IM_LEN(in,1)
		do j = 1, IM_LEN(in,3) {
		    do i = 1, IM_LEN(in,2) {
			outbuf = impl3r (out, i, j)
			switch (SMW_FORMAT(mw)) {
			case SMW_ND:
			    inbuf = imgl3r (in, i, j)
			    switch (SMW_LAXIS(mw,1)) {
			    case 1:
				call deredden (Memr[SX(sh)], Memr[inbuf],
				    Memr[outbuf], SN(sh), av, rv, avold, rvold)
			    case 2:
				w = Memr[SX(sh)+i-1]
				call deredden1 (w, Memr[inbuf], Memr[outbuf],
				    n, av, rv, avold, rvold)
			    case 3:
				w = Memr[SX(sh)+j-1]
				call deredden1 (w, Memr[inbuf], Memr[outbuf],
				    n, av, rv, avold, rvold)
			    }
			case SMW_ES, SMW_MS:
			    call shdr_open (in, mw, i, j, INDEFI, SHDATA, sh)
			    if (rng_elementi (aps, AP(sh))) {
				if (j==1) {
				    call printf ("  Ap %d: %s\n")
					call pargi (AP(sh))
					call pargstr (TITLE(sh))
				}
				call deredden (Memr[SX(sh)], Memr[SY(sh)],
				    Memr[outbuf], SN(sh), av, rv, avold, rvold)
			    } else
				call amovr (Memr[SY(sh)], Memr[outbuf], SN(sh))
			    if (IM_LEN(out,1) > SN(sh))
				call amovkr (Memr[SY(sh)+SN(sh)-1],
				    Memr[outbuf+SN(sh)], IM_LEN(out,1)-SN(sh))
			}
		    }
		}
	    } then {
		call erract (EA_WARN)
		if (out != NULL) {
		    call imunmap (out)
		    call imdelete (Memc[temp])
		}
	    }

	    if (mw != NULL) {
		if (MW(sh) == mw)
		    call smw_close (MW(sh))
		else
		    call smw_close (mw)
	    }
	    if (out != NULL) {
		call imunmap (out)
		call imunmap (in)
		if (streq (Memc[input], Memc[output])) {
		    call imdelete (Memc[input])
		    call imrename (Memc[temp], Memc[output])
		}
	    } else if (in != NULL)
		call imunmap (in)
	}

	call shdr_close (sh)
	call rng_close (aps)
	call imtclose (inlist)
	call imtclose (outlist)
	call sfree (sp)
end


# DEREDDEN -- Deredden spectrum

procedure deredden (x, y, z, n, av, rv, avold, rvold)

real	x[n]			# Wavelengths
real	y[n]			# Input fluxes
real	z[n]			# Output fluxes
int	n			# Number of points
real	av, avold		# A(V)
real	rv, rvold		# A(V)/E(B-V)

int	i
real	cor, ccm()
errchk	ccm

begin
	if (avold != 0.) {
	    if (rv != rvold) {
		do i = 1, n {
		    cor = 10. ** (0.4 *
			(av * ccm (x[i], rv) - avold * ccm (x[i], rvold)))
		    z[i] = y[i] * cor
		}
	    } else {
		do i = 1, n {
		    cor = 10. ** (0.4 * (av - avold) * ccm (x[i], rv))
		    z[i] = y[i] * cor
		}
	    }
	} else {
	    do i = 1, n {
		cor = 10. ** (0.4 * av * ccm (x[i], rv))
		z[i] = y[i] * cor
	    }
	}
end


# DEREDDEN1 -- Deredden fluxes at a single wavelength

procedure deredden1 (x, y, z, n, av, rv, avold, rvold)

real	x			# Wavelength
real	y[n]			# Input fluxes
real	z[n]			# Output fluxes
int	n			# Number of points
real	av, avold		# A(V)
real	rv, rvold		# A(V)/E(B-V)

int	i
real	cor, ccm()
errchk	ccm

begin
	if (avold != 0.) {
	    if (rv != rvold)
		cor = 10. ** (0.4 *
		    (av * ccm (x, rv) - avold * ccm (x, rvold)))
	    else
		cor = 10. ** (0.4 * (av - avold) * ccm (x, rv))
	} else
	    cor = 10. ** (0.4 * av * ccm (x, rv))
	do i = 1, n
	    z[i] = y[i] * cor
end


# CCM -- Compute CCM Extinction Law

real procedure ccm (wavelength, rv) 

real	wavelength		# Wavelength in Angstroms
real	rv			# A(V) / E(B-V)

real	x, y, a, b

begin
	# Convert to inverse microns
	x = 10000. / wavelength

	# Compute a(x) and b(x)
	if (x < 0.3) {
	    call error (1, "Wavelength out of range of extinction function")

	} else if (x < 1.1) {
	    y = x ** 1.61
	    a = 0.574 * y
	    b = -0.527 * y

	} else if (x < 3.3) {
	    y = x - 1.82
	    a = 1 + y * (0.17699 + y * (-0.50447 + y * (-0.02427 +
		y * (0.72085 + y * (0.01979 + y * (-0.77530 + y * 0.32999))))))
	    b = y * (1.41338 + y * (2.28305 + y * (1.07233 + y * (-5.38434 +
		y * (-0.62251 + y * (5.30260 + y * (-2.09002)))))))

	} else if (x < 5.9) {
	    y = (x - 4.67) ** 2
	    a = 1.752 - 0.316 * x - 0.104 / (y + 0.341)
	    y = (x - 4.62) ** 2
	    b = -3.090 + 1.825 * x + 1.206 / (y + 0.263)

	} else if (x < 8.0) {
	    y = (x - 4.67) ** 2
	    a = 1.752 - 0.316 * x - 0.104 / (y + 0.341)
	    y = (x - 4.62) ** 2
	    b = -3.090 + 1.825 * x + 1.206 / (y + 0.263)

	    y = x - 5.9
	    a = a - 0.04473 * y**2 - 0.009779 * y**3
	    b = b + 0.2130 * y**2 + 0.1207 * y**3

	} else if (x <= 10.0) {
	    y = x - 8
	    a = -1.072 - 0.628 * y + 0.137 * y**2 - 0.070 * y**3
	    b = 13.670 + 4.257 * y - 0.420 * y**2 + 0.374 * y**3

	} else {
	    call error (1, "Wavelength out of range of extinction function")

	}

	# Compute A(lambda)/A(V)
	y = a + b / rv
	return (y)
end
