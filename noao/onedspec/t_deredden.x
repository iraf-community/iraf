include	<error.h>
include	<imhdr.h>
include "shdr.h"

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

bool	dered
int	i, j
pointer	sp, input, output, temp, log, aps
pointer	in, out, mw, sh, tmp, data

long	clktime()
real	clgetr()
bool	clgetb(), streq(), is_in_range()
int	clgwrd(), imtgetim(), imtlen(), imaccf(), decode_ranges()
pointer	imtopenp(), immap(), smw_openim(), impl3r()
errchk	immap, smw_openim, shdr_open, deredden

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (temp, SZ_LINE, TY_CHAR)
	call salloc (log,  SZ_LINE, TY_CHAR)
	call salloc (aps, 300, TY_INT)

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
	if (decode_ranges (Memc[temp], Memi[aps], 100, i) == ERR)
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
		call shdr_open (in, mw, 1, 1, INDEFI, SHDATA, sh)
	    
		if (DC(sh) == DCNO) {
		    call sprintf (Memc[temp], SZ_LINE,
			"[%s] has no dispersion function")
			call pargstr (Memc[input])
		    call error (1, Memc[temp])
		}

		if (!clgetb ("override")) {
		    if (imaccf (in, "DEREDDEN") == YES) {
			call sprintf (Memc[temp], SZ_LINE,
			    "[%s] has already been corrected")
			call pargstr (Memc[input])
			call error (1, Memc[temp])
		    }
		}

		# Map the output image.
		if (streq (Memc[input], Memc[output]))
		    call mktemp ("temp", Memc[temp], SZ_LINE)
		else
		    call strcpy (Memc[output], Memc[temp], SZ_LINE)
		tmp = immap (Memc[temp], NEW_COPY, in); out = tmp
		IM_PIXTYPE(out) = TY_REAL
		call imastr (out, "DEREDDEN", Memc[log])

		do i = 1, IM_LEN(out,2) {
		    call shdr_open (in, mw, i, 1, INDEFI, SHDATA, sh)

		    if (is_in_range (Memi[aps], AP(sh)))
			dered = true
		    else
			dered = false
			
		    if (dered) {
			call printf ("[%s][%d]: %s\n  %s\n")
			    call pargstr (Memc[output])
			    call pargi (AP(sh))
			    call pargstr (TITLE(sh))
			    call pargstr (Memc[log])
		    }

		    do j = 1, IM_LEN(out,3) {
			call shdr_open (in, mw, i, j, INDEFI, SHDATA, sh)
			if (dered) {
			    call deredden (Memr[SX(sh)], Memr[SY(sh)], SN(sh),
				av, rv)
			}
			data = impl3r (out, i, j)
			call amovr (Memr[SY(sh)], Memr[data], SN(sh))
			if (IM_LEN(out,1) > SN(sh))
			    call amovkr (Memr[SY(sh)+SN(sh)-1],
				Memr[data+SN(sh)-1], IM_LEN(out,1)-SN(sh))
		    }
		}
	    } then {
		if (out != NULL) {
		    call imunmap (out)
		    call imdelete (Memc[temp])
		}
		call erract (EA_WARN)
	    }

	    if (mw != NULL)
		call mw_close (mw)
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
	call imtclose (inlist)
	call imtclose (outlist)
	call sfree (sp)
end


# DEREDDEN -- Deredden spectrum

procedure deredden (x, y, n, av, rv)

real	x[n]			# Wavelengths
real	y[n]			# Fluxes
int	n			# Number of points
real	av			# A(V)
real	rv			# A(V)/E(B-V)

int	i
real	z, ccm()
errchk	ccm

begin
	do i = 1, n {
	    z = av * ccm (x[i], rv)
	    y[i] = y[i] * 10. ** (0.4 * z)
	}
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
		y * (-0.62251 + y * (5.30260 + y * -2.09002))))))

	} else if (x < 5.9) {
	    y = (x - 4.67) ** 2
	    a = 1.752 - 0.316 * x - 0.104 / (y + 0.341)
	    b = -3.090 + 1.825 * x + 1.206 / (y + 0.263)

	} else if (x < 8.0) {
	    y = (x - 4.67) ** 2
	    a = 1.752 - 0.316 * x - 0.104 / (y + 0.341)
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
