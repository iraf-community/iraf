include	<error.h>
include	<imhdr.h>
include "shdr.h"

define	EXTN_LOOKUP	10	# Interp index for de-extinction
define	VLIGHT	2.997925e5	# Speed of light, Km/sec

# T_DOPCOR -- Apply doppler correction to spectra.

procedure t_dopcor ()

int	inlist			# List of input spectra
int	outlist			# List of output spectra
double	z			# Doppler redshift or velocity
bool	isvel			# Is redshift parameter a velocity?
bool	dcor			# Apply dispersion correction?
bool	fcor			# Apply flux correction?
real	ffac			# Flux correction factor (power of 1+z)
pointer	aps			# Apertures
bool	verbose			# Verbose?

real	fcval
bool	wc, fc
int	i, j, ap, beam, nw, dtype
double	w1, dw, zold, aplow,aphigh
pointer	ptr, in, out, mw, sh
pointer	sp, input, output, vkey, apstr, key, coeff

real	clgetr()
double	imgetd()
bool	clgetb(), streq(), is_in_range()
int	imtopenp(), imtgetim(), decode_ranges(), ctod()
pointer	immap(), smw_openim(), impl3r()
errchk	immap, smw_openim, shdr_open, shdr_gwattrs, imgetd, imgstr, impl3r

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (vkey, SZ_FNAME, TY_CHAR)
	call salloc (apstr, SZ_FNAME, TY_CHAR)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (aps, 300, TY_INT)
	coeff = NULL

	# Parameters
	inlist = imtopenp ("input")
	outlist = imtopenp ("output")
	call clgstr ("redshift", Memc[vkey], SZ_FNAME)
	isvel = clgetb ("isvelocity")
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
	if (decode_ranges (Memc[apstr], Memi[aps], 100, i) == ERR)
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

		# Map input and output images
		if (streq (Memc[input], Memc[output])) {
		    ptr = immap (Memc[input], READ_WRITE, 0); in = ptr
		    out = in
		} else {
		    ptr = immap (Memc[input], READ_ONLY, 0); in = ptr
		    ptr = immap (Memc[output], NEW_COPY, in); out = ptr
		}

		# Set velocity and flux correction
		i = 1
		if (Memc[vkey] == '-' || Memc[vkey] == '+') {
		    if (ctod (Memc[vkey+1], i, z) == 0)
			z = imgetd (in, Memc[vkey+1])
		    if (Memc[vkey] == '-')
			z = -z
		} else {
		    if (ctod (Memc[vkey], i, z) == 0)
			z = imgetd (in, Memc[vkey])
		}
		if (isvel) {
		    z = z / VLIGHT
		    z = sqrt ((1 - z) / (1 + z)) - 1
		}
		if (fcor)
		    fcval = (1 + z) ** ffac

		# Check format
		ptr = smw_openim (in); mw = ptr
		call shdr_open (in, mw, i, 1, INDEFI, SHHDR, sh)
		if (FORMAT(sh) != MULTISPEC)
		    call error (1, "Spectrum must be in multispec format")

		# Go through lines and apply corrections to selected apertures
		do i = 1, IM_LEN(in,2) {
		    call shdr_open (in, mw, i, 1, INDEFI, SHHDR, sh)
		    if (is_in_range (Memi[aps], AP(sh))) {
			wc = dcor
			fc = fcor
		    } else {
			wc = false
			fc = false
		    }

		    # Reset wcs
		    if (wc) {
			call shdr_gwattrs (mw, PINDEX1(sh), ap, beam, dtype,
			    w1, dw, nw, zold, aplow, aphigh, coeff)
			call shdr_swattrs (mw, PINDEX1(sh), ap, beam, dtype,
			    w1, dw, nw, z, aplow, aphigh, Memc[coeff])
		    }

		    # Correct fluxes
		    # Note that if the operation is in-place we can skip this
		    # step if there is no corrections.  Otherwise we still
		    # have copy the data even if there is no correction.

		    if (!fc && in == out)
			next

		    do j = 1, IM_LEN(in,3) {
			call shdr_open (in, mw, i, j, INDEFI, SHDATA, sh)
			if (fc)
			    call amulkr (Memr[SY(sh)], fcval, Memr[SY(sh)],
				SN(sh))
			call amovr (Memr[SY(sh)], Memr[impl3r(out,i,j)], SN(sh))
			if (SN(sh) < IM_LEN(out,1))
			    call aclrr (Memr[impl3r(out,i,j)+SN(sh)],
				IM_LEN(out,1)-SN(sh))
		    }
		}

		# Verbose output
		if (verbose) {
		    call printf ("%s: Doppler correction - apertures=%s, ")
			call pargstr (Memc[output])
			call pargstr (Memc[apstr])
		    call printf ("redshift=%8g, flux factor=%g\n")
			call pargd (z)
			call pargr (ffac)
		    call flush (STDOUT)
		}

	    } then {
		if (out != NULL && out != in) {
		    call imunmap (out)
		    call imdelete (Memc[output])
		}
		call erract (EA_WARN)
	    }

	    if (mw != NULL) {
		call smw_saveim (mw, out)
		call mw_close (mw)
	    }
	    if (out != NULL && out != in)
		call imunmap (out)
	    if (in != NULL)
		call imunmap (in)
	}

	call shdr_close (sh)
	call imtclose (inlist)
	call imtclose (outlist)
	call mfree (coeff, TY_CHAR)
	call sfree (sp)
end
