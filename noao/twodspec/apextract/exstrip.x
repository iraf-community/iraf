include	<error.h>
include	<imhdr.h>
include	"exio.h"
include	"apertures.h"
include	"extract.h"

define	EX_MAX	5	# Maximum number of simultaneous extractions

# EX_STRIP -- Extract 2D aperture strips.

procedure ex_strip (input, output, profiles, aps, naps)

char	input[SZ_FNAME]		# Input image
char	output[SZ_FNAME]	# Output image root name
char	profiles[SZ_FNAME]	# Profile reference image
pointer	aps[ARB]		# Apertures
int	naps			# Number of apertures

int	i, ap1, ap2, nap, ex_max, alen, dlen, aaxis
pointer	in, immod, apsmod, sp, str, ex, bufout, bufouts, alens, dlens

int	clgeti(), clginterp(), btoi(), clgwrd()
real	clgetr()
bool	clgetb(), streq()
pointer	ex_map()

errchk	ex_map, ex_gmodaps

begin
	if (naps == 0) {
	    call eprintf ("APSTRIP - No apertures defined for %s\n")
		call pargstr (input)
	    return
	}

	# Map the input image.
	in = ex_map (input)
	aaxis = EX_AAXIS (in)

	# Get extraction parameters.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (ex, EX_LENSTRUCT, TY_STRUCT)

	EX_ONED(ex) = NO
	if (clgetb ("apstrip.fit"))
	    EX_WTTYPE(ex) = VARIANCE
	else
	    EX_WTTYPE(ex) = PROFILE
	EX_BKGD(ex) = clgwrd ("apstrip.background", Memc[str], SZ_LINE,
	    BACKGROUND)
	EX_CLEAN(ex) = btoi (clgetb ("apstrip.clean"))
	EX_NAVG(ex) = clgeti ("apstrip.naverage") + 1
	EX_NCLEAN(ex) = clgeti ("apstrip.nclean")
	EX_LSIGMA(ex) = clgetr ("apstrip.lsigma")
	EX_USIGMA(ex) = clgetr ("apstrip.usigma")
	EX_V0(ex) = clgetr ("apstrip.v0")
	EX_V1(ex) = clgetr ("apstrip.v1")
	call asiinit (EX_ASI(ex), clginterp ("apstrip.interpolator"))

	# If using profile fitting get the profile image and apertures.
	immod = in
	if ((EX_WTTYPE(ex) == VARIANCE) || (EX_CLEAN(ex) == YES)) {
	    call salloc (apsmod, AP_MAXAPS, TY_INT)
	    if (profiles[1] == EOS || streq (profiles, input)) {
	        do i = 1, naps
		    Memi[apsmod+i-1] = aps[i]
	    } else {
	        immod = ex_map (profiles)
		call ex_gmodaps (profiles, aps, Memi[apsmod], naps)
	    }
	}

	# Allocate memory for the extracted strips.  To conserve memory
	# limit the number of strips.

	ex_max = EX_MAX
	call salloc (bufouts, min (ex_max, naps), TY_INT)
	call salloc (alens, min (ex_max, naps), TY_INT)
	call salloc (dlens, min (ex_max, naps), TY_INT)

	if (clgetb ("apio.verbose")) {
	    call printf ("Extracting apertures ...\n")
	    call flush (STDOUT)
	}

	do ap1 = 1, naps, ex_max {
	    ap2 = min (naps, ap1 + ex_max - 1)
	    nap = ap2 - ap1 + 1

	    do i = 1, nap {
		alen = nint (AP_HIGH(aps[ap1+i-1],aaxis)) -
		    nint (AP_LOW(aps[ap1+i-1],aaxis)) + 1
		alen = min (alen, EX_ALEN(in))
		dlen = EX_DLEN(in)
	        call malloc (bufout, alen * dlen, TY_REAL)
		Memi[bufouts+i-1] = bufout
	        Memi[alens+i-1] = alen
	        Memi[dlens+i-1] = dlen
	    }

	    call ex_strip1 (ex, in, immod, aps[ap1],
		Memi[apsmod+ap1-1], nap, Memi[bufouts], Memi[alens],
		Memi[dlens])
	    call ex_stripout (input, output, in, aps, naps, ap1,
		nap, Memi[bufouts], Memi[alens], Memi[dlens])

	    do i = 1, nap
		call mfree (Memi[bufouts+i-1], TY_REAL)
	}

	if (immod != in) {
	    call ex_unmap (immod)
	    do i = 1, naps {
		if (Memi[apsmod+i-1] != aps[i])
		    call ap_free (Memi[apsmod+i-1])
	    }
	}

	call asifree (EX_ASI(ex))
	call ex_unmap (in)
	call sfree (sp)
end


# EX_STRIP1 -- Extract 2D aperture strips.
# This procedure does most of the work of getting the profiles from the
# input image, doing any shifting and model fitting, and extracting the
# profiles centered in the strip.

procedure ex_strip1 (ex, in, immod, aps, apsmod, naps, bufout, alen, dlen)

pointer	ex			# Pointer to extraction parameters
pointer	in			# Input IMIO pointer
pointer	immod			# Model IMIO pointer
pointer	aps[naps]		# Apertures
pointer	apsmod[naps]		# Model apertures
int	naps			# Number of apertures
pointer	bufout[naps]		# Pointers for output strips
int	alen[naps]		# Width of output strip
int	dlen[naps]		# Length of output strip

int	ncols			# Length along aperture axis
int	nlines			# Number of points

int	i, j, nextract, line, len_profs, replace, apaxis
real	center, low, high
pointer	sp, data, model, avg, pstart, pcen, pend, pnrep, bckgrnd
pointer	bufin, buf

real	cveval()
pointer	ex_g2r()

begin
	call smark (sp)
	apaxis = EX_AAXIS(in)
	ncols = EX_ALEN(in)
	nlines = EX_DLEN(in)

	# Select profile fitting if needed.
	if ((EX_WTTYPE(ex) == VARIANCE) || (EX_CLEAN(ex) == YES)) {
	    # Set the end points of the profile in the profile array
	    call salloc (pstart, naps, TY_INT)
	    call salloc (pcen, naps, TY_REAL)
	    call salloc (pend, naps, TY_INT)
	    call salloc (pnrep, naps, TY_INT)
	    j = 0
	    do i = 1, naps {
		nextract = nint (AP_HIGH(aps[i],apaxis)) -
		    nint (AP_LOW(aps[i],apaxis)) + 1
		nextract = min (nextract, ncols) + 2

		Memi[pstart+i-1] = j + 1
	        Memi[pend+i-1] = j + nextract
		j = j + nextract
	    }
	    len_profs = j

	    # Allocate working memory for the profiles.
	    call salloc (data, len_profs, TY_REAL)
	    call salloc (model, len_profs, TY_REAL)
	    call salloc (bckgrnd, len_profs, TY_REAL)
	    if (EX_NAVG(ex) > nlines) {
		# Compute average profile.
	        call salloc (avg, len_profs, TY_REAL)
		call aclrr (Memr[avg], len_profs)
		do line = 1, nlines {
	            call ex_gprofs (ex, immod, line, apsmod, naps,
			Memi[pstart], Memi[pcen], Memi[pend], YES, Memr[model],
			Memr[bckgrnd])
		    call aaddr (Memr[model], Memr[avg], Memr[avg], len_profs)
		}
	    } else if (EX_NAVG(ex) > 1) {
	        call salloc (avg, len_profs * (EX_NAVG(ex) + 1), TY_REAL)
		if (immod == in && EX_CLEAN(ex) == YES)
		    replace = YES
		else
		    replace = NO
	    }

	    # Extract each line of the apertures.
	    do line = 1, nlines {
		do i = 1, naps {
		    center = AP_CEN(aps[i],apaxis) +
			cveval (AP_CV(aps[i]), real (line))
		    low = nint (center + AP_LOW(aps[i],apaxis))
		    Memr[pcen+i-1] = center - low + Memi[pstart+i-1] + 1
		}

	        # Get the data and model spectra.  Variance weighting is
	        # implemented by extracting the model spectrum.
		if (EX_NAVG(ex) > nlines) {
	            call ex_recen (ex, Memi[pstart], Memr[pcen], Memi[pend],
			naps, Memr[avg], Memr[model], 1)
		} else if (EX_NAVG(ex) > 1) {
		    if (in == immod)
	                call ex_mvsum2 (ex, immod, line, apsmod, naps,
			    Memi[pstart], Memi[pcen], Memi[pend], Memr[avg],
			    Memr[bckgrnd], len_profs)
		    else 
	                call ex_mvsum1 (ex, immod, line, apsmod, naps,
			    Memi[pstart], Memi[pcen], Memi[pend], Memr[avg],
			    Memr[bckgrnd], len_profs)
	            call ex_recen (ex, Memi[pstart], Memr[pcen], Memi[pend],
			naps, Memr[avg], Memr[model], 1)
		} else {
	            call ex_gprofs (ex, immod, line, apsmod, naps,
			Memi[pstart], Memr[pcen], Memi[pend], NO, Memr[model],
			Memr[bckgrnd])
		}
	        call ex_gprofs (ex, in, line, aps, naps, Memi[pstart],
		    Memr[pcen], Memi[pend], NO, Memr[data], Memr[bckgrnd])

		call ex_fit (ex, line, Memi[pstart], Memi[pend], Memi[pnrep],
		    naps, Memr[data], Memr[bckgrnd], Memr[model])
		if (replace == YES)
		    call ex_replace (ex, line, Memi[pstart], Memr[pcen],
			Memi[pend], Memi[pnrep], naps, Memr[data], Memr[avg],
			len_profs)
	        if (EX_WTTYPE(ex) == VARIANCE)
	            call ex_recen (ex, Memi[pstart], Memr[pcen], Memi[pend],
		        naps, Memr[model], Memr[data], -1)
		else
	            call ex_recen (ex, Memi[pstart], Memr[pcen], Memi[pend],
		        naps, Memr[data], Memr[data], -1)

		# Extract two dimensional strip.
	        do i = 1, naps {
		    if (Memi[pstart] == 0)
			next

		    j = Memi[pstart+i-1] + 1
		    buf = bufout[i] + (line - 1) * alen[i]
		    call amovr (Memr[data+j-1], Memr[buf], alen[i])
	        }
	    }

	} else {
	    # Extract each line of the apertures.
	    j = 0
	    do i = 1, naps
		j = max (alen[i], j)
	    call salloc (bckgrnd, j, TY_REAL)

	    do line = 1, nlines {
		bufin = ex_g2r (in, line)
		do i = 1, naps {
		    center = AP_CEN(aps[i],apaxis) +
			cveval (AP_CV(aps[i]), real (line))
		    low = center + AP_LOW(aps[i],apaxis)
		    high = center + AP_HIGH(aps[i],apaxis)

		    buf = bufout[i] + (line - 1) * alen[i]
		    call ex_apstrip (Memr[bufin], ncols, low, center, high,
			EX_ASI(ex), AP_IC(aps[i]), EX_BKGD(ex), Memr[buf],
			Memr[bckgrnd], alen[i])
		}
	    }
	}

	call sfree (sp)
end


# EX_STRIPOUT -- Output the extracted spectra.

procedure ex_stripout (image, output, im, aps, naps, ap1, naps1,
	data, alen, dlen)

char	image[ARB]		# Input image name
char	output[ARB]		# Output root name
pointer	im			# Input EXIO pointer
pointer	aps[naps]		# Apertures
int	naps			# Number of apertures
int	ap1			# Starting aperture
int	naps1			# Number of apertures to output
pointer	data[naps1]		# Output data
int	alen[naps1], dlen[naps1]	# Number of points

int	i, j, k, l, aaxis, daxis
pointer	sp, str, name, in, out, bufin, bufout

pointer	immap(), impl2r()
errchk	immap

begin
	# Allocate string and file name arrays.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (name, SZ_FNAME, TY_CHAR)

	# Write out the extracted spectra.
	do i = ap1, ap1+naps1-1 {
	    j = i - ap1 + 1
	    if (output[1] == EOS) {
	        call sprintf (Memc[name], SZ_FNAME, "%s.%04d")
	            call pargstr (image)
	            call pargi (AP_ID(aps[i]))
	    } else {
	        call sprintf (Memc[name], SZ_FNAME, "%s.%04d")
	            call pargstr (output)
	            call pargi (AP_ID(aps[i]))
	    }
	    
	    # Output the image.
	    in = EX_IM(im)
	    aaxis = EX_AAXIS(im)
	    daxis = EX_DAXIS(im)

	    iferr {
		out = immap (Memc[name], NEW_COPY, in)
		IM_PIXTYPE(out) = TY_REAL
		IM_LEN(out, AAXIS) = alen[j]
		IM_LEN(out, DAXIS) = dlen[j]
		call sprintf (Memc[str], SZ_LINE, "%s - Aperture %d")
		    call pargstr (IM_TITLE(out))
		    call pargi (AP_ID(aps[i]))
		call strcpy (Memc[str], IM_TITLE(out), SZ_IMTITLE)
		call imaddi (out, "apnum1", AP_BEAM(aps[i]))
		call imaddi (out, "beam-num", AP_BEAM(aps[i]))

		switch (aaxis) {
		case 1:
		    do k = 1, dlen[j] {
			bufin = data[j] + (k - 1) * alen[j]
			bufout = impl2r (out, k)
	                call amovr (Memr[bufin], Memr[bufout], alen[j])
		    }
		case 2:
		    do l = 1, alen[j] {
			bufin = data[j] + l - 1
			bufout = impl2r(out, l)
			do k = 1, dlen[j] {
			    Memr[bufout] = Memr[bufin]
			    bufout = bufout + 1
			    bufin = bufin + alen[j]
			}
		    }
		}
	        call imunmap (out)

	        call sprintf (Memc[str], SZ_LINE,
	            "APSUM   - Aperture %d from %s --> %s")
		    call pargi (AP_ID(aps[i]))
       	            call pargstr (image)
		    call pargstr (Memc[name])
	        call ap_log (Memc[str])

	    } then
		call erract (EA_WARN)
	}
end
