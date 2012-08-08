include	<error.h>
include	<imhdr.h>
include	<mach.h>
include	<math.h>
include	<math/curfit.h>
include	<math/iminterp.h>
include	"specfocus.h"


# T_SPECFOCUS -- Spectral focusing task

procedure t_specfocus ()

int	list		# List of images
pointer	fvals		# List of focus values
int	dispaxis	# Default dispersion axis
int	amin		# Lower edge of data along slit
int	amax		# Upper edge of data along slit
int	nspec		# Number of spectra to subdivide width
int	ndisp		# Number of dispersion samples
int	lag		# Maximum lag
real	level		# Level for width
bool	shifts		# Measure shifts?
int	log		# Log file descriptor

int	i, j, k, l, nimages, npix, nprep
int	aaxis, a1, da, na
int	baxis, b1, db, nb
int	c1, dc, nc
int	l1, dl, nl
pointer	sp, image, sys
pointer	rg, sfs, sf, sfd, sfavg, sfbest, im, mw, data, buf1, buf2
pointer	rng_open(), immap(), imgl2r(), mw_openim()
int	clgeti(), imgeti(), imtopenp(), imtlen(), imtgetim(), nowhite()
int	rng_index(), open()
real	rval, clgetr(), imgetr(), asumr()
bool	ms, clgetb(), streq()
errchk	immap, spf_width

int	spf_compare()
extern	spf_compare

begin
	call smark (sp)
	call salloc (fvals, SZ_LINE, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (sys, SZ_FNAME, TY_CHAR)

	# Get task parameters (except log file)
	list = imtopenp ("images")
	call clgstr ("focus", Memc[fvals], SZ_LINE)
	dispaxis = clgeti ("dispaxis")
	amin = clgeti ("slit1")
	amax = clgeti ("slit2")
	nspec = clgeti ("nspectra")
	ndisp = clgeti ("ndisp")
	lag = (clgeti ("corwidth") + 1) / 2
	level = clgetr ("level")
	shifts = clgetb ("shifts")

	if (level > 1.)
	    level = level / 100.
	level = max (0.05, min (0.95, level))

	# Initialize focus values
	if (nowhite (Memc[fvals], Memc[fvals], SZ_LINE) == 0)
	    call strcpy ("1x1", Memc[fvals], SZ_LINE)
	iferr (rg = rng_open (Memc[fvals], -MAX_REAL, MAX_REAL, 1.))
	    rg = NULL

	# Allocate array for the image focus data structure pointers
	nimages = imtlen (list)
	call malloc (sfs, nimages, TY_POINTER)

	# Accumulate the focus data
	nimages = 0
	while (imtgetim (list, Memc[image], SZ_FNAME) != EOF) {
	    im = immap (Memc[image], READ_ONLY, 0)
	    mw = mw_openim (im)
	    call mw_gsystem (mw, Memc[sys], SZ_FNAME)
	    ms = streq (Memc[sys], "multispec")
	    call mw_close (mw)

	    # Set the focus value
	    if (rg != NULL) {
		if (rng_index (rg, nimages+1, rval) == EOF)
		    call error (1, "Focus list ended prematurely")
	    } else
		rval = imgetr (im, Memc[fvals])

	    # Set dispersion and cross dispersion axes
	    if (ms) {
		baxis = 1
		aaxis = 2

		# Set sampling across the dispersion axis
		if (IS_INDEFI (amin))
		    i = 1
		else
		    i = amin
		if (IS_INDEFI (amax))
		    j = IM_LEN(im,aaxis)
		else
		    j = amax
		a1 = max (1, min (i, j))
		da = min (IM_LEN(im,aaxis), max (i, j)) - a1 + 1
		if (da < 1)
		    call error (1, "Error in slit limits")
		na = da
		da = da / na

		# Set sampling along the dispersion axis
		npix = IM_LEN(im,baxis)
		nb = min (ndisp, npix / 100)
		db = npix / nb
		b1 = 1 + (npix - nb * db) / 2

		# Set sampling along the columns and lines
		c1 = b1; dc = db; nc = nb
		l1 = a1; dl = da; nl = na
	    } else {
		iferr (baxis = imgeti (im, "dispaxis"))
		    baxis = dispaxis
		aaxis = 3 - baxis

		# Set sampling across the dispersion axis
		if (IS_INDEFI (amin))
		    i = 1
		else
		    i = amin
		if (IS_INDEFI (amax))
		    j = IM_LEN(im,aaxis)
		else
		    j = amax
		a1 = max (1, min (i, j))
		da = min (IM_LEN(im,aaxis), max (i, j)) - a1 + 1
		if (da < 1)
		    call error (1, "Error in slit limits")
		na = min (nspec, da)
		da = da / na

		# Set sampling along the dispersion axis
		npix = IM_LEN(im,baxis)
		nb = min (ndisp, npix / 100)
		db = npix / nb
		b1 = 1 + (npix - nb * db) / 2

		# Set sampling along the columns and lines
		if (baxis == 1) {
		    c1 = b1; dc = db; nc = nb
		    l1 = a1; dl = da; nl = na
		} else {
		    c1 = a1; dc = da; nc = na
		    l1 = b1; dl = db; nl = nb
		}
	    }

	    # Check for consistency
	    if (nimages > 0) {
		if (baxis!=SF_AXIS(sf) ||
		    c1!=SF_X1(sf) || dc!=SF_DX(sf) || nc!=SF_NX(sf) ||
		    l1!=SF_Y1(sf) || dl!=SF_DY(sf) || nl!=SF_NY(sf))
		    call error (1, "Input images have different formats")
	    }

	    # Allocate the focus data structure for the image
	    call spf_alloc (sf, Memc[image], rval, level, baxis, npix, na,
		c1, dc, nc, l1, dl, nl)

	    # Get the spectrum samples
	    if (baxis == 1) {
		do i = 1, na {
		    k = a1 + (i - 1) * da
		    l = k + da - 1
		    data = SF_DATA(sf) + (i - 1) * npix
		    do j = k, l
			call aaddr (Memr[imgl2r(im,j)], Memr[data], Memr[data],
			    npix)
		}
	    } else {
		do j = 1, npix {
		    buf1 = imgl2r (im, j)
		    data = SF_DATA(sf) + j - 1
		    do i = 1, na {
			k = a1 + (i - 1) * da
			Memr[data] = asumr (Memr[buf1+k-1], da)
			data = data + npix
		    }
		}
	    }
	    Memi[sfs+nimages] = sf
	    nimages = nimages + 1

	    call imunmap (im)
	}

	if (nimages == 0)
	    call error (1, "No input data")

	# Sort the structures
	call qsort (Memi[sfs], nimages, spf_compare)

	# Allocate structure for the best focus
	call spf_alloc (sfavg, "Best", INDEF, level, baxis, 0, 0, c1, dc, nc,
	    l1, dl, nl)

	# Compute the correlations and profile width and position
	nprep = db + 2 * lag
	call malloc (buf1, nprep, TY_REAL)
	call malloc (buf2, nprep, TY_REAL)
	l = (na + 1) / 2
	do k = 1, nimages {
	    sf = Memi[sfs+k-1]
	    do i = 1, nb {
		if (baxis == 1)
		    sfd = SFD(sf,i,l)
		else
		    sfd = SFD(sf,l,i)
		call spf_prep (Memr[SF_SPEC(sfd)], db, Memr[buf1], nprep)
		call spf_corr (Memr[buf1], Memr[buf1], nprep, lag,
		    SF_ASI(sfd), SF_POS(sfd), SF_WID(sfd), SF_LEVEL(sf))
		do j = 1, na {
		    if (j != l) {
			if (baxis == 1)
			    sfd = SFD(sf,i,j)
			else
			    sfd = SFD(sf,j,i)
			call spf_prep (Memr[SF_SPEC(sfd)], db, Memr[buf2],
			    nprep)
			call spf_corr (Memr[buf2], Memr[buf2], nprep, lag,
			    SF_ASI(sfd), SF_POS(sfd), SF_WID(sfd), SF_LEVEL(sf))
			if (shifts)
			    call spf_corr (Memr[buf2], Memr[buf1], nprep,
				lag, SF_ASI(sfd), SF_POS(sfd), rval,
				SF_LEVEL(sf))
		    }
		}
	    }
	}
	call mfree (buf1, TY_REAL)
	call mfree (buf2, TY_REAL)

	# Set the averages
	call spf_fitfocus (Memi[sfs], nimages, sfavg, sfbest)

	# Graph the results
	call spf_graph (sfavg, sfbest, Memi[sfs], nimages, lag)

	# Log the results
	call spf_log (sfavg, sfbest, Memi[sfs], nimages, shifts, STDOUT)
	call clgstr ("logfile", Memc[image], SZ_FNAME)
	ifnoerr (log = open (Memc[image], APPEND, TEXT_FILE)) {
	    call spf_log (sfavg, sfbest, Memi[sfs], nimages, shifts, log)
	    call close (log)
	}

	# Finish up
	do i = 1, nimages
	    call spf_free (Memi[sfs+i-1])
	call spf_free (sfavg)
	call mfree (sfs, TY_POINTER)
	call rng_close (rg)
	call imtclose (list)
	call sfree (sp)
end


# SPF_ALLOC -- Allocate a focus data structure for an image

procedure spf_alloc (sf, image, focus, level, axis, ndisp, nspec,
    x1, dx, nx, y1, dy, ny)

pointer	sf		# Image focus data structure
char	image[ARB]	# Image name
real	focus		# Focus value
real	level		# Level for width
int	axis		# Dispersion axis
int	ndisp		# Number of pixels (along dispersion)
int	nspec		# Number of spectra (across dispersion)
int	x1, dx, nx	# X sampling
int	y1, dy, ny	# Y sampling

int	i, j
pointer	data, sfd

begin
	call calloc (sf, LEN_SF, TY_STRUCT)

	call strcpy (image, SF_IMAGE(sf), SZ_SFFNAME)
	SF_FOCUS(sf) = focus
	SF_WIDTH(sf) = INDEF
	SF_LEVEL(sf) = level
	SF_AXIS(sf) = axis
	SF_X1(sf) = x1
	SF_DX(sf) = dx
	SF_NX(sf) = nx
	SF_Y1(sf) = y1
	SF_DY(sf) = dy
	SF_NY(sf) = ny
	call malloc (SF_SFD(sf), nx*ny, TY_POINTER)
	SF_NSFD(sf) = nx*ny
	SF_NPIX(sf) = ndisp
	if (ndisp > 0)
	    call calloc (SF_DATA(sf), ndisp * nspec, TY_REAL)

	data = SF_DATA(sf)
	do j = 1, ny {
	    do i = 1, nx {
		call calloc (sfd, LEN_SFD, TY_STRUCT)
		SFD(sf,i,j) = sfd
		SF_X(sfd) = x1 + (i - 0.5) * dx
		SF_Y(sfd) = y1 + (j - 0.5) * dy
		if (ndisp > 0) {
		    if (axis == 1)
			SF_SPEC(sfd) = data + (j-1)*ndisp + (i-1)*dx + x1-1
		    else
			SF_SPEC(sfd) = data + (i-1)*ndisp + (j-1)*dy + y1-1
		}
		call asiinit (SF_ASI(sfd), II_SPLINE3)
		SF_FOC(sfd) = focus
		SF_WID(sfd) = INDEF
		SF_POS(sfd) = INDEF
		SF_DEL(sfd) = NO
	    }
	}
end


# SPF_FREE -- Free a focus image data structure

procedure spf_free (sf)

pointer	sf		# Image focus data structure

int	i
pointer	sfd

begin
	do i = 1, SF_NSFD(sf) {
	    sfd = SFD(sf,i,1)
	    call asifree (SF_ASI(sfd))
	    call mfree (sfd, TY_STRUCT)
	}
	call mfree (SF_DATA(sf), TY_REAL)
	call mfree (SF_SFD(sf), TY_POINTER)
	call mfree (sf, TY_STRUCT)
end


# SPF_PREP -- Prepare spectra for correlation: fit continuum, subtract, taper

procedure spf_prep (in, nin, out, nout)

real	in[nin]			# Input spectrum
int	nin			# Number of pixels in input spectrum
real	out[nout]		# Output spectrum
int	nout			# Number of pixels output spectrum (nin+2*lag)

int	i, lag
real	cveval()
pointer	sp, x, w, ic, cv

begin
	call smark (sp)
	call salloc (x, nin, TY_REAL)
	call salloc (w, nin, TY_REAL)

	call ic_open (ic)
	call ic_pstr (ic, "function", "chebyshev")
	call ic_puti (ic, "order", 3)
	call ic_putr (ic, "low", 3.)
	call ic_putr (ic, "high", 1.)
	call ic_puti (ic, "niterate", 5)
	call ic_putr (ic, "grow", 1.)
	call ic_putr (ic, "xmin", 1.)
	call ic_putr (ic, "xmax", real(nin))

	do i = 1, nin {
	    Memr[x+i-1] = i
	    Memr[w+i-1] = 1
	}
	call ic_fit (ic, cv, Memr[x], in, Memr[w], nin, YES, YES, YES, YES)

	lag = (nout - nin) / 2
	do i = 1-lag, 0
	    out[i+lag] = 0.
	do i = 1, lag-1
	    out[i+lag] = (1-cos (PI*i/lag))/2 * (in[i] - cveval (cv, real(i))) 
	do i = lag, nin-lag+1
	    out[i+lag] = (in[i] - cveval (cv, real(i))) 
	do i = nin-lag+2, nin
	    out[i+lag] = (1-cos (PI*(nin+1-i)/lag))/2 *
		(in[i] - cveval (cv, real(i))) 
	do i = nin+1, nin+lag
	    out[i+lag] = 0.

	call cvfree (cv)
	call ic_closer (ic)
	call sfree (sp)
end


# SPF_CORR -- Correlate spectra, fit profile, and measure center/width

procedure spf_corr (spec1, spec2, npix, lag, asi, center, width, level)

real	spec1[npix]		# First spectrum
real	spec2[npix]		# Second spectrum
int	npix			# Number of pixels in spectra
int	lag			# Maximum correlation lag
pointer	asi			# Pointer to correlation profile interpolator
real	center			# Center of profile
real	width			# Width of profile
real	level			# Level at which width is determined

int	i, j, nprof
real	x, p, pmin, pmax, asieval()
pointer	sp, prof

begin
	nprof = 2 * lag + 1

	call smark (sp)
	call salloc (prof, nprof, TY_REAL)

	do j = -lag, lag {
	    p = 0.
	    do i = 1+lag, npix-lag
		p = p + spec1[i] * spec2[i-j]
	    Memr[prof+j+lag] = p
	}

	# Fit interpolator
	call asifit (asi, Memr[prof], nprof)

	# Find the minimum and maximum
	center = 1.
	pmin = asieval (asi, 1.)
	pmax = pmin
	for (x=1; x<=nprof; x=x+.01) {
	    p = asieval (asi, x)
	    if (p < pmin)
		pmin = p
	    if (p > pmax) {
		pmax = p
		center = x
	    }
	}

	# Normalize
	pmax = pmax - pmin
	do i = 0, nprof-1
	    Memr[prof+i] = (Memr[prof+i] - pmin) / pmax

	call asifit (asi, Memr[prof], nprof)

	# Find the equal flux points
	for (x=center; x>=1 && asieval (asi,x)>level; x=x-0.01)
	    ;
	width = x
	for (x=center; x<=nprof && asieval (asi,x)>level; x=x+0.01)
	    ;
	width = (x - width - 0.01) / sqrt (2.)
	center = center - lag - 1

	call sfree (sp)
end


# SPF_FITFOCUS -- Find the best focus at each sample and the average over all
# samples.

procedure spf_fitfocus (sfs, nimages, sfavg, sfbest)

pointer	sfs[nimages]		#I Images
int	nimages			#I Number of images
pointer	sfavg			#U Average image
pointer	sfbest			#U Best focus image

int	i, j, n, jmin, nims
pointer	sp, x, y, z, sfd
real	focus, fwhm, pos, foc
bool	fp_equalr()

define	avg_	10

begin
	call smark (sp)
	call salloc (x, nimages, TY_REAL)
	call salloc (y, nimages, TY_REAL)
	call salloc (z, nimages, TY_REAL)

	do i = 1, SF_NSFD(sfavg) {
	    # Collect the focus values
	    nims = 0
	    do j = 1, nimages {
		sfd = SFD(sfs[j],i,1)
		if (SF_DEL(sfd) == NO) {
		    Memr[x+nims] = SF_FOC(sfd)
		    Memr[y+nims] = SF_WID(sfd)
		    Memr[z+nims] = SF_POS(sfd)
		    nims = nims + 1
		}
	    }
	    sfd = SFD(sfavg,i,1)

	    # Take the smallest width at each unique focus.
	    if (nims > 0) {
		call xt_sort3 (Memr[x], Memr[y], Memr[z], nims)
		n = 0
		do j = 1, nims-1 {
		    if (fp_equalr (Memr[x+n], Memr[x+j])) {
			if (Memr[y+n] > Memr[y+j]) {
			    Memr[y+n] = Memr[y+j]
			    Memr[z+n] = Memr[z+j]
			}
		    } else {
			n = n + 1
			Memr[x+n] = Memr[x+j]
			Memr[y+n] = Memr[y+j]
			Memr[z+n] = Memr[z+j]
		    }
		}

		# Find the minimum width
		jmin = 0
		do j = 1, n
		    if (Memr[y+j] < Memr[y+jmin])
			jmin = j

		# Use parabolic interpolation to find the best focus
		if (jmin == 0 || jmin == n) {
		    focus = Memr[x+jmin]
		    fwhm = Memr[y+jmin]
		    pos = Memr[z+jmin]
		} else
		    call spf_parab (Memr[x+jmin-1], Memr[y+jmin-1],
			Memr[z+jmin-1], focus, fwhm, pos)

		SF_FOC(sfd) = focus
		SF_WID(sfd) = fwhm
		SF_POS(sfd) = pos
		SF_DEL(sfd) = NO
	    } else {
		SF_FOC(sfd) = INDEF
		SF_WID(sfd) = INDEF
		SF_POS(sfd) = INDEF
		SF_DEL(sfd) = YES
	    }
	}

	call sfree (sp)

avg_
	# Set the averages over all samples
	n = 0
	focus = 0.
	fwhm = 0.
	do i = 1, SF_NSFD(sfavg) {
	    sfd = SFD(sfavg,i,1)
	    if (SF_DEL(sfd) == NO) {
		focus = focus + SF_FOC(sfd)
		fwhm = fwhm + SF_WID(sfd)
		n = n + 1
	    }
	}

	if (n > 0) {
	    SF_FOCUS(sfavg) = focus / n
	    SF_WIDTH(sfavg) = fwhm / n
	} else {
	    SF_FOCUS(sfavg) = INDEF
	    SF_WIDTH(sfavg) = INDEF
	}

	do j = 1, nimages {
	    n = 0
	    focus = 0.
	    fwhm = 0.
	    do i = 1, SF_NSFD(sfs[j]) {
		sfd = SFD(sfs[j],i,1)
		if (SF_DEL(sfd) == NO) {
		    fwhm = fwhm + SF_WID(sfd)
		    n = n + 1
		}
	    }

	    if (n > 0)
		SF_WIDTH(sfs[j]) = fwhm / n
	    else
		SF_WIDTH(sfs[j]) = INDEF
	}

	# Set the best focus image
	sfbest = NULL
	focus = SF_FOCUS(sfavg)
	if (!IS_INDEF(focus)) {
	    pos = MAX_REAL
	    do j = 1, nimages {
		foc = SF_FOCUS(sfs[j])
		if (!IS_INDEF(foc)) {
		    fwhm = abs (focus - foc)
		    if (fwhm < pos) {
			pos = fwhm
			sfbest = sfs[j]
		    }
		}
	    }
	}
end


# SPF_PARAB -- Find the minimum of a parabolic fit to three points.

procedure spf_parab (x, y, z, xmin, ymin, zmin)

real	x[3]
real	y[3]
real	z[3]
real	xmin
real	ymin
real	zmin

real	x12, x13, x23, x212, x213, x223, y12, y13, y23, a, b, c

begin
	x12 = x[1] - x[2]
	x13 = x[1] - x[3]
	x23 = x[2] - x[3]
	x212 = x[1] * x[1] - x[2] * x[2]
	x213 = x[1] * x[1] - x[3] * x[3]
	x223 = x[2] * x[2] - x[3] * x[3]
	y12 = y[1] - y[2]
	y13 = y[1] - y[3]
	y23 = y[2] - y[3]
	c = (y13 - y23 * x13 / x23) / (x213 - x223 * x13 / x23)
	b = (y23 - c * x223) / x23
	a = y[3] - b * x[3] - c * x[3] * x[3]
	xmin = -b / (2 * c)
	ymin = a + b * xmin + c * xmin * xmin

	if (xmin < x[2])
	    zmin = z[2] + (z[1] - z[2]) / (x[1] - x[2]) * (xmin - x[2])
	else
	    zmin = z[2] + (z[3] - z[2]) / (x[3] - x[2]) * (xmin - x[2])
end


# SPF_COMPARE -- Compare two structures by focus values

int procedure spf_compare (sf1, sf2)

pointer	sf1, sf2	# Structures to be compared.

begin
	if (SF_FOCUS[sf1] < SF_FOCUS[sf2])
	    return (-1)
	else if (SF_FOCUS[sf1] > SF_FOCUS[sf2])
	    return (1)
	else
	    return (0)
end


# SPF_LOG -- Print log of results

procedure spf_log (sfavg, sfbest, sfs, nimages, shifts, log)

pointer	sfavg			# Average image
pointer	sfbest			# Best focus image
pointer	sfs[nimages]		# Images
int	nimages			# Number of images
bool	shifts			# Measure shifts?
int	log			# Log file descriptor

int	i, j
pointer	sp, str, sfd

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	call sysid (Memc[str], SZ_LINE)
	call fprintf (log, "SPECFOCUS: %s\n")
	    call pargstr (Memc[str])

	call fprintf (log,
    "  Best average focus at %g with average width of %.2f at %d%% of peak\n\n")
	    call pargr (SF_FOCUS(sfavg))
	    call pargr (SF_WIDTH(sfavg))
	    call pargr (100 * SF_LEVEL(sfavg))

	call fprintf (log, "  -- Average Over All Samples\n\n")
	call fprintf (log, "\t%25wImage  Focus  Width\n")
	do i = 1, nimages {
	    call fprintf (log, "\t%30s  %5.3g  %5.2f\n")
		call pargstr (SF_IMAGE(sfs[i]))
		call pargr (SF_FOCUS(sfs[i]))
		call pargr (SF_WIDTH(sfs[i]))
	}
	call fprintf (log, "\n")

	call fprintf (log, "  -- Image %s at Focus %g --\n")
	    call pargstr (SF_IMAGE(sfbest))
	    call pargr (SF_FOCUS(sfbest))

	if (SF_NSFD(sfbest) > 1) {
	    call fprintf (log, "\n\n\tWidth at %d%% of Peak:\n")
		call pargr (100 * SF_LEVEL(sfavg))
	    call fprintf (log, "\n\t%9w    Columns\n\t%9w  ")
	    do i = 1, SF_NX(sfbest) {
		call fprintf (log, "  %4d-%-4d")
		    call pargi (SF_X1(sfbest)+(i-1)*SF_DX(sfbest))
		    call pargi (SF_X1(sfbest)+i*SF_DX(sfbest)-1)
	    }
	    call fprintf (log, "\n\t   Lines  +")
	    do i = 1, SF_NX(sfbest)
		call fprintf (log, "-----------")
	    do j = 1, SF_NY(sfbest) {
		if (SF_DY(sfbest) > 1.) {
		    call fprintf (log, "\n\t%4d-%-4d |")
			call pargi (SF_Y1(sfbest)+(j-1)*SF_DY(sfbest))
			call pargi (SF_Y1(sfbest)+j*SF_DY(sfbest)-1)
		} else {
		    call fprintf (log, "\n\t%9d |")
			call pargi (SF_Y1(sfbest)+(j-1)*SF_DY(sfbest))
			call pargi (SF_Y1(sfbest)+j*SF_DY(sfbest)-1)
		}
		do i = 1, SF_NX(sfbest) {
		    sfd = SFD(sfbest,i,j)
		    call fprintf (log, "   %5.2f   ")
			call pargr (SF_WID(sfd))
		}
	    }
	    if (shifts) {
		call fprintf (log, 
		    "\n\n\tPosition Shifts Relative To Central Sample:\n")
		call fprintf (log, "\n\t%9w    Columns\n\t%9w  ")
		do i = 1, SF_NX(sfbest) {
		    call fprintf (log, "  %4d-%-4d")
			call pargi (SF_X1(sfbest)+(i-1)*SF_DX(sfbest))
			call pargi (SF_X1(sfbest)+i*SF_DX(sfbest)-1)
		}
		call fprintf (log, "\n\t   Lines  +")
		do i = 1, SF_NX(sfbest)
		    call fprintf (log, "-----------")
		do j = 1, SF_NY(sfbest) {
		    call fprintf (log, "\n\t%4d-%-4d |")
			call pargi (SF_Y1(sfbest)+(j-1)*SF_DY(sfbest))
			call pargi (SF_Y1(sfbest)+j*SF_DY(sfbest)-1)
		    do i = 1, SF_NX(sfbest) {
			sfd = SFD(sfbest,i,j)
			call fprintf (log, "   %5.2f   ")
			    call pargr (SF_POS(sfd))
		    }
		}
	    }
	}
	call fprintf (log, "\n\n")
	    
	call sfree (sp)
end
