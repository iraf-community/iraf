include	<mach.h>
include	<ctype.h>
include	<error.h>
include	<imhdr.h>
include	<fset.h>
include	<math/iminterp.h>
include "oned.h"
include "idsmtn.h"

# Rebinning defs
define	INTERP_MODE	"|linear|spline3|poly3|poly5|sums|"
define	RB_LINEAR	1
define	RB_SPLINE3	2
define	RB_POLY3	3
define	RB_POLY5	4
define	RB_SUMS		5

# T_REBIN -- Rebin input spectra to the same dispersion relation
#  as another spectrum or a user defined relation.

procedure t_rebin ()

char	image[SZ_FNAME], primary[SZ_FNAME]
char	rec_numbers[SZ_LINE], ofile[SZ_FNAME]
char	interp_mode[SZ_LINE]
int	root, nfiles, start_rec
int	nrecs, records[3, MAX_RANGES]
int	mode, cols_out
real	swave, dwave
bool	logarithm
pointer	ids, sp, im, imout

int	clpopni(), clplen(), clgeti(), clgwrd()
int	get_next_image(), decode_ranges()
real	clgetr()
bool	clgetb()
pointer	immap()

begin
	# Open input file name template.
	root   = clpopni ("input")
	nfiles = clplen (root)

	# Get range specification if any.
	call clgstr ("records", rec_numbers, SZ_LINE)
	if (decode_ranges (rec_numbers, records, MAX_RANGES, nrecs) == ERR)
	    call error (0, "Bad range specification")

	# Two possible options determine what the argument list
	# will be: The user may specify the starting wavelength and
	# dispersion, or the info may be taken from the header of
	# a primary spectrum.
	swave = clgetr ("wave0")
	dwave = clgetr ("dw")

	if (swave == 0.0)
	    call clgstr ("primary", primary, SZ_FNAME)

	# Get rootname for output files and starting record
	call clgstr ("output", ofile, SZ_FNAME)
	start_rec = clgeti ("start_rec")

	# Is rebinning to be in logarithm form?
	logarithm = clgetb ("logarithm")

	# What about the rebinning method?
	mode = clgwrd ("interp_mode", interp_mode, SZ_LINE, INTERP_MODE)

	# Get number of output columns
	cols_out = clgeti ("cols_out")

	# Force STDOUT flush
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Initialize range decoder
	call reset_next_image ()

	# Mark dynamic space
	call smark (sp)

	# Allocate space for IDS header
	call salloc (ids, LEN_IDS, TY_STRUCT)
	call salloc (POINT(ids), MAX_NCOEFF, TY_REAL)

	# Get start wavelength and dispersion
	if (swave == 0.0)
	    call get_primary (primary, ids, swave, dwave, logarithm)

	# Loop over all input images - rebin and make new image.
	while (get_next_image (root, records, nrecs, image, 
	    SZ_FNAME) != EOF) {

	    # Open image
	    iferr (im = immap (image, READ_ONLY, 0)) {
		call eprintf ("[%s]")
		    call pargstr (image)
		call error (0, " Image not found or header info not available")
	    }

	    # Load header data into input and output header arrays
	    call load_ids_hdr (ids, im, 1)

	    if (DC_FLAG(ids) == -1) {
		call eprintf ("[%s] Image not dispersion corrected\n")
		    call pargstr (image)
		call imunmap (im)
		next
	    }

	    # Rebin to desired conditions
	    call al_rebin (im, ids, ofile, start_rec, mode, logarithm,
		cols_out, swave, dwave, imout)

	    W0(ids)  = swave
	    WPC(ids) = dwave

	    if (logarithm)
		DC_FLAG(ids) = 1
	    else
		DC_FLAG(ids) = 0

	    call store_keywords (ids, imout)
	    call imunmap (im)
	    call imunmap (imout)

	    start_rec = start_rec + 1
	}

	# Update record number
	call clputi ("onedspec.next_rec", start_rec)

	# Free space
	call sfree (sp)
	call clpcls (root)
end

# GET_PRIMARY -- Load primary image header to get starting wavelength and dw

procedure get_primary (image, ids, start, dw, logarithm)

char	image[SZ_FNAME]
pointer	ids
real	start, dw
bool	logarithm

pointer	im

pointer immap()

begin
	iferr (im = immap (image, READ_ONLY, 0))
	    call error (0, "Cannot open primary image header")

	call load_ids_hdr (ids, im, 1)

	# Check for logarithm
	if (logarithm && (DC_FLAG (ids) == 0)) {
	    start = log10 (W0(ids))
	    dw = (log10 (W0(ids)+(IM_LEN(im,1)-1)*WPC(ids)) - start) /
		(IM_LEN(im,1) - 1)
	} else if (!logarithm && (DC_FLAG(ids) == 1)) {
	    start = 10. ** W0(ids)
	    dw = (10. ** (W0(ids)+(IM_LEN(im,1)-1)*WPC(ids)) - start) /
		(IM_LEN(im, 1) - 1)
	} else {
	    start = W0 (ids)
	    dw    = WPC(ids)
	}

	call imunmap (im)
end

# AL_REBIN -- Rebin the image according to the specified alignment

procedure al_rebin (im, ids, ofile, start_rec, mode, logarithm,
		cols_out, w0, wpc, imout)

pointer	im, ids, imout
char	ofile[ARB]
int	start_rec, mode, cols_out
real	w0, wpc
bool	logarithm

pointer	sp, pixin, pixout, invert
bool	login
real	w1, w2
int	nlen, ncols
char	imname[SZ_FNAME]

pointer	imgl1r(), impl1r()
pointer	immap()

begin

	# Open output image
	call sprintf (imname, SZ_FNAME, "%s.%04d")
	    call pargstr (ofile)
	    call pargi (start_rec)

	imout = immap (imname, NEW_COPY, im)
	IM_NDIM (imout) = IM_NDIM (im)

	# Length of input spectrum
	nlen   = IM_LEN (im, 1)

	if (DC_FLAG(ids) == 1)
	    login = true
	else
	    login = false

	# Length of output spectrum
	ncols = cols_out
	if (ncols == 0) {
	    if ((logarithm && login) || (!logarithm && !login)) {
	        w1 = W0(ids)
	        w2 = W0(ids) + (nlen-1) * WPC(ids)
	    } else if (logarithm) {
	        w1 = log10 (W0(ids))
	        w2 = log10 (W0(ids) + (nlen-1) * WPC(ids))
	    } else {
	        w1 = 10. ** W0(ids)
	        w2 = 10. ** (W0(ids) + (nlen-1) * WPC(ids))
	    }

	    ncols =  max (w1 - w0, w2 - w0) / wpc + 1.5
	}

	NP1(ids) = 0
	NP2(ids) = ncols

	IM_LEN (imout, 1) = ncols
	IM_PIXTYPE (imout) = TY_REAL
	call strcpy (IM_TITLE (im), IM_TITLE (imout), SZ_LINE)

	# Make room for inverted solution
	call smark (sp)
	call salloc (invert, ncols, TY_REAL)

	# Compute pixel position as a function of lambda.
	# Interpolate according to the flexure parameter.
	call lambda_to_pixel2 (w0, wpc, W0(ids), WPC(ids), login, ncols, 
	    logarithm, Memr[invert])

	# Map image pixels
	pixin  = imgl1r (im, 1)
	pixout = impl1r (imout, 1)

	switch (mode) {
	case RB_LINEAR:
	    call reinterp (Memr[pixin], Memr[pixout], Memr[invert],
		ncols, nlen, II_LINEAR)
	case RB_SPLINE3:
	    call reinterp (Memr[pixin], Memr[pixout], Memr[invert],
		ncols, nlen, II_SPLINE3)
	case RB_POLY3:
	    call reinterp (Memr[pixin], Memr[pixout], Memr[invert],
		ncols, nlen, II_POLY3)
	case RB_POLY5:
	    call reinterp (Memr[pixin], Memr[pixout], Memr[invert],
		ncols, nlen, II_POLY5)
	case RB_SUMS:
	    call resum (Memr[pixin], Memr[pixout], Memr[invert], ncols, nlen)
	}

	call printf ("[%s]: %s\n")
	call pargstr (imname)
	call pargstr (IM_TITLE (im))
	call sfree (sp)
end

# LAMBDA_TO_PIXEL2 -- Compute transformation table converting lambda to
#		      pixel number for relinearization

procedure lambda_to_pixel2 (w0out, wpcout, w0in, wpcin, login, ncols, 
	logarithm, invert)

real	w0out, wpcout, w0in, wpcin
bool	login
int	ncols
bool	logarithm
real	invert[ARB]

int	i
real	w

begin
	if ((logarithm && login) || (!logarithm && !login))
	    do i = 1, ncols {
	        w = w0out + (i - 1) * wpcout
	        invert[i] = (w - w0in) / wpcin + 1
	    }
	else if (logarithm)
	    do i = 1, ncols {
	        w = 10. ** (w0out + (i - 1) * wpcout)
	        invert[i] = (w - w0in) / wpcin + 1
	    }
	else
	    do i = 1, ncols {
	        w = log10 (w0out + (i - 1) * wpcout)
	        invert[i] = (w - w0in) / wpcin + 1
	    }
end
