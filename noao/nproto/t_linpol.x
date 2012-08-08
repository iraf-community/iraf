# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include <error.h>

define	MAX_IMAGES	4

define	LP_TITLE	"Linear polarization image"
define	LP_IMKEY	"POL"		# keyword prefix for input images

define	LP_PBAND	1
define	LP_PKEY		"POLAR"
define	LP_PSTR		"Band 1 is the percent polarization"

define	LP_ABAND	2
define	LP_AKEY		"ANGLE"
define	LP_ASTR		"Band 2 is the polarization angle"

define	LP_IBAND	3
define	LP_IKEY		"I-STOKES"
define	LP_ISTR		"Band 3 is the Stokes I parameter"

define	LP_QBAND	4
define	LP_QKEY		"Q-STOKES"
define	LP_QSTR		"Band 4 is the Stokes Q parameter"
define	LP_QSTRN	"Band 4 is the normalized Stokes Q parameter"

define	LP_UBAND	5
define	LP_UKEY		"U-STOKES"
define	LP_USTR		"Band 5 is the Stokes U parameter"
define	LP_USTRN	"Band 5 is the normalized Stokes U parameter"


# LINPOL -- Calculate the percent polarization and the polarization
# angle images for the simplest linear polarization cases, 0-45-90 or
# 0-45-90-135 polarizer positions.

procedure t_linpol ()

pointer	inlist, output, in[MAX_IMAGES], out, key, sp
bool	dflag, sflag, nflag
int	len

int     imtopenp(), imtlen()
bool	clgetb()

errchk	lp_map, lp_polarize

begin
	call smark (sp)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (key, SZ_FNAME, TY_CHAR)

	# get the input image template
        inlist = imtopenp ("input")
	len = imtlen (inlist)
	if (len != 3 && len != 4) {
	    call imtclose (inlist)
	    call sfree (sp)
	    call error (1, "Must supply either three or four images.")
	}

	# get the output image stack name
	call clgstr ("output", Memc[output], SZ_FNAME)

	sflag = clgetb ("stokes")
	dflag = clgetb ("degrees")
	nflag = clgetb ("normalize")

	# keyword for polarizer angle - UPPERcase for neatness
	call clgstr ("keyword", Memc[key], SZ_FNAME)
	call strupr (Memc[key])

	iferr {
	    # pass the number of possible frames (4) explicitly in
	    # hopes of later relaxing the 45 degree restriction
	    call lp_map (inlist, in, MAX_IMAGES,
		Memc[key], Memc[output], out, sflag, nflag)
	    call lp_polarize (in, MAX_IMAGES, out, dflag, sflag, nflag)
	} then {
	    call lp_unmap (in, MAX_IMAGES, out)
	    call imtclose (inlist)
	    call sfree (sp)
	    call erract (EA_ERROR)
	}

	call lp_unmap (in, MAX_IMAGES, out)
	call imtclose (inlist)
	call sfree (sp)
end


# LP_MAP -- map the set of input images.

procedure lp_map (inlist, in, nin, key, output, out, sflag, nflag)

pointer	inlist		#I input image template
pointer	in[nin]		#O input image descriptor array
int	nin		#I size of the array (4)
char	key[ARB]	#I keyword for polarizer angle
char	output[ARB]	#I output image name
pointer	out		#O output image descriptor
bool	sflag		#I include stokes frames in output?
bool	nflag		#I normalize the stokes frames?

pointer	input, im_tmp, sp
real	pol
int	i, j, ipol, ndim
long	axis[IM_MAXDIM]
bool	firsttime

int     imtgetim()
real	imgetr()
pointer	immap()
bool	fp_equalr()

errchk	immap, imgetr, imdelf

begin
	# for graceful error recovery
	im_tmp = NULL
	out = NULL
	do i = 1, nin
	    in[i] = NULL


	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)

	iferr {
	    while (imtgetim (inlist, Memc[input], SZ_FNAME) != EOF) {
		im_tmp = immap (Memc[input], READ_ONLY, 0)

		if (IM_NDIM(im_tmp) > 2)
		    call error (1, "only 1 or 2 dimensional images allowed")

		pol = imgetr (im_tmp, key)

		if (pol < 0 || pol > 135 || mod (nint(pol), 45) != 0 ||
		    ! fp_equalr (pol, real(nint(pol)))) {
		    call eprintf ("image %s, %s must be 0,45,90,135 degrees\n")
			call pargstr (Memc[input])
			call pargstr (key)
		    call flush (STDERR)
		    call error (1, "task LINPOL")
		}

		# index into in pointer array
		ipol = max (1, min (nin, 1 + int(pol) / 45))

		if (in[ipol] == NULL) {
		    in[ipol] = im_tmp
		    im_tmp = NULL
		} else {
		    call eprintf ("multiple images specified at %d degrees\n")
			call pargi ((ipol-1) * 45)
		    call flush (STDERR)
		    call error (1, "task JOIN")
		}
	    }

	    # check dimensionality
	    firsttime = true
	    do i = 1, nin {
		if (in[i] == NULL)
		    next

		if (firsttime) {
		    ndim = IM_NDIM(in[i])
		    do j = 1, IM_MAXDIM
			axis[j] = IM_LEN(in[i],j)
		    firsttime = false
		    next
		}

		if (IM_NDIM(in[i]) != ndim)
		    call error (1, "images are different sizes")

		do j = 1, ndim
		    if (IM_LEN(in[i],j) != axis[j])
			call error (1, "images are different sizes")
	    }

	    # create the output polarization (hyper) cube
	    # just copy header from first image available
	    do i = 1, nin
		if (in[i] != NULL) {
		    out = immap (output, NEW_COPY, in[i])
		    break
		}

	    # increase the image's girth
	    IM_NDIM(out) = ndim + 1
	    for (i=1; i <= ndim; i=i+1)
		IM_LEN(out,i) = axis[i]

	    if (sflag)
		IM_LEN(out,i) = 5
	    else
		IM_LEN(out,i) = 2

	    call strcpy (LP_TITLE, IM_TITLE(out), SZ_IMTITLE)

	    # delete the polarizer angle keyword
	    call imdelf (out, key)

	    # add keywords naming the input images
	    do i = 1, nin {
		if (in[i] == NULL)
		    next

		call sprintf (Memc[input], SZ_FNAME, "%s%d")
		    call pargstr (LP_IMKEY)
		    call pargi (45*(i-1))

		call imastr (out, Memc[input], IM_HDRFILE(in[i]))
	    }

	    # add keywords to index output frames
	    call imastr (out, LP_PKEY, LP_PSTR)
	    call imastr (out, LP_AKEY, LP_ASTR)

	    if (sflag) {
		call imastr (out, LP_IKEY, LP_ISTR)
		if (nflag) {
		    call imastr (out, LP_QKEY, LP_QSTRN)
		    call imastr (out, LP_UKEY, LP_USTRN)
		} else {
		    call imastr (out, LP_QKEY, LP_QSTR)
		    call imastr (out, LP_UKEY, LP_USTR)
		}
	    }

	} then {
	    if (im_tmp != NULL)
		call imunmap (im_tmp)
	    call sfree (sp)
	    call erract (EA_ERROR)
	}

	# start off with a clean slate
	call imflush (out)
	call sfree (sp)
end


# LP_UNMAP -- unmap the set of input images.

procedure lp_unmap (in, nin, out)

pointer	in[nin]		#U input image pointer array
int	nin		#I size of the array (4)
pointer	out		#U output image pointer

int	i

begin
	do i = 1, nin
	    if (in[i] != NULL)
		call imunmap (in[i])

	if (out != NULL)
	    call imunmap (out)
end


# LP_POLARIZE -- calculate the polarization given at least 3 of the 4
# possible frames taken with the polarizer at 45 degree increments.

procedure lp_polarize (in, nin, out, dflag, sflag, nflag)

pointer	in[nin]		#I input image pointer array
int	nin		#I size of the array (4)
pointer	out		#I output image pointer
bool	dflag		#I report the angle in degrees?
bool	sflag		#I include stokes frames in output?
bool	nflag		#I normalize the stokes frames?

pointer	ibuf, qbuf, ubuf, sp
pointer	buf1, buf2, buf3, buf4
long	v1[IM_MAXDIM], v2[IM_MAXDIM], v3[IM_MAXDIM], v4[IM_MAXDIM]
int	line, npix, skip, i

int	imgnlr()

begin
	npix = IM_LEN(out,1)

	call smark (sp)
	call salloc (ibuf, npix, TY_REAL)
	call salloc (qbuf, npix, TY_REAL)
	call salloc (ubuf, npix, TY_REAL)

	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)
	call amovkl (long(1), v3, IM_MAXDIM)
	call amovkl (long(1), v4, IM_MAXDIM)

	# choose the combining scheme
	skip = 0
	do i = 1, nin
	    if (in[i] == NULL) {
		skip = i
		break
	    }

	# not worth generalizing the method, just duplicate the code...
	switch (skip) {

	case 0:
	    # I =  (im0 + im45 + im90 + im135) / 4
	    # Q =  (im0 - im90)  / 2
	    # U = (im45 - im135) / 2
	    while (imgnlr (in[1], buf1, v1) != EOF &&
		   imgnlr (in[2], buf2, v2) != EOF &&
		   imgnlr (in[3], buf3, v3) != EOF &&
		   imgnlr (in[4], buf4, v4) != EOF) {

		call aaddr (Memr[buf1], Memr[buf2], Memr[ibuf], npix)
		call aaddr (Memr[buf3], Memr[ibuf], Memr[ibuf], npix)
		call aaddr (Memr[buf4], Memr[ibuf], Memr[ibuf], npix)
		call adivkr (Memr[ibuf], 4., Memr[ibuf], npix)

		call asubr (Memr[buf1], Memr[buf3], Memr[qbuf], npix)
		call adivkr (Memr[qbuf], 2., Memr[qbuf], npix)

		call asubr (Memr[buf2], Memr[buf4], Memr[ubuf], npix)
		call adivkr (Memr[ubuf], 2., Memr[ubuf], npix)

		line = int(v1[2]) - 1
		call lp_stokes (Memr[ibuf], Memr[qbuf], Memr[ubuf],
		    npix, out, line, dflag, sflag, nflag)
	    }

	case 1:
	    # I = (im45 + im135) / 2
	    # Q =     I - im90
	    # U = (im45 - im135) / 2
	    while (imgnlr (in[2], buf2, v2) != EOF &&
		   imgnlr (in[3], buf3, v3) != EOF &&
		   imgnlr (in[4], buf4, v4) != EOF) {

		call aaddr (Memr[buf2], Memr[buf4], Memr[ibuf], npix)
		call adivkr (Memr[ibuf], 2., Memr[ibuf], npix)

		call asubr (Memr[ibuf], Memr[buf3], Memr[qbuf], npix)

		call asubr (Memr[buf2], Memr[buf4], Memr[ubuf], npix)
		call adivkr (Memr[ubuf], 2., Memr[ubuf], npix)

		line = int(v2[2]) - 1
		call lp_stokes (Memr[ibuf], Memr[qbuf], Memr[ubuf],
		    npix, out, line, dflag, sflag, nflag)
	    }

	case 2:
	    # I = (im0 + im90) / 2
	    # Q = (im0 - im90) / 2
	    # U =    I - im135
	    while (imgnlr (in[1], buf1, v1) != EOF &&
		   imgnlr (in[3], buf3, v3) != EOF &&
		   imgnlr (in[4], buf4, v4) != EOF) {

		call aaddr (Memr[buf1], Memr[buf3], Memr[ibuf], npix)
		call adivkr (Memr[ibuf], 2., Memr[ibuf], npix)

		call asubr (Memr[buf1], Memr[buf3], Memr[qbuf], npix)
		call adivkr (Memr[qbuf], 2., Memr[qbuf], npix)

		call asubr (Memr[ibuf], Memr[buf4], Memr[ubuf], npix)

		line = int(v1[2]) - 1
		call lp_stokes (Memr[ibuf], Memr[qbuf], Memr[ubuf],
		    npix, out, line, dflag, sflag, nflag)
	    }

	case 3:
	    # I = (im45 + im135) / 2
	    # Q =   im0 - I
	    # U = (im45 - im135) / 2
	    while (imgnlr (in[1], buf1, v1) != EOF &&
		   imgnlr (in[2], buf2, v2) != EOF &&
		   imgnlr (in[4], buf4, v4) != EOF) {

		call aaddr (Memr[buf2], Memr[buf4], Memr[ibuf], npix)
		call adivkr (Memr[ibuf], 2., Memr[ibuf], npix)

		call asubr (Memr[buf1], Memr[ibuf], Memr[qbuf], npix)

		call asubr (Memr[buf2], Memr[buf4], Memr[ubuf], npix)
		call adivkr (Memr[ubuf], 2., Memr[ubuf], npix)

		line = int(v1[2]) - 1
		call lp_stokes (Memr[ibuf], Memr[qbuf], Memr[ubuf],
		    npix, out, line, dflag, sflag, nflag)
	    }

	case 4:
	    # I = (im0 + im90) / 2
	    # Q = (im0 - im90) / 2
	    # U = im45 - I
	    while (imgnlr (in[1], buf1, v1) != EOF &&
		   imgnlr (in[2], buf2, v2) != EOF &&
		   imgnlr (in[3], buf3, v3) != EOF) {

		call aaddr (Memr[buf1], Memr[buf3], Memr[ibuf], npix)
		call adivkr (Memr[ibuf], 2., Memr[ibuf], npix)

		call asubr (Memr[buf1], Memr[buf3], Memr[qbuf], npix)
		call adivkr (Memr[qbuf], 2., Memr[qbuf], npix)

		call asubr (Memr[buf2], Memr[ibuf], Memr[ubuf], npix)

		line = int(v1[2]) - 1
		call lp_stokes (Memr[ibuf], Memr[qbuf], Memr[ubuf],
		    npix, out, line, dflag, sflag, nflag)
	    }

	}

	call sfree(sp)
end


# LP_STOKES -- calculate the fractional polarization and angle for a
# specific line (from the stokes parameters) and output the results.

procedure lp_stokes (i, q, u, npix, out, line, dflag, sflag, nflag)

real	i[ARB]		#I Stokes I vector
real	q[ARB]		#I Stokes Q vector
real	u[ARB]		#I Stokes U vector
int	npix		#I length of the vectors
pointer	out		#I output image descriptor
int	line		#I line number
bool	dflag		#I convert to degrees?
bool	sflag		#I include stokes frames in output?
bool	nflag		#I normalize the stokes frames?

pointer	pbuf, abuf, sp

pointer	impl3r()
real	lp_errfcn()
extern	lp_errfcn

begin
	call smark (sp)
	call salloc (pbuf, npix, TY_REAL)
	call salloc (abuf, npix, TY_REAL)

	call lp_pol (i, q, u, Memr[pbuf], npix)
	call lp_ang (q, u, Memr[abuf], npix, dflag)

	call amovr (Memr[pbuf], Memr[impl3r (out, line, LP_PBAND)], npix)
	call amovr (Memr[abuf], Memr[impl3r (out, line, LP_ABAND)], npix)

	if (sflag) {
	    call amovr (i, Memr[impl3r (out, line, LP_IBAND)], npix)
	    if (nflag) {
		call advzr (q, i, q, npix, lp_errfcn)
		call advzr (u, i, u, npix, lp_errfcn)
	    }
	    call amovr (q, Memr[impl3r (out, line, LP_QBAND)], npix)
	    call amovr (u, Memr[impl3r (out, line, LP_UBAND)], npix)
	}

	call sfree (sp)
end


# LP_POL -- calculate the fractional linear polarization for a vector,
# given the stokes I, Q, and U vectors.

procedure lp_pol (i, q, u, p, npix)

real	i[ARB]		#I Stokes I vector
real	q[ARB]		#I Stokes Q vector
real	u[ARB]		#I Stokes U vector
real	p[ARB]		#O fractional polarization vector
int	npix		#I length of the vectors

pointer	tmp, sp

real	lp_errfcn()
extern	lp_errfcn

begin
	call smark (sp)
	call salloc (tmp, npix, TY_REAL)

	call amulr (q, q, p, npix)
	call amulr (u, u, Memr[tmp], npix)
	call aaddr (p, Memr[tmp], p, npix)
	call asqrr (p, p, npix, lp_errfcn)
	call advzr (p, i, p, npix, lp_errfcn)

	call sfree (sp)
end


# LP_ERRFCN -- error function for the square root of negative numbers.

real procedure lp_errfcn (x)

real    x

begin
	return (0.)
end


# LP_ANG -- calculate the polarization angle, given the Stokes params.

procedure lp_ang (q, u, a, npix, dflag)

real	q[ARB]		#I Stokes Q vector
real	u[ARB]		#I Stokes U vector
real	a[ARB]		#O polarization angle vector
int	npix		#I length of the vectors
bool	dflag		#I convert to degrees?

define	PI		3.14159265358979
define	RAD2DEG		(180./PI)

begin
	call lp_aatn2r (u, q, a, npix)
	call adivkr (a, 2., a, npix)

	if (dflag)
	    call amulkr (a, RAD2DEG, a, npix)
end


# LP_AATN2R -- calculate the arctangent in the proper quadrant.

procedure lp_aatn2r (y, x, a, npix)

real	y[ARB], x[ARB]		#I numerator and denominator, respectively
real	a[ARB]			#O arctangent vector (radians)

int	npix, i

begin
	do i = 1, npix {
	    a[i] = atan2 (y[i], x[i])
	}
end
