# MIM (Match IMage) -- Match a 2D image to a 2D reference image.
#
# These routines provide an I/O interface to get data from a 2D image which
# matches a line of a 2D reference image.  The two common uses are to get a
# subraster of the image which matches the reference image and to interpolate
# an image which is blocked to a lower resolution than the reference image.
# The matching is done in physical pixel coordinates.  It is completely
# general in allowing any linear transformation between the physical
# coordinates.  But in most cases the reference image and the input image
# will be related either by an image section or some kind of blocking factor
# without rotation.  Any relative rotation of the two in physical pixels is
# likely to be slow for large images (either the reference image or the mim
# image).  Interpolation (if any is required) is done with the MSI library.
# Extrapolation outside of the input image uses the nearest edge value.
# 
#         mim = mim_open (input, refim)
#         buf = mim_glr (mim, refline)
#               mim_close (mim)
# 
# Parameters may be queried and set by the following routines.
# 
#               mim_geti (mim, param, val)
#               mim_getr (mim, param, val)
#               mim_gets (mim, param, str, maxchar)
#               mim_seti (mim, param, val)
#               mim_setr (mim, param, val)
#               mim_sets (mim, param, str)
# 
# The parameters are specified by strings as given below.  The default values
# are in parentheses.  Currently there are only integer parameters.
# 
#         msitype - interpolation type defined by the MSI library
#                   (II_BISPLINE3)
#         msiedge - number of additional lines at each edge to include
#                   in interpolation (3)
#          msimax - maximum number of pixels to allow in MSIFIT calls (500000)
	

include	<error.h>
include <imhdr.h>
include	<imset.h>
include	<math/iminterp.h>

# Data structure.
define	MIM_LEN		18
define	MIM_INTERP	Memi[$1]	# Use interpolation?
define	MIM_ROTATE	Memi[$1+1]	# Is there any rotation?
define	MIM_IM		Memi[$1+2]	# IMIO mim pointer
define	MIM_MSI		Memi[$1+3]	# MSI interpolation pointer
define	MIM_NCREF	Memi[$1+4]	# Number of columns in ref image
define	MIM_NC		Memi[$1+5]	# Number of columns in input image
define	MIM_NL		Memi[$1+6]	# Number of lines in input image
define	MIM_LINE1	Memi[$1+7]	# First line in msi fit
define	MIM_LINE2	Memi[$1+8]	# Last line in msi fit
define	MIM_X		Memi[$1+9]	# Pointer to line of x values
define	MIM_Y		Memi[$1+10]	# Pointer to line of y values
define	MIM_Z		Memi[$1+11]	# Pointer to line of z values
define	MIM_MW		Memi[$1+12]	# MWCS pointer
define	MIM_CT		Memi[$1+13]	# CT from ref logical to input logical
define	MIM_MSITYPE	Memi[$1+14]	# MSI interpolation type
define	MIM_MSIEDGE	Memi[$1+15]	# Number of edge pixels to reserve
define	MIM_MSIMAX	Memi[$1+16]	# Maximum number of pixels in msi fit
define	MIM_DELETE	Memi[$1+17]	# Delete image after closing?

# Defaults
define	MIM_MSITYPEDEF	II_BISPLINE3
define	MIM_MSIEDGEDEF	3
define	MIM_MSIMAXDEF	500000


# MIM_GL -- Get a line of data matching a line of the reference image.
# A pointer to the data is returned.  The data buffer is assumed to be
# read-only and not to be modified by the calling routine.

pointer procedure mim_glr (mim, line)

pointer	mim		#I Map pointer
int	line		#I Reference image line

int	i, j, nc, nl, ncref, line1, line2, nlines
pointer	msi, ct, x, y, z, imname, ptr
real	rnl, val

real	mw_c1tranr()
pointer	imgl2r(), imgs2r()

errchk	imgl2r, msiinit, msifit, imdelete

begin
	if (mim == NULL)
	    call error (1, "Map is undefined")

	# If interpolation is not needed return the IMIO buffer.
	if (MIM_INTERP(mim) == NO) {
	    ptr = imgl2r (MIM_IM(mim), line)
	    return (ptr)
	}

	nc = MIM_NC(mim)
	nl = MIM_NL(mim)
	ncref = MIM_NCREF(mim)
	rnl = nl
	msi = MIM_MSI(mim)
	ct = MIM_CT(mim)
	x = MIM_X(mim)
	y = MIM_Y(mim)
	z = MIM_Z(mim)

	# Set the interpolation coordinates in the input image logical pixels.
	# This is limited to be within the input image.  Therefore, requests
	# outside the input image will use the nearest edge value.
	# Also set the minimum range of input lines required.

	if (MIM_ROTATE(mim) == NO) {
	    val = mw_c1tranr (ct, real(line))
	    val = max (1., min (rnl, val))
	    call amovkr (val, Memr[y], ncref)
	    line1 = max (1., val - 1)
	    line2 = min (rnl, val + 1)
	} else {
	    call amovkr (real(line), Memr[y], ncref)
	    call mw_v2tranr (ct, Memr[x], Memr[y], Memr[z], Memr[y], ncref)
	    x = z

	    # Limit the x range to within the input image.
	    ptr = x
	    val = nc
	    do i = 1, ncref {
		Memr[ptr] = max (1., min (val, Memr[ptr]))
		ptr = ptr + 1
	    }

	    # Limit the y range to within the input image and find the range
	    # of lines required.
	    j = nint (Memr[y])
	    line1 = max (1, min (nl, j))
	    line2 = line1
	    ptr = y
	    rnl = nl
	    do i = 1, ncref {
		val = max (1., min (rnl, Memr[ptr]))
		j = nint (val)
		line1 = min (j, line1)
		line2 = max (j, line2)
		Memr[ptr] = val
		ptr = ptr + 1
	    }
	    line1 = max (1, line1 - 1)
	    line2 = min (nl, line2 + 1)
	}

	# Set or reset image interpolator.  For small input interpolation
	# images read the entire image, fit the interpolator, and free the
	# image.  For larger input images determine the range of lines
	# required including edge space and fit the interpolator to those
	# lines.  Providing the reference lines are requested sequentially
	# this is about as efficient as we can make it.

	if (line1 < MIM_LINE1(mim) || line2 > MIM_LINE2(mim)) {
	    if (msi != NULL)
		call msifree (MIM_MSI(mim))
	    if (min (nc, nl) > 3)
		call msiinit (MIM_MSI(mim), MIM_MSITYPE(mim))
	    else if (min (nc, nl) > 1)
		call msiinit (MIM_MSI(mim), II_BILINEAR)
	    else
		call msiinit (MIM_MSI(mim), II_BINEAREST)
	    msi = MIM_MSI(mim)
	    if (nc * nl <= MIM_MSIMAX(mim)) {
		nlines = nl
		line1 = 1
		line2 = nlines
		ptr = imgs2r (MIM_IM(mim), 1, nc, line1, line2)
		call msifit (msi, Memr[ptr], nc, nlines, nc)
		if (MIM_DELETE(mim) == YES) {
		    call malloc (imname, SZ_FNAME, TY_CHAR)
		    call imstats (MIM_IM(mim), IM_IMAGENAME, Memc[imname],
			SZ_FNAME)
		    call imgimage (Memc[imname], Memc[imname], SZ_FNAME)
		    call imunmap (MIM_IM(mim))
		    call imdelete (Memc[imname])
		    call mfree (imname, TY_CHAR)
		} else
		    call imunmap (MIM_IM(mim))
	    } else {
		nlines = max (2*MIM_MSIEDGE(mim)+(line2-line1+1),
		    MIM_MSIMAX(mim) / nc)
		line1 = max (1, min (nl, line1 - MIM_MSIEDGE(mim)))
		line2 = max (1, min (nl, line1 + nlines - 1))
		line1 = max (1, min (nl, line2 - nlines + 1))
		nlines = line2 - line1 + 1
		ptr = imgs2r (MIM_IM(mim), 1, nc, line1, line2)
		call msifit (msi, Memr[ptr], nc, nlines, nc)
	    }
	    MIM_LINE1(mim) = line1
	    MIM_LINE2(mim) = line2
	}

	# Interpolate input image to a line in the reference image.
	call msivector (msi, Memr[x], Memr[y], Memr[z], ncref)

	return (z)
end


# MIM_OPEN -- Open an image matched to a reference image.
#
# Fitting of any interpolator is later.  This allows calls to reset
# the interpolation type, edge buffer, and maximum size to fit. 

pointer procedure mim_open (input, refim)

char	input[ARB]	#I Input image name
pointer	refim		#I Reference image
pointer	mim		#O Map pointer returned

bool	interp, rotate
int	i, nc, nl, ncref, nlref, ilt[6]
double	lt[6], ltref[6], ltin[6]
pointer	sp, section, im, mw, ct, x, ptr

int	strlen(), btoi()
pointer	immap(), mw_openim(), mw_sctran()
errchk	calloc, malloc
errchk	immap
errchk	mw_openim, mw_invertd, mw_sctran

begin
	call smark (sp)
	call salloc (section, SZ_FNAME, TY_CHAR)

	iferr {
	    mim = NULL; im = NULL; mw = NULL

	    call calloc (mim, MIM_LEN, TY_STRUCT)
	    MIM_DELETE(mim) = NO

	    call imgimage (input, Memc[section], SZ_FNAME)
	    ptr = immap (Memc[section], READ_ONLY, 0); im = ptr
	    nc = IM_LEN(im,1)
	    nl = IM_LEN(im,2)
	    ncref = IM_LEN(refim,1)
	    nlref = IM_LEN(refim,2)

	    # Check relationship between reference and input images in physical
	    # coordinates.

	    ptr = mw_openim (refim); mw = ptr 
	    call mw_gltermd (mw, lt, lt[5], 2)
	    call mw_close (mw)

	    mw = mw_openim (im)
	    call mw_gltermd (mw, ltin, ltin[5], 2)

	    # Combine lterms.
	    call mw_invertd (lt, ltref, 2)
	    call mw_mmuld (ltref, ltin, lt, 2)
	    call mw_vmuld (lt, lt[5], lt[5], 2)
	    lt[5] = ltin[5] - lt[5]
	    lt[6] = ltin[6] - lt[6]
	    do i = 1, 6
		lt[i] = nint (1D6 * lt[i]) / 1D6

	    # Check if interpolation is required.
	    interp = false
	    do i = 1, 6 {
		ilt[i] = nint (lt[i])
		if (lt[i] - ilt[i] > 1D-3) {
		    interp = true
		    break
		}
	    }
	    if (lt[2] != 0. || lt[3] != 0.)
		rotate = true
	    else
		rotate = false
	    if (!interp && rotate)
		interp = true

	    if (interp) {
		# Use IMIO to extract a smaller section if possible to
		# minimize the requirements for the interpolation.
		# This could be more general if we deal with a section
		# of a rotated image.

		if (!rotate) {
		    ilt[1] = lt[1] + lt[5]
		    ilt[2] = lt[1] * ncref + lt[5] + 0.999
		    ilt[3] = lt[3] + lt[4] + lt[6]
		    ilt[4] = lt[4] * nlref + lt[6] + 0.999
		    ilt[1] = max (1, min (nc, ilt[1]))
		    ilt[2] = max (1, min (nc, ilt[2]))
		    ilt[3] = max (1, min (nl, ilt[3]))
		    ilt[4] = max (1, min (nl, ilt[4]))
		    if (ilt[1]!=1 || ilt[2]!=nc ||ilt[1]!=1 || ilt[2]!=nl) {
			i = strlen(Memc[section]) + 1
			call sprintf (Memc[section+i-1], SZ_FNAME-i,
			    "[%d:%d,%d:%d]")
			    call pargi (ilt[1])
			    call pargi (ilt[2])
			    call pargi (ilt[3])
			    call pargi (ilt[4])
			call imunmap (im)
			im = immap (Memc[section], READ_ONLY, 0)
			nc = IM_LEN(im,1)
			nl = IM_LEN(im,2)
			lt[5] = lt[5] - ilt[1] + 1
			lt[6] = lt[6] - ilt[3] + 1
		    }
		}

		# Set reference logical to input logical transformation.
		# The reference logical coordinates are the physical
		# coordinates of the transformation.

		call mw_sltermd (mw, lt, lt[5], 2)

		# If there are cross terms set the x array to the reference
		# logical coordinates (physical transformation coordinates).
		# Otherwise we only need to evalute x array once in the
		# input logical coordinates to be interpolated.

		call malloc (x, ncref, TY_REAL)
		do i = 1, ncref
		    Memr[x+i-1] = i
		if (rotate)
		    ct = mw_sctran (mw, "physical", "logical", 3B)
		else {
		    ct = mw_sctran (mw, "physical", "logical", 1B)
		    call mw_v1tranr (ct, Memr[x], Memr[x], ncref)
		    ptr = x
		    do i = 1, ncref {
			Memr[ptr] = max (1., min (real(nc), Memr[ptr]))
			ptr = ptr + 1
		    }
		    call mw_ctfree (ct)
		    ct = mw_sctran (mw, "physical", "logical", 2B)
		}

		MIM_X(mim) = x
		call malloc (MIM_Y(mim), ncref, TY_REAL)
		call malloc (MIM_Z(mim), ncref, TY_REAL)
		MIM_MW(mim) = mw
		MIM_CT(mim) = ct
		MIM_MSITYPE(mim) = MIM_MSITYPEDEF
		MIM_MSIEDGE(mim) = MIM_MSIEDGEDEF
		MIM_MSIMAX(mim) = MIM_MSIMAXDEF

	    } else {
		# If ref is a subraster of the input use IMIO section to match.
		if (ilt[1]!=1 || ilt[4]!=1 || ilt[5]!=0 || ilt[6]!=0) {
		    i = strlen(Memc[section]) + 1
		    call sprintf (Memc[section+i-1], SZ_FNAME-i,
			"[%d:%d:%d,%d:%d:%d]")
			call pargi (ilt[1]+ilt[5])
			call pargi (ilt[1]*ncref+ilt[5])
			call pargi (ilt[1])
			call pargi (ilt[4]+ilt[6])
			call pargi (ilt[4]*nlref+ilt[6])
			call pargi (ilt[4])
		    call imunmap (im)
		    im = immap (Memc[section], READ_ONLY, 0)
		    nc = IM_LEN(im,1)
		    nl = IM_LEN(im,2)
		}
		call mw_close (mw)
	    }

	    MIM_IM(mim) = im
	    MIM_INTERP(mim) = btoi (interp)
	    MIM_ROTATE(mim) = btoi (rotate)
	    MIM_NC(mim) = nc
	    MIM_NL(mim) = nl
	    MIM_NCREF(mim) = ncref
	} then {
	    if (mw != NULL)
		call mw_close (mw)
	    if (im != NULL)
		call imunmap (im)
	    call mim_close (mim)
	    call sfree (sp)
	    call erract (EA_ERROR)
	}

	call sfree (sp)
	return (mim)
end


# MIM_CLOSE -- Close mim structure.

procedure mim_close (mim)

pointer	mim			#I MIM pointer

pointer	imname
errchk	imdelete

begin
	if (mim == NULL)
	    return

	if (MIM_IM(mim) != NULL) {
	    if (MIM_DELETE(mim) == YES) {
		call malloc (imname, SZ_FNAME, TY_CHAR)
		call imstats (MIM_IM(mim), IM_IMAGENAME, Memc[imname], SZ_FNAME)
		call imgimage (Memc[imname], Memc[imname], SZ_FNAME)
		call imunmap (MIM_IM(mim))
		call imdelete (Memc[imname])
		call mfree (imname, TY_CHAR)
	    } else
		call imunmap (MIM_IM(mim))
	}
	if (MIM_MSI(mim) != NULL)
	    call msifree (MIM_MSI(mim))
	if (MIM_MW(mim) != NULL)
	    call mw_close (MIM_MW(mim))
	call mfree (MIM_X(mim), TY_REAL)
	call mfree (MIM_Y(mim), TY_REAL)
	call mfree (MIM_Z(mim), TY_REAL)
	call mfree (mim, TY_STRUCT)
end


# MIM_GETS -- Get string parameter.

procedure mim_gets (mim, param, val, maxchar)

pointer	mim		#I MIM pointer
char	param[ARB]	#I Parameter
char	val[ARB]	#O Parameter string value 
int	maxchar		#I Maximum number of characters to return

begin
	call error (1, "mim_gets: unknown parameter")
end


# MIM_GETI -- Get integer parameter.

procedure mim_geti (mim, param, val)

pointer	mim		#I MIM pointer
char	param[ARB]	#I Parameter
int	val		#O Value

bool	streq()

begin
	if (streq (param, "msitype"))
	    val = MIM_MSITYPE(mim)
	else if (streq (param, "msiedge"))
	    val = MIM_MSIEDGE(mim)
	else if (streq (param, "msimax"))
	    val = MIM_MSIMAX(mim)
	else if (streq (param, "delete"))
	    val = MIM_DELETE(mim)
	else
	    call error (1, "mim_geti: unknown parameter")
end


# MIM_GETR -- Get real parameter.

procedure mim_getr (mim, param, val)

pointer	mim		#I MIM pointer
char	param[ARB]	#I Parameter
real	val		#O Value

begin
	call error (1, "mim_getr: unknown parameter")
end


# MIM_SETS -- Set string parameter.

procedure mim_sets (mim, param, val)

pointer	mim		#I MIM pointer
char	param[ARB]	#I Parameter
char	val[ARB]	#I Value

begin
	call error (1, "mim_sets: unknown parameter")
end


# MIM_SETI -- Set integer parameter.

procedure mim_seti (mim, param, val)

pointer	mim		#I MIM pointer
char	param[ARB]	#I Parameter
int	val		#I Value

bool	streq()

begin
	if (streq (param, "msitype")) {
	    if (val != MIM_MSITYPE(mim)) {
		MIM_MSITYPE(mim) = val
		if (MIM_MSI(mim) != NULL) {
		    call msifree (MIM_MSI(mim))
		    MIM_LINE1(mim) = 0
		    MIM_LINE2(mim) = 0
		}
	    }
	} else if (streq (param, "msiedge")) {
	    if (val != max (3, MIM_MSIEDGE(mim))) {
		MIM_MSIEDGE(mim) = val
		if (MIM_MSI(mim) != NULL) {
		    call msifree (MIM_MSI(mim))
		    MIM_LINE1(mim) = 0
		    MIM_LINE2(mim) = 0
		}
	    }
	} else if (streq (param, "msimax")) {
	    if (val != max (64000, MIM_MSIMAX(mim))) {
		MIM_MSIMAX(mim) = val
		if (MIM_MSI(mim) != NULL) {
		    call msifree (MIM_MSI(mim))
		    MIM_LINE1(mim) = 0
		    MIM_LINE2(mim) = 0
		}
	    }
	} else if (streq (param, "delete"))
	    MIM_DELETE(mim) = val
	else
	    call error (1, "mim_setr: unknown parameter")
end


# MIM_SETR -- Set real parameter.

procedure mim_setr (mim, param, val)

pointer	mim		#I MIM pointer
char	param[ARB]	#I Parameter
real	val		#I Value

begin
	call error (1, "mim_setr: unknown parameter")
end
