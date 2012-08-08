include <imhdr.h>
include <math.h>

# This file contains the following high-level routines for converting
# between world coordinates and pixel coordinates:
#
# xt_wcs_init		initialize struct for world coordinate system
# xt_wcs_init_c		initialize from input cdelt, crota, etc
# xt_wcs_init_cd	initialize from input CD matrix, etc
# xt_wcs_free		deallocate wcs struct
# xt_wc_pix		convert from world coordinates to pixel coordinates
# xt_pix_wc		convert from pixel coordinates to world coordinates
#
# Phil Hodge, 27-Sept-1988  Created, based on code by Nelson & Zolt.
# Phil Hodge, 6-April-1990  CD matrix mult. was transposed in xt_pix_wc.
# Phil Hodge, 26-July-1991  In xt_e_ctype, change GBS to GLS (global sine).

define	LEN_WCS		136	# size of wcs struct for naxis <= 7

define	W_VALID		Memi[$1]	# coordinates valid, YES or NO?
define	W_NAXIS		Memi[$1+1]	# number of axes
define	W_RA_AX		Memi[$1+2]	# which axis is RA?  zero if none
define	W_DEC_AX	Memi[$1+3]	# which axis is Dec?  zero if none
define	W_PROJECTION	Memi[$1+4]	# projection type

#   6 is currently not used

#   7 -  55:  full CD matrix (7x7); units = e.g. degrees
#  56 - 104:  LU decomposition of CD matrix
# 105 - 111:  index returned by ludcmp for use by lubksb
# 112 - 118:  reference pixel location
# 119 - 122:  cosine & sine of declination at the reference pixel
# 123 - 136:  coordinates at crpix; units = e.g. degrees

define	W_CD		Memr[P2R($1+6 +($2-1)+($3-1)*7)]
define	W_CDLU		Memr[P2R($1+55 +($2-1)+($3-1)*7)]
define	W_CDINDX	Memr[P2R($1+104)]		# this is an array of 7
define	W_CRPIX		Memr[P2R($1+110+$2)]
define	W_COSDEC	Memd[P2D($1+118)]
define	W_SINDEC	Memd[P2D($1+120)]
define	W_CRVAL		Memd[P2D($1+120)+$2]

# Projection types.

define	W_LINEAR	0
define	W_GNOMONIC	1	# TAN
define	W_SINE		2	# SIN
define	W_ARC		3	# ARC
define	W_NORTH_POLAR	4	# NCP, north celestial pole (Westerbork)
define	W_STEREOGRAPHIC	5	# STG (conformal)
define	W_AITOFF	6	# AIT (equal-area)
define	W_GLOBAL_SINE	7	# GLS (equal-area)
define	W_MERCATOR	8	# MER (conformal)


# xt_wcs_init -- initialize wcs struct
# This routine allocates space for a structure describing the world
# coordinate system for an image, fills in the values or defaults, and
# returns a pointer to that structure.

procedure xt_wcs_init (im, wcs)

pointer im			# i: pointer to image descriptor
pointer wcs			# o: pointer to world coord system struct
#--
real	dummy			# returned by ludcmp and ignored
int	ira, idec		# index of RA, Dec axes
int	j, k			# loop indexes
errchk	xt_load_ctstruct

begin
	call calloc (wcs, LEN_WCS, TY_STRUCT)

	W_VALID(wcs) = YES			# initial value
	W_NAXIS(wcs) = IM_NDIM(im)

	call xt_load_wcsstruct (im, wcs)	# get CRVAL, etc from image

	if (W_NAXIS(wcs) >= 2) {

	    ira = W_RA_AX(wcs)
	    idec = W_DEC_AX(wcs)

	    if (idec > 0) {
		W_COSDEC(wcs) = cos (DEGTORAD(W_CRVAL(wcs,idec)))
		W_SINDEC(wcs) = sin (DEGTORAD(W_CRVAL(wcs,idec)))
	    } else {
		W_COSDEC(wcs) = 1.d0
		W_SINDEC(wcs) = 0.d0
	    }

	    # Copy the CD matrix to W_CDLU, and do the LU decomposition
	    # on W_CDLU in-place.
	    do k = 1, IM_MAXDIM
		do j = 1, IM_MAXDIM
		    W_CDLU(wcs,j,k) = W_CD(wcs,j,k)

	    iferr {
		call ludcmp (W_CDLU(wcs,1,1), W_NAXIS(wcs), IM_MAXDIM,
			W_CDINDX(wcs), dummy)
	    } then {
		call mfree (wcs, TY_STRUCT)
		call error (0, "xt_wcs_init:  cd matrix is singular")
	    }
	}
end


# xt_wcs_free -- deallocate wcs struct
# This routine deallocates space for a wcs structure.

procedure xt_wcs_free (wcs)

pointer wcs		# io: pointer to world coord system struct
#--

begin
	if (wcs != NULL)
	    call mfree (wcs, TY_STRUCT)
end


# xt_wcs_init_c -- initialize wcs struct
# xt_wcs_init_c and xt_wcs_init_cd allocate space for a structure
# describing the world coordinate system for an image, fill in the values
# or defaults, and return a pointer to that structure.  They differ from
# xt_wcs_init in that these take the coordinate parameters as arguments
# rather than getting them from the image.
# xt_wcs_init_c takes cdelt & crota, and xt_wcs_init_cd takes the CD matrix.

procedure xt_wcs_init_c (crval, crpix, cdelt, crota, ctype, naxis, wcs)

double	crval[naxis]		# i: coordinate values at reference pixel
real	crpix[naxis]		# i: reference pixel
real	cdelt[naxis]		# i: pixel spacing
real	crota			# i: rotation angle (if 2-D)
char	ctype[SZ_CTYPE,naxis]	# i: e.g. "RA---TAN"
int	naxis			# i: size of arrays
pointer wcs			# o: pointer to world coord system struct
#--
real	dummy			# returned by ludcmp and ignored
int	ira, idec		# index of RA, Dec axes
int	j, k			# loop indexes
errchk	ludcmp

begin
	do k = 1, naxis
	    if (cdelt[k] == 0.)
		call error (0, "xt_wcs_init_c:  zero value of CDELT")

	call calloc (wcs, LEN_WCS, TY_STRUCT)

	W_NAXIS(wcs) = naxis
	W_VALID(wcs) = YES		# initial value

	# Examine ctype to get ira, idec, proj_type.
	call xt_e_ctype (ctype, naxis, ira, idec, W_PROJECTION(wcs))
	W_RA_AX(wcs) = ira
	W_DEC_AX(wcs) = idec

	do k = 1, naxis {
	    W_CRVAL(wcs,k) = crval[k]
	    W_CRPIX(wcs,k) = crpix[k]
	}
	do k = naxis+1, IM_MAXDIM {
	    W_CRVAL(wcs,k) = 0.d0
	    W_CRPIX(wcs,k) = 1.
	}

	if (naxis == 1) {

	    W_CD(wcs,1,1) = cdelt[1]

	} else if (naxis >= 2) {

	    if (idec > 0) {
		W_COSDEC(wcs) = cos (DEGTORAD(W_CRVAL(wcs,idec)))
		W_SINDEC(wcs) = sin (DEGTORAD(W_CRVAL(wcs,idec)))
	    } else {
		W_COSDEC(wcs) = 1.d0
		W_SINDEC(wcs) = 0.d0
	    }

	    # Convert cdelt & crota to the CD matrix.
	    call xt_to_cd (wcs, cdelt, crota, naxis)

	    # Copy the CD matrix, and do the LU decomposition on W_CDLU.
	    do k = 1, IM_MAXDIM
		do j = 1, IM_MAXDIM
		    W_CDLU(wcs,j,k) = W_CD(wcs,j,k)

	    call ludcmp (W_CDLU(wcs,1,1), naxis, IM_MAXDIM,
			W_CDINDX(wcs), dummy)
	}
end


# xt_wcs_init_cd -- initialize wcs struct (CD)

procedure xt_wcs_init_cd (crval, crpix, cd, ctype, naxis, wcs)

double	crval[naxis]		# i: coordinate values at reference pixel
real	crpix[naxis]		# i: reference pixel
real	cd[naxis,naxis]		# i: CD matrix
char	ctype[SZ_CTYPE,naxis]	# i: e.g. "RA---TAN"
int	naxis			# i: size of arrays
pointer wcs			# o: pointer to world coord system struct
#--
real	dummy			# returned by ludcmp and ignored
int	ira, idec		# index of RA, Dec axes
int	j, k			# loop indexes

begin
	call calloc (wcs, LEN_WCS, TY_STRUCT)

	W_NAXIS(wcs) = naxis
	W_VALID(wcs) = YES		# initial value

	# Examine ctype to get ira, idec, proj_type.
	call xt_e_ctype (ctype, naxis, ira, idec, W_PROJECTION(wcs))
	W_RA_AX(wcs) = ira
	W_DEC_AX(wcs) = idec

	do k = 1, naxis {
	    W_CRVAL(wcs,k) = crval[k]
	    W_CRPIX(wcs,k) = crpix[k]
	}
	do k = naxis+1, IM_MAXDIM {
	    W_CRVAL(wcs,k) = 0.d0
	    W_CRPIX(wcs,k) = 1.
	}

	if (naxis == 1) {

	    W_CD(wcs,1,1) = cd[1,1]

	} else if (naxis >= 2) {

	    if (idec > 0) {
		W_COSDEC(wcs) = cos (DEGTORAD(W_CRVAL(wcs,idec)))
		W_SINDEC(wcs) = sin (DEGTORAD(W_CRVAL(wcs,idec)))
	    } else {
		W_COSDEC(wcs) = 1.d0
		W_SINDEC(wcs) = 0.d0
	    }

	    # Assign initial values to the CD matrix.
	    do k = 1, IM_MAXDIM {
		do j = 1, IM_MAXDIM {
		    if (j == k) {
			W_CD(wcs,k,k) = 1.
			W_CDLU(wcs,k,k) = 1.
		    } else {
			W_CD(wcs,j,k) = 0.
			W_CDLU(wcs,j,k) = 0.
		    }
		}
	    }

	    # Copy the CD matrix, and do the LU decomposition on W_CDLU.
	    do k = 1, naxis {
		do j = 1, naxis {
		    W_CD(wcs,j,k) = cd[j,k]
		    W_CDLU(wcs,j,k) = cd[j,k]
		}
	    }

	    iferr {
		call ludcmp (W_CDLU(wcs,1,1), naxis, IM_MAXDIM,
			W_CDINDX(wcs), dummy)
	    } then {
		call mfree (wcs, TY_STRUCT)
		call error (0, "xt_wcs_init_cd:  cd matrix is singular")
	    }
	}
end

# xt_to_cd -- from cdelt & crota to cd matrix
# This routine computes the CD matrix from CDELT and CROTA.

procedure xt_to_cd (wcs, cdelt, crota, naxis)

pointer wcs			# i: pointer to world coord system struct
real	cdelt[naxis]		# i: pixel spacing
real	crota			# i: rotation angle (if 2-D)
int	naxis			# i: size of arrays
#--
real	cosrota, sinrota	# cosine & sine of crota
real	sign_cdelt[2]		# one, with sign of cdelt1 or cdelt2
int	ira, idec		# index of RA, Dec axes
int	j, k			# loop indexes

begin
	ira = W_RA_AX(wcs)
	idec = W_DEC_AX(wcs)

	if ( ! IS_INDEFD(crota) ) {
	    cosrota = cos (DEGTORAD(crota))
	    sinrota = sin (DEGTORAD(crota))
	} else {
	    cosrota = 1.d0
	    sinrota = 0.d0
	}

	# Initial values for CD matrix.
	do k = 1, IM_MAXDIM {
	    do j = 1, IM_MAXDIM {
		if (j == k)
		    W_CD(wcs,k,k) = 1.
		else
		    W_CD(wcs,j,k) = 0.
	    }
	}
	do k = 1, naxis
	    W_CD(wcs,k,k) = cdelt[k]

	if (ira > 0 && idec > 0) {

	    if (cdelt[ira] >= 0.)
		sign_cdelt[1] = 1.
	    else
		sign_cdelt[1] = -1.

	    if (cdelt[idec] >= 0.)
		sign_cdelt[2] = 1.
	    else
		sign_cdelt[2] = -1.

	    W_CD(wcs,ira,ira) = cdelt[ira] * cosrota
	    W_CD(wcs,ira,idec) = abs (cdelt[idec]) * sign_cdelt[1] * sinrota
	    W_CD(wcs,idec,ira) = -abs (cdelt[ira]) * sign_cdelt[2] * sinrota
	    W_CD(wcs,idec,idec) = cdelt[idec] * cosrota
	}
end

# xt_e_ctype -- examine ctype
# Examine each element of the ctype array to find which axes (if any)
# are RA & Dec (or glon & glat, etc).  Also get the projection type,
# such as gnomonic, if this was specified in ctype.

procedure xt_e_ctype (ctype, naxis, ra_axis, dec_axis, proj_type)

char	ctype[SZ_CTYPE,naxis]	# i: coordinate type, e.g. "RA---TAN"
int	naxis			# i: dimension
int	ra_axis			# o: which axis is RA (or glon, etc)?
int	dec_axis		# o: which axis is Dec (or glat, etc)?
int	proj_type		# o: type of projection
#--
char	lctype[SZ_CTYPE]	# local copy of an element of ctype
char	dash			# '-'
int	k
int	index			# index of '-' in ctype
int	strncmp(), strldx()

begin
	# Assign defaults.
	ra_axis = 0
	dec_axis = 0
	if (naxis == 1)
	    proj_type = W_LINEAR
	else
	    proj_type = W_GNOMONIC

	# Search for "RA", "DEC", etc.
	do k = 1, naxis {
	    # Make a local copy of ctype & make sure it's upper case.
	    call strcpy (ctype[1,k], lctype, SZ_CTYPE)
	    call strupr (lctype)

	    if (strncmp (lctype, "RA", 2) == 0)
		ra_axis = k
	    else if (strncmp (lctype, "DEC", 3) == 0)
		dec_axis = k

	    else if (strncmp (lctype, "GLON", 4) == 0)
		ra_axis = k
	    else if (strncmp (lctype, "LL", 2) == 0)
		ra_axis = k
	    else if (strncmp (lctype, "UU", 2) == 0)
		ra_axis = k
	    else if (strncmp (lctype, "ELON", 4) == 0)
		ra_axis = k

	    else if (strncmp (lctype, "GLAT", 4) == 0)
		dec_axis = k
	    else if (strncmp (lctype, "MM", 2) == 0)
		dec_axis = k
	    else if (strncmp (lctype, "VV", 2) == 0)
		dec_axis = k
	    else if (strncmp (lctype, "ELAT", 4) == 0)
		dec_axis = k
	}

	if (ra_axis > 0)
	    k = ra_axis
	else if (dec_axis > 0)
	    k = dec_axis
	else
	    k = 0

	# If at least one of the axes is like RA or Dec, check to see
	# whether a projection type was specified.
	if (k > 0) {
	    dash = '-'
	    index = strldx (dash, lctype)
	    if (index > 0) {
		index = index + 1
		if (strncmp (lctype[index], "TAN", 3) == 0)
		    proj_type = W_GNOMONIC
		else if (strncmp (lctype[index], "SIN", 3) == 0)
		    proj_type = W_SINE
		else if (strncmp (lctype[index], "ARC", 3) == 0)
		    proj_type = W_ARC
		else if (strncmp (lctype[index], "NCP", 3) == 0)
		    proj_type = W_NORTH_POLAR
		else if (strncmp (lctype[index], "STG", 3) == 0)
		    proj_type = W_STEREOGRAPHIC
		else if (strncmp (lctype[index], "AIT", 3) == 0)
		    proj_type = W_AITOFF
		else if (strncmp (lctype[index], "GLS", 3) == 0)
		    proj_type = W_GLOBAL_SINE
		else if (strncmp (lctype[index], "MER", 3) == 0)
		    proj_type = W_MERCATOR
	    }
	}
end


define	SZ_PNAME	8

# xt_load_wcsstruct -- load coordinate information
# Get the coordinate information from the image, and load
# that info into the wcs structure.

procedure xt_load_wcsstruct (im, wcs)

pointer im		# i: pointer to image header struct
pointer wcs		# i: pointer to world coord system struct
#--
char	pname[SZ_PNAME]
char	ctype[SZ_CTYPE,IM_MAXDIM]
int	naxis, iax		# dimension of image; loop index for axis
bool	cdm_found		# true if CD matrix present in image
int	imaccf()
double	imgetd()
real	imgetr()
errchk	imgstr, imgetd, imgetr, xt_g_cd_matrix, xt_c_cd_matrix

begin
	naxis = IM_NDIM(im)

	# Get the coordinate info.  If anything is missing set W_VALID to NO.
	do iax = 1, naxis {

	    # CTYPE for each axis.
	    call sprintf (pname, SZ_PNAME, "ctype%d")
		call pargi (iax)
	    if (imaccf (im, pname) == YES) {
		call imgstr (im, pname, ctype[1,iax], SZ_CTYPE)
	    } else {
		call strcpy ("PIXEL", ctype[1,iax], SZ_CTYPE)
		W_VALID(wcs) = NO
	    }

	    # CRVAL for each axis
	    call sprintf (pname, SZ_PNAME, "crval%d")
		call pargi (iax)
	    if (imaccf (im, pname) == YES) {
		W_CRVAL(wcs,iax) = imgetd (im, pname)
	    } else {
		W_CRVAL(wcs,iax) = 0.d0
		W_VALID(wcs) = NO
	    }

	    # CRPIX for each axis
	    call sprintf (pname, SZ_PNAME, "crpix%d")
		call pargi (iax)
	    if (imaccf (im, pname) == YES) {
		W_CRPIX(wcs,iax) = imgetr (im, pname)
	    } else {
		W_CRPIX(wcs,iax) = 1.
		W_VALID(wcs) = NO
	    }
	}
	# Assign reasonable values to the unused elements.
	do iax = naxis+1, IM_MAXDIM {
	    W_CRVAL(wcs,iax) = 0.d0
	    W_CRPIX(wcs,iax) = 1.
	}

	# Examine ctype array.
	call xt_e_ctype (ctype, naxis,
		W_RA_AX(wcs), W_DEC_AX(wcs), W_PROJECTION(wcs))

	# First try to get the CD matrix, and if it isn't there
	# get CDELT and CROTA and convert to CD.

	call xt_g_cd_matrix (im, wcs, naxis, cdm_found)

	if ( ! cdm_found )
	    call xt_c_cd_matrix (im, wcs, naxis)
end


# xt_g_cd_matrix -- get CD matrix
# If the CD matrix is present, get the values and place them into the
# wcs structure.  Note that we assume that if *any* of the CD matrix
# parameters are there, they are *all* there.

define	TOLER	1.e-5

procedure xt_g_cd_matrix (im, wcs, naxis, cdm_found)

pointer	im			# i: image pointer
pointer wcs			# i: pointer to wcs structure
int	naxis			# i: number of axes in image
bool	cdm_found		# o: true if CD matrix found
#--
real	cd_matrix[IM_MAXDIM,IM_MAXDIM]	# the CD matrix
char	pname[SZ_PNAME]
int	i, j
int	imaccf()
real	imgetr()
errchk	imgetr

begin
	# This is reset below if any element of the CD matrix is found.
	cdm_found = false

	# Assign default values.
	do j = 1, IM_MAXDIM
	    do i= 1, IM_MAXDIM
		if (i == j)
		    cd_matrix[i,j] = 1.
		else
		    cd_matrix[i,j] = 0.

	# Get each element of the CD matrix.
	do j = 1, naxis {
	    do i = 1, naxis {
		call sprintf (pname, SZ_PNAME, "cd%d_%d")
		    call pargi (i)
		    call pargi (j)
		if (imaccf (im, pname) == YES) {
		    cd_matrix[i,j] = imgetr (im, pname)
		    cdm_found = true
		}
	    }
	}

	# Copy to the wcs structure.
	do j = 1, IM_MAXDIM
	    do i = 1, IM_MAXDIM
		W_CD(wcs,i,j) = cd_matrix[i,j]
end


# xt_c_cd_matrix -- create CD matrix
# If the CD matrix is not present, get the values of CDELT & CROTA,
# convert to the CD matrix, and store the values in the wcs structure.
# Since this is called after trying unsuccessfully to get the CD matrix,
# if cdelt or crota is not present W_VALID will be reset to NO.

procedure xt_c_cd_matrix (im, wcs, naxis)

pointer	im			# i: image pointer
pointer wcs			# i: pointer to wcs structure
int	naxis			# i: number of axes in image
#--
char	pname[SZ_PNAME]		# parameter name (e.g. "cdelt1")
real	cdelt[IM_MAXDIM]	# pixel spacing
real	crota			# rotation angle in degrees
int	k			# loop index for axis
int	imaccf()
real	imgetr()
errchk	imgetr

begin
	do k = 1, naxis {

	    # CDELT for each axis.
	    call sprintf (pname, SZ_PNAME, "cdelt%d")
		call pargi (k)
	    if (imaccf (im, pname) == YES) {
		cdelt[k] = imgetr (im, pname)
		if (cdelt[k] == 0.)
		    call error (0, "xt_c_cd_matrix:  cdelt is zero")
	    } else {
		cdelt[k] = 1.
		W_VALID(wcs) = NO
	    }
	}

	# For a 1-D image, assign CD1_1 and return.
	if (naxis == 1) {
	    W_CD(wcs,1,1) = cdelt[1]
	    return
	}

	# CROTA (only one).
	call strcpy ("crota1", pname, SZ_PNAME)
	if (imaccf (im, pname) == YES) {
	    crota = imgetr (im, pname)
	} else {
	    crota = 0.
	    W_VALID(wcs) = NO
	}

	# Compute CD matrix from CDELT & CROTA.
	call xt_to_cd (wcs, cdelt, crota, naxis)
end


# xt_wc_pix -- wcs to pixels
# This routine converts world coordinates to pixel coordinates.
#
# In the 1-D case, CRVAL is subtracted from the coordinate, the
# result is divided by CDELT (same as CD1_1), and CRPIX is added.
#
# For 2-D or higher dimension, if two of the axes are like RA and Dec,
# the input coordinates are converted to standard coordinates Xi
# and Eta.  The (Xi, Eta) vector is then multiplied on the left by
# the inverse of the CD matrix, and CRPIX is added.
# The units for axes like Ra & Dec are degrees, not hours or radians.
# For linear axes the conversion is the same as for 1-D.

procedure xt_wc_pix (wcs, phys, pix, naxis)

pointer wcs		# i: pointer to world coord system struct
double	phys[naxis]	# i: physical (world) coordinates (e.g. degrees)
real	pix[naxis]	# o: pixel coordinates
int	naxis		# i: size of arrays
#--
double	delta_ra		# RA of object - RA at reference pixel
double	dra_r, dec_r		# delta_ra & declination in radians
double	xi_r, eta_r		# xi & eta in radians
real	dphys[IM_MAXDIM]	# phys coord - reference coord
int	ira, idec		# index of RA, Dec axes
int	k			# loop index
errchk	xt_wp_ncp, xt_wp_mer

begin
	do k = 1, naxis
	    dphys[k] = phys[k] - W_CRVAL(wcs,k)

	if (naxis == 1) {

	    pix[1] = dphys[1] / W_CD(wcs,1,1) + W_CRPIX(wcs,1)

	} else {

	    ira = W_RA_AX(wcs)
	    idec = W_DEC_AX(wcs)

	    # Convert RA & Dec to Xi & Eta (standard coordinates).
	    if (ira > 0 && idec > 0) {

		delta_ra = phys[ira] - W_CRVAL(wcs,ira)		# double prec
		dra_r = DEGTORAD (delta_ra)
		dec_r = DEGTORAD (phys[idec])

		switch (W_PROJECTION(wcs)) {
		case W_GNOMONIC:
		    call xt_wp_tan (wcs, dra_r, dec_r, xi_r, eta_r)
		case W_SINE:
		    call xt_wp_sin (wcs, dra_r, dec_r, xi_r, eta_r)
		case W_ARC:
		    call xt_wp_arc (wcs, dra_r, dec_r, xi_r, eta_r)
		case W_NORTH_POLAR:
		    call xt_wp_ncp (wcs, dra_r, dec_r, xi_r, eta_r)
		case W_STEREOGRAPHIC:
		    call xt_wp_stg (wcs, dra_r, dec_r, xi_r, eta_r)
		case W_AITOFF:
		    call xt_wp_ait (wcs, dra_r, dec_r, xi_r, eta_r)
		case W_GLOBAL_SINE:
		    call xt_wp_gls (wcs, dra_r, dec_r, xi_r, eta_r)
		case W_MERCATOR:
		    call xt_wp_mer (wcs, dra_r, dec_r, xi_r, eta_r)
		}

		dphys[ira] = RADTODEG (xi_r)		# xi, eta in degrees
		dphys[idec] = RADTODEG (eta_r)
	    }

	    # Use LU backsubstitution to get pixel coords from physical coords.
	    call lubksb (W_CDLU(wcs,1,1), naxis, IM_MAXDIM,
			W_CDINDX(wcs), dphys)	# dphys is modified in-place
	    do k = 1, naxis
		pix[k] = dphys[k] + W_CRPIX(wcs,k)	# copy to output
	}
end


# xt_pix_wc -- pixels to wcs
# This routine converts pixel coordinates to world coordinates.
#
# In the 1-D case, CRPIX is subtracted from the pixel coordinate,
# the result is multiplied by CDELT (same as CD1_1), and CRVAL is added.
#
# For 2-D or higher dimension, CRPIX is subtracted, and the result is
# multiplied on the left by the CD matrix.  If two of the axes are like
# RA and Dec, the pixel coordinates are converted to standard coordinates
# Xi and Eta.  The (xi, eta) vector is then converted to differences
# between RA and Dec and CRVAL, and then CRVAL is added to each coordinate.

procedure xt_pix_wc (wcs, pix, phys, naxis)

pointer wcs		# i: pointer to world coord system struct
real	pix[naxis]	# i: pixel coordinates
double	phys[naxis]	# o: physical (world) coordinates
int	naxis		# i: size of arrays
#--
double	dpix[IM_MAXDIM]		# pix coord - crpix
double	sum			# for matrix multiplication
double	dra_r, dec_r		# delta_ra & declination in radians
double	xi_r, eta_r		# xi & eta in radians
int	ira, idec		# index of RA, Dec axes
int	j, k			# loop indexes

begin
	do k = 1, naxis
	    dpix[k] = pix[k] - W_CRPIX(wcs,k)

	if (naxis == 1) {

	    phys[1] = dpix[1] * W_CD(wcs,1,1) + W_CRVAL(wcs,1)

	} else {

	    do j = 1, naxis {
		sum = 0.d0
		do k = 1, naxis
		    sum = sum + W_CD(wcs,j,k) * dpix[k]
		phys[j] = sum
	    }

	    ira = W_RA_AX(wcs)
	    idec = W_DEC_AX(wcs)

	    # Convert Xi & Eta (standard coordinates) to RA & Dec.
	    if (ira > 0 && idec > 0) {
		xi_r = DEGTORAD (phys[ira])
		eta_r = DEGTORAD (phys[idec])

		switch (W_PROJECTION(wcs)) {
		case W_GNOMONIC:
		    call xt_pw_tan (wcs, xi_r, eta_r, dra_r, dec_r)
		case W_SINE:
		    call xt_pw_sin (wcs, xi_r, eta_r, dra_r, dec_r)
		case W_ARC:
		    call xt_pw_arc (wcs, xi_r, eta_r, dra_r, dec_r)
		case W_NORTH_POLAR:
		    call xt_pw_ncp (wcs, xi_r, eta_r, dra_r, dec_r)
		case W_STEREOGRAPHIC:
		    call xt_pw_stg (wcs, xi_r, eta_r, dra_r, dec_r)
		case W_AITOFF:
		    call xt_pw_ait (wcs, xi_r, eta_r, dra_r, dec_r)
		case W_GLOBAL_SINE:
		    call xt_pw_gls (wcs, xi_r, eta_r, dra_r, dec_r)
		case W_MERCATOR:
		    call xt_pw_mer (wcs, xi_r, eta_r, dra_r, dec_r)
		}

		phys[idec] = RADTODEG (dec_r)
		phys[ira] = RADTODEG (dra_r) + W_CRVAL(wcs,ira)
		if (phys[ira] < 0.d0)
		    phys[ira] = phys[ira] + 360.d0
	    }
	    do k = 1, naxis
		if (k != ira && k != idec)
		    phys[k] = phys[k] + W_CRVAL(wcs,k)
	}
end


# xt_wp_tan -- convert from ra & dec using gnomonic projection

procedure xt_wp_tan (wcs, dra_r, dec_r, xi_r, eta_r)

pointer wcs		# i: pointer to world coord system struct
double	dra_r		# i: RA of object - RA at reference pixel (radians)
double	dec_r		# i: declination of object (radians)
double	xi_r		# o: standard coordinate (radians)
double	eta_r		# o: standard coordinate (radians)
#--
double	cosdra, sindra	# cos & sin of dra_r
double	cosdec, sindec	# cos & sin of object declination
double	cosdist		# cos of dist from ref pixel to object

begin
	cosdra = cos (dra_r)
	sindra = sin (dra_r)

	cosdec = cos (dec_r)
	sindec = sin (dec_r)

	cosdist = sindec * W_SINDEC(wcs) + cosdec * W_COSDEC(wcs) * cosdra

	xi_r = cosdec * sindra / cosdist
	eta_r = (sindec * W_COSDEC(wcs) -
			cosdec * W_SINDEC(wcs) * cosdra) / cosdist
end


# xt_pw_tan -- convert to ra & dec using gnomonic projection
# In rectangular coordinates the vector (1, xi, eta) points toward
# the object; the origin is the observer's location, the x-axis points
# toward the reference pixel, the y-axis is in the direction of increasing
# right ascension, and the z-axis is in the direction of increasing
# declination.  The coordinate system is then rotated by the declination so
# the x-axis passes through the equator at the RA of the reference pixel;
# the components of the vector in this coordinate system are used to
# compute (RA - reference_RA) and declination.

procedure xt_pw_tan (wcs, xi_r, eta_r, dra_r, dec_r)

pointer wcs		# i: pointer to world coord system struct
double	xi_r		# i: standard coordinate (radians)
double	eta_r		# i: standard coordinate (radians)
double	dra_r		# o: RA of object - RA at reference pixel (radians)
double	dec_r		# o: declination of object (radians)
#--
double	x, y, z		# vector (not unit length) pointing toward object

begin
	# Rotate the rectangular coordinate system of the vector (1, xi, eta)
	# by the declination so the x-axis will pass through the equator.
	x = W_COSDEC(wcs) - eta_r * W_SINDEC(wcs)
	y = xi_r
	z = W_SINDEC(wcs) + eta_r * W_COSDEC(wcs)

	if (x == 0.d0 && y == 0.d0)
	    dra_r = 0.d0
	else
	    dra_r = atan2 (y, x)
	dec_r = atan2 (z, sqrt (x*x + y*y))
end


# xt_wp_sin -- convert from ra & dec using sine projection
#
# Reference:  AIPS Memo No. 27 by Eric W. Greisen

procedure xt_wp_sin (wcs, dra_r, dec_r, xi_r, eta_r)

pointer wcs		# i: pointer to world coord system struct
double	dra_r		# i: RA of object - RA at reference pixel (radians)
double	dec_r		# i: declination of object (radians)
double	xi_r		# o: standard coordinate (radians)
double	eta_r		# o: standard coordinate (radians)
#--
double	cosdra, sindra	# cos & sin of delta_ra
double	cosdec, sindec	# cos & sin of object declination

begin
	cosdra = cos (dra_r)
	sindra = sin (dra_r)

	cosdec = cos (dec_r)
	sindec = sin (dec_r)

	xi_r = cosdec * sindra
	eta_r = sindec * W_COSDEC(wcs) - cosdec * W_SINDEC(wcs) * cosdra
end


# xt_pw_sin -- convert to ra & dec using sine projection
# In rectangular coordinates the vector (v1, xi, eta), where
# v1 = sqrt (1 - xi**2 - eta**2), is the location of the object on the
# unit celestial sphere.  The x-axis points toward the reference pixel,
# the y-axis is in the direction of increasing right ascension, and the
# z-axis is in the direction of increasing declination.  The coordinate
# system is then rotated (around the y-axis) by the declination so the
# x-axis passes through the equator at the RA of the reference pixel;
# the components of the vector in this coordinate system are used to
# compute (RA - reference_RA) and declination.

procedure xt_pw_sin (wcs, xi_r, eta_r, dra_r, dec_r)

pointer wcs		# i: pointer to world coord system struct
double	xi_r		# i: standard coordinate (radians)
double	eta_r		# i: standard coordinate (radians)
double	dra_r		# o: RA of object - RA at reference pixel (radians)
double	dec_r		# o: declination of object (radians)
#--
double	v1		# x component of unit vector
double	x, y, z		# unit vector with x[1] pointing toward equator

begin
	v1 = sqrt (1.d0 - xi_r*xi_r - eta_r*eta_r)

	# Rotate the rectangular coordinate system of the vector (v1, xi, eta)
	# by the declination so the x-axis will pass through the equator.
	x = v1 * W_COSDEC(wcs) - eta_r * W_SINDEC(wcs)
	y = xi_r
	z = v1 * W_SINDEC(wcs) + eta_r * W_COSDEC(wcs)

	if (x == 0.d0 && y == 0.d0)
	    dra_r = 0.d0
	else
	    dra_r = atan2 (y, x)
	dec_r = atan2 (z, sqrt (x*x + y*y))
end


# xt_wp_arc -- convert from ra & dec using arc projection
#
# Reference:  AIPS Memo No. 27 by Eric W. Greisen

procedure xt_wp_arc (wcs, dra_r, dec_r, xi_r, eta_r)

pointer wcs		# i: pointer to world coord system struct
double	dra_r		# i: RA of object - RA at reference pixel (radians)
double	dec_r		# i: declination of object (radians)
double	xi_r		# o: standard coordinate (radians)
double	eta_r		# o: standard coordinate (radians)
#--
double	cosdra, sindra	# cos & sin of delta_ra
double	cosdec, sindec	# cos & sin of object declination
double	theta		# distance (radians) from ref pixel to object
double	r		# theta / sin (theta)

begin
	cosdra = cos (dra_r)
	sindra = sin (dra_r)

	cosdec = cos (dec_r)
	sindec = sin (dec_r)

	theta = acos (sindec * W_SINDEC(wcs) + cosdec * W_COSDEC(wcs) * cosdra)
	if (theta == 0.d0)
	    r = 1.d0
	else
	    r = theta / sin (theta)

	xi_r = r * cosdec * sindra
	eta_r = r * (sindec * W_COSDEC(wcs) - cosdec * W_SINDEC(wcs) * cosdra)
end


# xt_pw_arc -- convert to ra & dec using arc projection
# The rectangular coordinates of the pixel on a unit celestial sphere
# are computed in a coordinate system such that the x-axis points toward
# the reference pixel, the y-axis is in the direction of increasing right
# ascension, and the z-axis is in the direction of increasing declination.
# The coordinate system is then rotated (around the y-axis) by the
# declination so the x-axis passes through the equator at the RA of the
# reference pixel; the components of the vector in this coordinate system
# are used to compute (RA - reference_RA) and declination.

procedure xt_pw_arc (wcs, xi_r, eta_r, dra_r, dec_r)

pointer wcs		# i: pointer to world coord system struct
double	xi_r		# i: standard coordinate (radians)
double	eta_r		# i: standard coordinate (radians)
double	dra_r		# o: RA of object - RA at reference pixel (radians)
double	dec_r		# o: declination of object (radians)
#--
double	theta		# arc length, i.e. sqrt (xi**2 + eta**2)
double	v[3]		# unit vector with v[1] pointing toward ref pixel
double	x, y, z		# vector with x[1] pointing toward equator

begin
	theta = sqrt (xi_r*xi_r + eta_r*eta_r)
	if (theta == 0.d0) {
	    v[1] = 1.d0
	    v[2] = 0.d0
	    v[3] = 0.d0
	} else {
	    v[1] = cos (theta)
	    v[2] = sin (theta) / theta * xi_r
	    v[3] = sin (theta) / theta * eta_r
	}

	# Rotate the rectangular coordinate system of the vector v by the
	# declination so the x-axis will pass through the equator.
	x = v[1] * W_COSDEC(wcs) - v[3] * W_SINDEC(wcs)
	y = v[2]
	z = v[1] * W_SINDEC(wcs) + v[3] * W_COSDEC(wcs)

	if (x == 0.d0 && y == 0.d0)
	    dra_r = 0.d0
	else
	    dra_r = atan2 (y, x)
	dec_r = atan2 (z, sqrt (x*x + y*y))
end


# xt_wp_ncp -- convert from ra & dec using ncp projection
#
# References:
#	AIPS Memo No. 27 by Eric W. Greisen
#	Data Processing for the Westerbork Synthesis Radio Telescope
#		by W. N. Brouw

procedure xt_wp_ncp (wcs, dra_r, dec_r, xi_r, eta_r)

pointer wcs		# i: pointer to world coord system struct
double	dra_r		# i: RA of object - RA at reference pixel (radians)
double	dec_r		# i: declination of object (radians)
double	xi_r		# o: standard coordinate (radians)
double	eta_r		# o: standard coordinate (radians)
#--
double	cosdra, sindra	# cos & sin of delta_ra
double	cosdec		# cos of object declination

begin
	if (W_SINDEC(wcs) == 0.)
	    call error (1, "NCP projection:  dec is zero")

	cosdra = cos (dra_r)
	sindra = sin (dra_r)

	cosdec = cos (dec_r)

	xi_r = - cosdec * sindra
	eta_r = (W_COSDEC(wcs) - cosdec * cosdra) / W_SINDEC(wcs)
end


# xt_pw_ncp -- convert to ra & dec using ncp projection
#
# References:
#	AIPS Memo No. 27 by Eric W. Greisen
#	Data Processing for the Westerbork Synthesis Radio Telescope
#		by W. N. Brouw

procedure xt_pw_ncp (wcs, xi_r, eta_r, dra_r, dec_r)

pointer wcs		# i: pointer to world coord system struct
double	xi_r		# i: standard coordinate (radians)
double	eta_r		# i: standard coordinate (radians)
double	dra_r		# o: RA of object - RA at reference pixel (radians)
double	dec_r		# o: declination of object (radians)
#--
double	temp

begin
	temp = W_COSDEC(wcs) - eta_r * W_SINDEC(wcs)

	dra_r = atan2 (-xi_r, temp)
	dec_r = acos (temp / cos (dra_r))
	if (W_SINDEC(wcs) < 0)
	    dec_r = -dec_r
end


# xt_wp_gls -- convert from ra & dec using global-sine projection
#
# Reference:  AIPS Memo No. 46 by Eric W. Greisen

procedure xt_wp_gls (wcs, dra_r, dec_r, xi_r, eta_r)

pointer wcs		# i: pointer to world coord system struct
double	dra_r		# i: RA of object - RA at reference pixel (radians)
double	dec_r		# i: declination of object (radians)
double	xi_r		# o: standard coordinate (radians)
double	eta_r		# o: standard coordinate (radians)
#--
double	cosdec		# cos of object declination
double	temp		# delta RA
int	idec		# which axis is declination axis

begin
	cosdec = cos (dec_r)
	idec = W_DEC_AX(wcs)

	temp = dra_r

	# Put dra_r on the interval (-180,+180] degrees.
	if (temp <= -PI)
	    temp = temp + TWOPI
	if (temp > PI)
	    temp = temp - TWOPI

	xi_r = temp * cosdec

	if (idec > 0)
	    eta_r = dec_r - DEGTORAD (W_CRVAL(wcs,idec))
	else
	    eta_r = dec_r
end


# xt_pw_gls -- convert to ra & dec using global-sine projection
#
# Reference:  AIPS Memo No. 46 by Eric W. Greisen

procedure xt_pw_gls (wcs, xi_r, eta_r, dra_r, dec_r)

pointer wcs		# i: pointer to world coord system struct
double	xi_r		# i: standard coordinate (radians)
double	eta_r		# i: standard coordinate (radians)
double	dra_r		# o: RA of object - RA at reference pixel (radians)
double	dec_r		# o: declination of object (radians)
#--
double	cosdec		# cosine of object declination
int	idec		# which axis is declination axis

begin
	idec = W_DEC_AX(wcs)
	if (idec > 0)
	    dec_r = eta_r + DEGTORAD (W_CRVAL(wcs,idec))
	else
	    dec_r = eta_r

	cosdec = cos (dec_r)
	if (cosdec > 0.d0)
	    dra_r = xi_r / cosdec
	else
	    dra_r = 0.d0
end

# xt_wp_stg -- convert from ra & dec using stereographic projection
#
# Reference:  AIPS Memo No. 46 by Eric W. Greisen

procedure xt_wp_stg (wcs, dra_r, dec_r, xi_r, eta_r)

pointer wcs		# i: pointer to world coord system struct
double	dra_r		# i: RA of object - RA at reference pixel (radians)
double	dec_r		# i: declination of object (radians)
double	xi_r		# o: standard coordinate (radians)
double	eta_r		# o: standard coordinate (radians)
#--
double	cosdra, sindra	# cos & sin of dra_r
double	cosdec, sindec	# cos & sin of object declination
double	cosdist		# cos of dist from ref pixel to object
double	sincos		# sin (theta) * cos (phi)

begin
	cosdra = cos (dra_r)
	sindra = sin (dra_r)

	cosdec = cos (dec_r)
	sindec = sin (dec_r)

	cosdist = sindec * W_SINDEC(wcs) + cosdec * W_COSDEC(wcs) * cosdra
	sincos = sindec * W_COSDEC(wcs) - cosdec * W_SINDEC(wcs) * cosdra

	xi_r = 2.d0 * cosdec * sindra / (1.d0 + cosdist)
	eta_r = 2.d0 * sincos / (1.d0 + cosdist)
end


# xt_pw_stg -- convert to ra & dec using stereographic projection

procedure xt_pw_stg (wcs, xi_r, eta_r, dra_r, dec_r)

pointer wcs		# i: pointer to world coord system struct
double	xi_r		# i: standard coordinate (radians)
double	eta_r		# i: standard coordinate (radians)
double	dra_r		# o: RA of object - RA at reference pixel (radians)
double	dec_r		# o: declination of object (radians)
#--
double	rho2		# square of distance from reference pixel
double	scale		# factor to reduce xi, eta to y, z
double	x, y, z		# unit vector toward object
double	temp

begin
	rho2 = xi_r * xi_r + eta_r * eta_r

	x = (4.d0 - rho2) / (4.d0 + rho2)
	scale = (x + 1.d0) / 2.d0

	y = xi_r * scale
	z = eta_r * scale

	temp = x * W_COSDEC(wcs) - z * W_SINDEC(wcs)
	z    = x * W_SINDEC(wcs) + z * W_COSDEC(wcs)
	x = temp

	if (x == 0.d0 && y == 0.d0)
	    dra_r = 0.d0
	else
	    dra_r = atan2 (y, x)
	dec_r = atan2 (z, sqrt (x*x + y*y))
end


# xt_wp_ait -- convert from ra & dec using Aitoff projection
#
# Note that the declination at the reference pixel is ignored and is
# assumed to be zero.  The algorithms given in the AIPS reference do
# allow for a non-zero declination at the reference pixel.
#
# Reference:  AIPS Memo No. 46 by Eric W. Greisen

procedure xt_wp_ait (wcs, dra_r, dec_r, xi_r, eta_r)

pointer wcs		# i: pointer to world coord system struct
double	dra_r		# i: RA of object - RA at reference pixel (radians)
double	dec_r		# i: declination of object (radians)
double	xi_r		# o: standard coordinate (radians)
double	eta_r		# o: standard coordinate (radians)
#--
double	z		# temp variable
double	cosdec		# cosine of declination

begin
	cosdec = cos (dec_r)
	z = sqrt ((1.d0 + cosdec * cos (dra_r/2.d0)) / 2.d0)

	xi_r = 2.d0 * cosdec * sin (dra_r/2.d0) / z
	eta_r = sin (dec_r) / z
end


# xt_pw_ait -- convert to ra & dec using Aitoff projection
#
# Note that the declination at the reference pixel is ignored and is
# assumed to be zero.  The algorithms given in the AIPS reference do
# allow for a non-zero declination at the reference pixel.
#
# Reference:  AIPS Memo No. 46 by Eric W. Greisen

procedure xt_pw_ait (wcs, xi_r, eta_r, dra_r, dec_r)

pointer wcs		# i: pointer to world coord system struct
double	xi_r		# i: standard coordinate (radians)
double	eta_r		# i: standard coordinate (radians)
double	dra_r		# o: RA of object - RA at reference pixel (radians)
double	dec_r		# o: declination of object (radians)
#--
double	z		# temp variable
double	cosdec		# cosine of declination

begin
	z = sqrt (1.d0 - xi_r*xi_r/16.d0 - eta_r*eta_r/4.d0)

	dec_r = asin (eta_r * z)
	cosdec = cos (dec_r)

	if (cosdec > 0.d0) {
	    dra_r = 2.d0 * asin (xi_r * z / (2.d0 * cosdec))
	} else {
	    dra_r = 0.d0
	}
end


# xt_wp_mer -- convert from ra & dec using Mercator projection
#
# Note that the declination at the reference pixel is ignored and is
# assumed to be zero.  The algorithms given in the AIPS reference do
# allow for a non-zero declination at the reference pixel.
#
# Reference:  AIPS Memo No. 46 by Eric W. Greisen

procedure xt_wp_mer (wcs, dra_r, dec_r, xi_r, eta_r)

pointer wcs		# i: pointer to world coord system struct
double	dra_r		# i: RA of object - RA at reference pixel (radians)
double	dec_r		# i: declination of object (radians)
double	xi_r		# o: standard coordinate (radians)
double	eta_r		# o: standard coordinate (radians)
#--
double	temp

begin
	xi_r = dra_r
	temp = (dec_r + HALFPI) / 2.d0
	if (temp >= HALFPI || temp <= 0.d0)
	    call error (1, "invalid declination for Mercator projection")
	eta_r = log (tan (temp))
end


# xt_pw_mer -- convert to ra & dec using Mercator projection
#
# Reference:  AIPS Memo No. 46 by Eric W. Greisen

procedure xt_pw_mer (wcs, xi_r, eta_r, dra_r, dec_r)

pointer wcs		# i: pointer to world coord system struct
double	xi_r		# i: standard coordinate (radians)
double	eta_r		# i: standard coordinate (radians)
double	dra_r		# o: RA of object - RA at reference pixel (radians)
double	dec_r		# o: declination of object (radians)
#--

begin
	dra_r = xi_r
	dec_r = 2.d0 * atan (exp (eta_r)) - HALFPI
end
