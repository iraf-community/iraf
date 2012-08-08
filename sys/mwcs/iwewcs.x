# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<syserr.h>
include	<ctype.h>
include	<imhdr.h>
include	<imio.h>
include	<math.h>
include	"mwcs.h"
include	"imwcs.h"

# IW_ENTERWCS -- Enter a WCS as represented in an IMWCS (FITS oriented)
# wcs descriptor into an MWCS descriptor.  This routine is called by MW_LOADIM
# after IW_RFITS has been called to scan a FITS image header to build the
# IMWCS descriptor used as input here.

procedure iw_enterwcs (mw, iw, ndim)

pointer	mw			#I pointer to MWCS descriptor
pointer	iw			#I pointer to IMWCS descriptor
int	ndim			#I system dimension

double	theta
char	ctype[8]
bool	have_ltm, have_ltv, have_wattr
int	axes[2], axis, npts, ch, ip, raax, decax, ax1, ax2, i, j, ea_type
double	maxval
pointer	sp, r, o_r, cd, ltm, cp, rp, bufp, pv, wv, o_cd, o_ltm, str

bool	streq()
pointer	iw_gbigfits(), iw_findcard()
int	strncmp(), ctod(), strldxs(), envgeti()
errchk	mw_swtermd, iw_gbigfits, malloc, mw_swtype, mw_swsampd
define	samperr_ 91

begin
	call smark (sp)
	call salloc (r, ndim, TY_DOUBLE)
	call salloc (o_r, ndim, TY_DOUBLE)
	call salloc (cd, ndim*ndim, TY_DOUBLE)
	call salloc (ltm, ndim*ndim, TY_DOUBLE)
	call salloc (o_cd, ndim*ndim, TY_DOUBLE)
	call salloc (o_ltm, ndim*ndim, TY_DOUBLE)
	call salloc (str, SZ_LINE, TY_CHAR)

	raax = 1
	decax = 2

	# Set any nonlinear functions on the axes.
	do axis = 1, ndim {
	    rp = IW_CTYPE(iw,axis)
	    if (rp == NULL)
		next

	    # Get the value of CTYPEi.  Ignore case and treat '_' and '-'
	    # as equivalent.

	    do i = 1, 8 {
		ch = Memc[rp+i-1]
		if (ch == EOS || ch == ' ' || ch == '\'')
		    break
		else if (IS_UPPER(ch))
		    ch = TO_LOWER(ch)
		else if (ch == '_')
		    ch = '-'
		ctype[i] = ch
	    }
	    ctype[i] = EOS

	    # Determine the type of function on this axis.
	    if (streq (ctype, "linear")) {
		;   # Linear is the default.

	    } else if (streq (ctype, "sampled")) {
		# A sampled WCS is an array of [P,W] points.

		bufp = iw_gbigfits (iw, TY_WSVDATA, axis)
		npts = IW_WSVLEN(iw,axis)
		call malloc (pv, npts, TY_DOUBLE)
		call malloc (wv, npts, TY_DOUBLE)

		ip = 1
		do i = 1, npts {
		    if (ctod (Memc[bufp], ip, Memd[pv+i-1]) <= 0)
			goto samperr_
		    if (ctod (Memc[bufp], ip, Memd[wv+i-1]) <= 0) {
samperr_		call eprintf (
			    "Image %s, axis %d: Cannot read sampled WCS\n")
			    call pargstr (IM_NAME(IW_IM(iw)))
			    call pargi (axis)
			break
		    }
		}

		call mw_swtype (mw, axis, 1, "sampled", "")
		call mw_swsampd (mw, axis, Memd[pv], Memd[wv], npts)

		call mfree (wv, TY_DOUBLE)
		call mfree (pv, TY_DOUBLE)
		call mfree (bufp, TY_CHAR)

	    } else if (strncmp (ctype, "ra--", 4) == 0) {
		# The projections are restricted to two axes and are indicated
		# by CTYPEi values such as, e.g., "RA---TAN" and "DEC--TAN"
		# for the TAN projection.

		raax = axis

		# Locate the DEC axis.
		decax = 0
		do j = 1, ndim {
		    cp = IW_CTYPE(iw,j)
		    if (cp != NULL)
			if (Memc[cp+3] == '-' || Memc[cp+3] == '_')
			    if (strncmp (Memc[cp], "DEC", 3) == 0 ||
				strncmp (Memc[cp], "dec", 3) == 0) {
				decax = j
				break
			    }
		}

		# Did we find it?
		if (decax == 0) {
		    call eprintf (
			"Image %s, axis %d: Cannot locate dec-%s axis\n")
			call pargstr (IM_NAME(IW_IM(iw)))
			call pargi (axis)
			call pargstr (ctype[5])
		}

		# Get the function type.
		ip = strldxs ("-", ctype) + 1

		# Assign the function to the two axes.
		axes[1] = axis
		axes[2] = decax
		call mw_swtype (mw, axes, 2, ctype[ip],
		    "axis 1: axtype=ra axis 2: axtype=dec")

	    } else if (strncmp (ctype, "dec-", 4) == 0) {
		;   # This case is handled when RA-- is seen.

	    } else if (strncmp (ctype[2], "lon-", 4) == 0) {
		# The projections are restricted to two axes and are indicated
		# by CTYPEi values such as, e.g., "xLON-TAN" and "xLAT-TAN"
		# for the TAN projection. The letter x may be any character
		# but must be the same for both the longitude and latitude
		# axes. The standard values of x are G/g for galactic, E/e
		# for ecliptic, and S/s for supergalactic coordinates.

		raax = axis

		# Locate the corresponding LAT axis.
		decax = 0
		do j = 1, ndim {
		    cp = IW_CTYPE(iw,j)
		    if (cp != NULL) {
			if (Memc[cp+4] == '-' || Memc[cp+4] == '_') {
			    if (strncmp (Memc[cp+1], "LAT", 3) == 0 ||
			        strncmp (Memc[cp+1], "lat", 3) == 0) {
			        decax = j
			        break
			    }
			}
		    }
		}

		# Did we find it?
		if (decax == 0) {
		    call eprintf (
		        "Image %s, axis %d: Cannot locate %clat%s axis\n")
		        call pargstr (IM_NAME(IW_IM(iw)))
		        call pargi (axis)
			call pargc (ctype[1])
		        call pargstr (ctype[5])
		}

		# Get the function type.
		ip = strldxs ("-", ctype) + 1

		# Assign the function to the two axes.
		axes[1] = axis
		axes[2] = decax
		call sprintf (Memc[str], SZ_LINE,
		    "axis 1: axtype=%clon axis 2: axtype=%clat")
		    call pargc (ctype[1])
		    call pargc (ctype[1])
		call mw_swtype (mw, axes, 2, ctype[ip], Memc[str])

	    } else if (strncmp (ctype[2], "lat-", 4) == 0) { 
		;   # This case is handled when xLON is seen.

	    } else if (strncmp (ctype, "multispec", 8) == 0) {
		# Multispec format image.  Axis 1,2 are coupled.
		if (axis == 1) {
		    axes[1] = 1;  axes[2] = 2
		    call mw_swtype (mw, axes, 2, "multispec", "")
		}

	    } else {
		# Since we have to be able to read any FITS header, we have
		# no control over the value of CTYPEi.  If the value is
		# something we don't know about, assume a LINEAR axis, using
		# the given value of CTYPEi as the default axis label.

		call mw_swattrs (mw, axis, "label", ctype)
	    }
	}

	# Compute the CD matrix, or verify that one was read.  Either the
	# CD matrix was input, the CROTA/CDELT representation was input,
	# or nothing was input, in which case we have the identity matrix.

	if (iw_findcard (iw, TY_CD, ERR, 0) == NULL) {
	    # Initialize CD matrix to the identity matrix.  Can't use mw_mkidm
	    # here as IW_CD is not dimensioned ndim.

	    do j = 1, ndim {
		do i = 1, ndim
		    IW_CD(iw,i,j) = 0.0
		IW_CD(iw,j,j) = 1.0
	    }

	    # Convert CDELT/CROTA to CD matrix.
	    if (iw_findcard (iw, TY_CDELT, ERR, 0) != NULL) {
		theta = DEGTORAD(IW_CROTA(iw))
		ax1 = raax
		ax2 = decax
		IW_CD(iw,ax1,ax1) =  IW_CDELT(iw,ax1) * cos(theta)
		IW_CD(iw,ax1,ax2) =  IW_CDELT(iw,ax1) * sin(theta)
		IW_CD(iw,ax2,ax1) = -IW_CDELT(iw,ax2) * sin(theta)
		IW_CD(iw,ax2,ax2) =  IW_CDELT(iw,ax2) * cos(theta)
	    }

	    do j = 1, ndim {
		if (j == raax || j == decax)
		    next
		IW_CD(iw,j,j) =  IW_CDELT(iw,j)
	    }
	}

	# Set axes with no scales to unit scales.  Issue a warning by
	# default but use "wcs_matrix_err" to allow setting other error
	# actions.

	do i = 1, ndim {
	    maxval = 0D0
	    do j = 1, ndim
		maxval = max (maxval, abs(IW_CD(iw,i,j)))
	    if (maxval == 0D0) {
		iferr (ea_type = envgeti ("wcs_matrix_err"))
		    ea_type = EA_WARN
		iferr {
		    switch (ea_type) {
		    case EA_FATAL, EA_ERROR:
			call sprintf (Memc[str], SZ_FNAME, 
			    "CD keywords for axis %d undefined")
			    call pargi (i)
			call error (SYS_MWMISSAX, Memc[str])
		    case EA_WARN:
			IW_CD(iw,i,i) = 1D0
			call sprintf (Memc[str], SZ_LINE, 
			    "setting CD%d_%d to %.4g")
			    call pargi (i)
			    call pargi (i)
			    call pargd (IW_CD(iw,i,i))
			call error (SYS_MWMISSAX, Memc[str])
		    default:
			IW_CD(iw,i,i) = 1D0
		    }
		} then
		    call erract (ea_type)
	    }
	}

	# Extract an NDIM submatrix from LTM and CD.
	do j = 1, ndim
	    do i = 1, ndim {
		Memd[o_cd+(j-1)*ndim+(i-1)] = IW_CD(iw,i,j)
		Memd[o_ltm+(j-1)*ndim+(i-1)] = IW_LTM(iw,i,j)
	    }

	# Set the linear portion of the Wterm.  First we have to transform
	# it from the FITS logical->world representation to the MWCS
	# physical->world form, by separating out the Lterm.  We have
	# CD = CD' * LTM and R = inv(LTM) * (R' - LTV), where CD' and R' are
	# the FITS versions of the MWCS CD matrix and R vector (CRPIX), and
	# LTM and LTV are the Lterm rotation matrix and translation vector.

	# First, determine if either LTM or LTV was specified in the header.
	have_ltm = (iw_findcard (iw, TY_LTM, ERR, 0) != NULL)
	have_ltv = (iw_findcard (iw, TY_LTV, ERR, 0) != NULL)

	# Compute CD = CD' * LTM.
	if (have_ltm)
	    call mw_mmuld (Memd[o_cd], Memd[o_ltm], Memd[cd], ndim)
	else
	    call amovd (Memd[o_cd], Memd[cd], ndim*ndim)

	# Compute R = inv(LTM) * (R' - LTV).
	if (have_ltm || have_ltv) {
	    call asubd (IW_CRPIX(iw,1), IW_LTV(iw,1), Memd[o_r], ndim)
	    if (have_ltm) {
		call mw_invertd (Memd[o_ltm], Memd[ltm], ndim)
		call mw_vmuld (Memd[ltm], Memd[o_r], Memd[r], ndim)
	    } else
		call amovd (Memd[o_r], Memd[r], ndim)
	} else
	    call amovd (IW_CRPIX(iw,1), Memd[r], ndim)

	# Set the Wterm.
	call mw_swtermd (mw, Memd[r], IW_CRVAL(iw,1), Memd[cd], ndim)
	# Process in any axis attributes.  The pseudo-axis 0 is used by
	# any global WCS attributes.

	do axis = 0, ndim {
	    # Is there any attribute data for axis J?
	    have_wattr = false
	    do i = 1, IW_NCARDS(iw) {
		cp = IW_CARD(iw,i)
		if (C_TYPE(cp) == TY_WATDATA && C_AXIS(cp) == axis) {
		    have_wattr = true
		    break
		}
	    }

	    # Reconstruct the attribute list and enter into MWCS.
	    if (have_wattr) {
		bufp = iw_gbigfits (iw, TY_WATDATA, axis)
		call mw_swtype (mw, axis, 1, "", Memc[bufp])
		call mfree (bufp, TY_CHAR)
	    }
	}

	call sfree (sp)
end
