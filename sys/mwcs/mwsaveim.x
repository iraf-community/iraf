# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<imhdr.h>
include	<imio.h>
include	"imwcs.h"
include	"mwcs.h"

# MW_SAVEIM -- Save the current MWCS in an image header in FITS format.
# This is only possible to some degree.  Although the Lterm is always saved,
# only one world system can be saved.  FITS convention requires that the
# FITS wcs represent the transformation from image (logical) coordinates
# to world coordinates, whereas the MWCS Wterm represents the physical to
# world transformation, so what we save is actually a combination of the
# Wterm and Lterm; combining the two is only possible if there are no
# rotations between dissimilar axes.  Sampled WCS vectors and WCS attributes
# can be saved, although this can be inefficient for large vectors and can
# result in header overflow, and there can be problems preserving the
# precision of double precision data since the FITS representation is ASCII.
# Since the WCS is represented by a variable set of cards, we must be careful
# to delete any old WCS cards which are not updated by the save operation.

procedure mw_saveim (mw, im)

pointer	mw				#I pointer to MWCS descriptor
pointer	im				#I pointer to image descriptor

double	cdelt
char	label[SZ_VALSTR]
bool	update, output_cdelt
char	kwname[SZ_KWNAME], ctype[SZ_KWNAME], axtype[4]
int	ndim, axis, fn, ira, idec, i, j, pv, wv, npts, fd
pointer	sp, iw, wp, wf, vp, cp, at, o_r, n_r, o_cd, n_cd, ltm
int	op

bool	streq(), fp_equald()
pointer	iw_rfits(), iw_findcard()
int	strncmp(), strlen(), open(), nowhite(), stridxs()
errchk	iw_rfits, mw_ssystem, iw_putarray, iw_putstr, open
include	"mwcs.com"
define	ewcs_  91

begin
	# Scan the old image header, recording all WCS cards.
	iw = iw_rfits (mw, im, RF_COPY)

	# Save the WCS dimension (not necessarily same as that of the image).
	ndim = MI_NDIM(mw)
	cp = iw_findcard (iw, TY_WCSDIM, -1, 0)
	if (cp == NULL || IW_NDIM(iw) != ndim) {
	    call strcpy ("WCSDIM", kwname, SZ_KWNAME)
	    if (cp == NULL)
		call imaddf (im, kwname, "i")
	    call imputi (im, kwname, ndim)
	}
	if (cp != NULL)
	    C_UPDATED(cp) = YES

	call smark (sp)
	call salloc (o_r, ndim, TY_DOUBLE)
	call salloc (n_r, ndim, TY_DOUBLE)
	call salloc (o_cd, ndim*ndim, TY_DOUBLE)
	call salloc (n_cd, ndim*ndim, TY_DOUBLE)
	call salloc (ltm, ndim*ndim, TY_DOUBLE)

	# Get pointer to the world system to be saved.  Currently only one
	# such system can be saved since the image header is FITS based and
	# FITS doesn't support multiple world coordinate systems.  The system
	# to be saved can be set by calling MW_SSYSTEM before doing the
	# mw_saveim.

	wp = MI_WCS(mw)

	# Do we need to save any WCS information at all?
	if (MI_NWCS(mw) <= 2)
	    goto ewcs_

	# Store the current WCS in the image header.  This is optimized to
	# use the knowledge of the current header contents obtained by
	# iw_rfits above, to determine if each header card needs to be
	# modified in the header, or added to the header.  If the card
	# already exists with the correct value nothing is done.

	# Output CTYPEi for each axis.
	do axis = 1, ndim {

	    # Get the new value of CTYPEi.
	    if (WCS_AXCLASS(wp,axis) == F_LINEAR) {
		# For the default case of a linear axis, set CTYPEi to the
		# value of the axis label, if there is one and it is a simple
		# keyword but not one of the CTYPE keywords reserved by MWCS.

		call strcpy ("LINEAR  ", ctype, SZ_KWNAME)
		ifnoerr {
		    call mw_gwattrs (mw, axis, "label", label, SZ_VALSTR)
		} then {
		    call strupr (label)
		    if (nowhite (label, label, SZ_VALSTR) <= SZ_KWNAME) {
			if (strncmp (label, "SAMPLED", 8) != 0 &&
			    strncmp (label, "RA--", 4) != 0 &&
			    strncmp (label, "DEC-", 4) != 0 &&
			    strncmp (label[2], "LON", 3) != 0 &&
			    strncmp (label[2], "LAT", 3) != 0) {

			    call sprintf (ctype, SZ_KWNAME, "%-8s%9t")
				call pargstr (label)
			}
		    }
		}

	    } else {
		wf = WCS_FUNC(wp,WCS_AXCLASS(wp,axis))
		fn = WF_FN(wf)

		if (and (FN_FLAGS(fn), F_RADEC) != 0) {
		    # Determine the axis type.
		    ira = 0
		    idec = 0
		    axtype[1] = EOS
		    do i = 1, 2 {
			ifnoerr (call mw_gwattrs (mw,
				WF_AXIS(wf,i), "axtype", axtype, 4)) {
			    call strlwr (axtype)
			    if (streq (axtype, "ra") ||
				    streq (axtype[2], "lon")) {
				ira  = i
				idec = 3 - i
				break
			    } else if (streq (axtype, "dec") ||
				    streq (axtype[2], "lat")) {
				ira  = 3 - i
				idec = i
				break
			    } 
			}
		    }

		    # RA and DEC had better be flagged, but if not, assume
		    # that the first axis is RA and the second DEC.

		    if (ira == 0)
			ira = 1
		    if (idec == 0)
			idec = 2
			
		    # Make a name like "RA---TAN".
		    if (WF_AXIS(wf,idec) == axis) {
			if (streq (axtype, "ra") || streq (axtype, "dec")) {
			    call strcpy ("DEC-----", ctype, SZ_KWNAME)
			} else if (streq (axtype[2], "lon") ||
				streq (axtype[2], "lat")) {
			    call sprintf (ctype, SZ_KWNAME, "%cLAT----")
				call pargc (axtype[1])
			} else {
			    call strcpy ("DEC-----", ctype, SZ_KWNAME)
			}
		    } else {
			if (streq (axtype, "ra") || streq (axtype, "dec")) {
			    call strcpy ("RA------", ctype, SZ_KWNAME)
			} else if (streq (axtype[2], "lon") ||
				streq (axtype[2], "lat")) {
			    call sprintf (ctype, SZ_KWNAME, "%cLON----")
				call pargc (axtype[1])
			} else {
			    call strcpy ("RA------", ctype, SZ_KWNAME)
			}
		    }

		    op = max (1, SZ_KWNAME - strlen (FN_NAME(fn)) + 1)
		    call strcpy (FN_NAME(fn), ctype[op], SZ_KWNAME-op+1)
		    call strupr (ctype)

		} else {
		    # Just output the WCS function name as CTYPE.
		    call strcpy ("        ", ctype, SZ_KWNAME)
		    call strcpy (FN_NAME(fn), ctype, SZ_KWNAME)
		    call strupr (ctype)
		}
	    }

	    # Update the header value if there is any change.
	    update = true
	    vp = IW_CTYPE(iw,axis)
	    if (vp != NULL)
		update = (strncmp (Memc[vp], ctype, SZ_KWNAME) != 0)

	    cp = iw_findcard (iw, TY_CTYPE, axis, 0)
	    if (update) {
		call sprintf (kwname, SZ_KWNAME, "CTYPE%d")
		    call pargi (axis)
		if (cp == NULL)
		    call imaddf (im, kwname, "c")
		call impstr (im, kwname, ctype)
	    }
	    if (cp != NULL)
		C_UPDATED(cp) = YES
	}

	# FITS requires that the WCS specify the transformation from raw
	# image (logical) coordinates to world coordinates, whereas the
	# MWCS Wterm specifies the transformation from physical coordinates
	# to world coordinates.  Hence, we must modify CD and CRPIX (R)
	# to specify the transformation from logical to world coordinates.

	# Get the MWCS R vector.
	if (WCS_R(wp) != NULL)
	    call amovd (D(mw,WCS_R(wp)), Memd[o_r], ndim)
	else
	    call aclrd (Memd[o_r], ndim)

	# Get the MWCS CD matrix.
	if (WCS_CD(wp) != NULL)
	    call amovd (D(mw,WCS_CD(wp)), Memd[o_cd], ndim*ndim)
	else
	    call mw_mkidmd (Memd[o_cd], ndim)
	    
	# Output CRVAL (this is unaffected by the Lterm).
	if (WCS_W(wp) != NULL)
	    call iw_putarray (iw, D(mw,WCS_W(wp)), IW_CRVAL(iw,1), ndim,
		"CRVAL%d", TY_CRVAL, 0)

	# Output CRPIX = R' =  (LTM * R + LTV).
	call mw_vmuld (D(mw,MI_LTM(mw)), Memd[o_r], Memd[n_r], ndim)
	call aaddd (D(mw,MI_LTV(mw)), Memd[n_r], Memd[n_r], ndim)
	call iw_putarray (iw, Memd[n_r], IW_CRPIX(iw,1), ndim,
	    "CRPIX%d", TY_CRPIX, 0)

	# Output the CD matrix = CD' =  (CD * inv(LTM)).  If the system
	# dimensionality is 2 or less and there is no rotation, output
	# the CDELT notation in addition to the CD matrix to enhance
	# compatibility with older programs.

	call mw_invertd (D(mw,MI_LTM(mw)), Memd[ltm], ndim)
	call mw_mmuld (Memd[o_cd], Memd[ltm], Memd[n_cd], ndim)

	# Output CDELT1/CDELT2 if the image dimension is 2 or less and the
	# CD matrix is a diagonal matrix (no rotational or skew terms).

	output_cdelt = false
	if (ndim == 1)
	    output_cdelt = true
	else if (ndim == 2) {
	    output_cdelt = (fp_equald(Memd[n_cd+1],0.0D0) &&
			    fp_equald(Memd[n_cd+2],0.0D0))
	}

	if (output_cdelt) {
	    do j = 1, ndim {
		cdelt = Memd[n_cd+(j-1)*(ndim+1)]
		cp = iw_findcard (iw, TY_CDELT, j, 0)
		if (cp == NULL || !fp_equald(IW_CDELT(iw,j),cdelt)) {
		    call sprintf (kwname, SZ_KWNAME, "CDELT%d")
			call pargi (j)
		    if (cp == NULL)
			call imaddf (im, kwname, "d")
		    call imputd (im, kwname, cdelt)
		}
		if (cp != NULL)
		    C_UPDATED(cp) = YES
	    }
	}

	# Update the CD matrix.
	do j = 1, ndim {
	    call sprintf (kwname, SZ_KWNAME, "CD%d_%%d")
		call pargi (j)
	    call iw_putarray (iw, Memd[n_cd+(j-1)*ndim],
		IW_CD(iw,1,j), ndim, kwname, TY_CD, j)
	}

	# Output the Lterm.
ewcs_
	# Output LTV.
	if (MI_LTV(mw) != NULL)
	    call iw_putarray (iw, D(mw,MI_LTV(mw)), IW_LTV(iw,1), ndim,
		"LTV%d", TY_LTV, 0)

	# Output LTM.
	if (MI_LTM(mw) != NULL) {
	    do j = 1, ndim {
		call sprintf (kwname, SZ_KWNAME, "LTM%%d_%d")
		    call pargi (j)
		call iw_putarray (iw, D(mw,MI_LTM(mw)+(j-1)*ndim),
		    IW_LTM(iw,1,j), ndim, kwname, TY_LTM, j)
	    }
	}

	# Output axis map if any.
	if (MI_USEAXMAP(mw) == YES) {
	    fd = open ("WAXMAP", READ_WRITE, SPOOL_FILE)
	    axis = ERR

	    do i = 1, ndim {
		call fprintf (fd, "%d %d ")
		    call pargi (MI_AXNO(mw,i))
		    call pargi (MI_AXVAL(mw,i))
	    }

	    # Output successive WAXMAPj FITS cards.
	    call seek (fd, BOFL)
	    call iw_putstr (fd, iw, axis, TY_WAXMAP, "WAXMAP%02d", "", 0)
	    call close (fd)
	}

	# Output any WCS attributes.
	do axis = 0, ndim {
	    fd = open ("WAT", READ_WRITE, SPOOL_FILE)
	    npts = 0

	    # Dump the attribute=value assignments for this axis into a single
	    # large string buffer, using a spool file.

	    do i = 1, WCS_NWATTR(wp) {
		at = WCS_WATTR(wp,i)
		if (AT_AXIS(at) != axis)
		    next

		if (npts > 0)
		    call putline (fd, " ")
		call putline (fd, S(mw,AT_NAME(at)))
		if (stridxs (" \t", S(mw,(AT_VALUE(at)))) > 0) {
		    call putline (fd, " = \"")
		    call putline (fd, S(mw,AT_VALUE(at)))
		    call putline (fd, "\"")
		} else {
		    call putline (fd, "=")
		    call putline (fd, S(mw,AT_VALUE(at)))
		}

		npts = npts + 1
	    }

	    # Output successive WATi_jjj FITS cards.
	    call seek (fd, BOFL)
	    if (npts > 0)
		call iw_putstr (fd, iw, axis, TY_WATDATA, "WAT%d_%03d",
		    "WAT%d%04d", 999)
	    call close (fd)
	}

	# Update any sampled WCS in the header.
	do axis = 1, ndim {
	    npts = WCS_NPTS(wp,axis)
	    if (npts == 0)
		next

	    # Update the LEN card.
	    cp = iw_findcard (iw, TY_WSVLEN, axis, 0)
	    if (IW_WSVLEN(iw,axis) != npts) {
		call sprintf (kwname, SZ_KWNAME, "WSV%d_LEN")
		    call pargi (axis)
		if (cp == NULL)
		    call imaddf (im, kwname, "i")
		call imputi (im, kwname, npts)
	    }
	    if (cp != NULL)
		C_UPDATED(cp) = YES

	    pv = WCS_PV(wp,axis)
	    wv = WCS_WV(wp,axis)

	    # Dump the entire array into an ASCII spool file as successive
	    # points [PV,WV].

	    fd = open ("WSV", READ_WRITE, SPOOL_FILE)
	    do i = 1, npts {
		call fprintf (fd, "%0.*g %0.*g ")
		    call pargi (NDIGITS_DP);  call pargd (D(mw,pv+i-1))
		    call pargi (NDIGITS_DP);  call pargd (D(mw,wv+i-1))
	    }

	    # Output successive WSVi_jjj FITS cards.
	    call seek (fd, BOFL)
	    call iw_putstr (fd, iw, axis, TY_WSVDATA, "WSV%d_%03d",
		"WSV%d%04d", 999)
	    call close (fd)
	}

	# Delete any old WCS cards which were not updated, and hence which
	# are no longer valid, or which are not needed because the value is
	# the default (in which case the old card is probably invalid).

	do i = 1, IW_NCARDS(iw) {
	    cp = IW_CARD(iw,i)
	    if (C_UPDATED(cp) == NO) {
		call strcpy (Memc[C_RP(cp)], kwname, SZ_KWNAME)
		if (nowhite (kwname, kwname, SZ_KWNAME) > 0)
		    call imdelf (im, kwname)
	    }
	}

	call iw_cfits (iw)
	call sfree (sp)
end
