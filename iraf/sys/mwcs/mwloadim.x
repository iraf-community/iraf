# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<syserr.h>
include	<imhdr.h>
include	<imio.h>
include	"mwcs.h"
include	"imwcs.h"

# MW_LOADIM -- Load a MWCS object saved in an image header in FITS format
# into an MWCS descriptor.  Note that the MWCS descriptor is allocated
# if the input is NULL.  This is to allow the WCS cards to be read to
# determine the WCS dimensionality.

procedure mw_loadim (mw, im)

pointer	mw			#U pointer to MWCS descriptor
pointer	im			#I pointer to image header

bool	have_wcs
int	ndim, i, j, ea_type
int	axno[MAX_DIM], axval[MAX_DIM]
double	maxval
pointer	sp, sysname, iw, ct, wp, cp, bufp, ip

int	mw_allocd(), mw_refstr(), ctoi(), envgeti()
pointer	iw_rfits(), iw_findcard(), iw_gbigfits(), mw_open()
errchk	iw_rfits, mw_allocd, mw_newsystem, mw_swtype, iw_enterwcs, mw_saxmap
errchk	mw_open
string	s_physical "physical"
define	axerr_ 91
define	axinit_ 92

begin
	call smark (sp)
	call salloc (sysname, SZ_FNAME, TY_CHAR)

	# Read the FITS image header into an IMWCS descriptor.
	iw = iw_rfits (mw, im, RF_REFERENCE)
	if (mw == NULL) {
	    ndim = max (IW_NDIM(iw), IM_NPHYSDIM(im))
	    mw = mw_open (NULL, ndim)
	}
	ndim = IW_NDIM(iw)

	# Initialize the MWCS descriptor from the IMWCS descriptor.
	# Free any storage associated with the old descriptor.
	# Start with any still allocated CTRAN descriptors.

	do i = 1, MAX_CTRAN {
	    ct = MI_CTRAN(mw,i)
	    if (ct != NULL)
		iferr (call mw_ctfree (ct))
		    call erract (EA_WARN)
	}

	# Free the old string and data buffers.
	if (MI_SBUF(mw) != NULL)
	    call mfree (MI_SBUF(mw), TY_CHAR)
	if (MI_DBUF(mw) != NULL)
	    call mfree (MI_DBUF(mw), TY_DOUBLE)

	# Initialize the new descriptor.
	call aclri (Memi[mw], LEN_MWCS)

	MI_MAGIC(mw) = MWCS_MAGIC
	MI_REFIM(mw) = im
	MI_NDIM(mw) = ndim
	MI_LTV(mw) = mw_allocd (mw, ndim)
	MI_LTM(mw) = mw_allocd (mw, ndim * ndim)

	# Set the Lterm.  Set axes with no LTM scales to unit scales.
	# Issue a warning by default but use "wcs_matrix_err" to allow
	# setting other error actions.

	call amovd (IW_LTV(iw,1), D(mw,MI_LTV(mw)), ndim)
	if (iw_findcard (iw, TY_LTM, ERR, 0) != NULL) {
	    do i = 1, ndim {
		maxval = 0D0
		do j = 1, ndim {
		    D(mw,MI_LTM(mw)+(j-1)*ndim+(i-1)) = IW_LTM(iw,i,j)
		    maxval = max (maxval, abs (IW_LTM(iw,i,j)))
		}
		if (maxval == 0D0) {
		    iferr (ea_type = envgeti ("wcs_matrix_err"))
			ea_type = EA_WARN
		    iferr {
			switch (ea_type) {
			case EA_FATAL, EA_ERROR:
			    call sprintf (Memc[sysname], SZ_FNAME, 
				"LTM keywords for axis %d undefined")
				call pargi (i)
			    call error (SYS_MWMISSAX, Memc[sysname])
			case EA_WARN:
			    IW_LTM(iw,i,i) = 1D0
			    D(mw,MI_LTM(mw)+(i-1)*ndim+(i-1)) = IW_LTM(iw,i,i)
			    call sprintf (Memc[sysname], SZ_FNAME, 
				"setting LTM%d_%d to %.4g")
				call pargi (i)
				call pargi (i)
				call pargd (IW_LTM(iw,i,i))
			    call error (SYS_MWMISSAX, Memc[sysname])
			default:
			    IW_LTM(iw,i,i) = 1D0
			    D(mw,MI_LTM(mw)+(i-1)*ndim+(i-1)) = IW_LTM(iw,i,i)
			}
		    } then
			call erract (ea_type)
		}
	    }
	} else
	    call mw_mkidmd (D(mw,MI_LTM(mw)), ndim)

	# Set up the builtin world systems "physical" and "logical".
	# Both are linear systems.  The physical system is a unitary
	# transformation (since world systems are defined relative to
	# the physical system), and the logical system has the Lterm
	# for its linear term.  No wcs attributes other than wtype are
	# defined.

	# Create the physical system.
	call mw_newsystem (mw, s_physical, ndim)
	do i = 1, ndim
	    call mw_swtype (mw, i, 1, "linear", "")

	# Create the logical system.
	call mw_newsystem (mw, "logical", ndim)
	do i = 1, ndim
	    call mw_swtype (mw, i, 1, "linear", "")

	# Set W and CD for the logical system to point to the Lterm.
	wp = MI_WCS(mw)
	WCS_W(wp) = MI_LTV(mw)
	WCS_CD(wp) = MI_LTM(mw)

	# Did the image header specify a WCS?
	have_wcs = false
	do i = 1, IW_NCARDS(iw) {
	    cp = IW_CARD(iw,i)
	    switch (C_TYPE(cp)) {
	    case TY_CTYPE, TY_CRPIX, TY_CRVAL, TY_CD, TY_CDELT:
		have_wcs = true
		break
	    }
	}

	# Enter the saved WCS.  We make up a system name for now, and patch
	# it up later once the real name has been recalled along with the
	# attributes.

	if (have_wcs) {
	    call mw_newsystem (mw, "image", ndim)
	    call iw_enterwcs (mw, iw, ndim)
	    ifnoerr {
		call mw_gwattrs (mw, 0, "system", Memc[sysname], SZ_FNAME)
	    } then
		WCS_SYSTEM(MI_WCS(mw)) = mw_refstr (mw, Memc[sysname])
	}

	# Restore the saved WCS axis map if any.
	if (iw_findcard (iw, TY_WAXMAP, ERR, 0) != NULL) {
	    bufp = iw_gbigfits (iw, TY_WAXMAP, ERR)

	    ip = bufp
	    do i = 1, ndim {
		if (ctoi (Memc, ip, axno[i]) <= 0)
		    goto axerr_
		if (ctoi (Memc, ip, axval[i]) <= 0) {
axerr_		    call eprintf ("Image %s: cannot decode WAXMAP\n")
			call pargstr (IM_NAME(IW_IM(iw)))
		    goto axinit_
		}
	    }

	    call mfree (bufp, TY_CHAR)
	    call mw_saxmap (mw, axno, axval, ndim)

	} else {
axinit_	    do i = 1, ndim {
		MI_AXNO(mw,i) = i
		MI_AXVAL(mw,i) = 0
	    }
	    MI_USEAXMAP(mw) = NO
	    MI_NLOGDIM(mw) = ndim
	}

	# Apply the section transform, if the image was opened with an image
	# section.  This edits the axis map restored above, if any, and must
	# be done after restoring the original WCS axis map.

	call iw_setaxmap (mw, im)

	# Set the default world system.
	call mw_sdefwcs (mw)

	call iw_cfits (iw)
	call sfree (sp)
end
