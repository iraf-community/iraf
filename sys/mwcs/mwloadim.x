# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<imhdr.h>
include	<imio.h>
include	"mwcs.h"
include	"imwcs.h"

# MW_LOADIM -- Load a MWCS object saved in an image header in FITS format
# into an MWCS descriptor.

procedure mw_loadim (mw, im)

pointer	mw			#I pointer to MWCS descriptor
pointer	im			#I pointer to image header

bool	have_wcs
int	ndim, i, j
pointer	sp, sysname, iw, ct, wp, cp

int	mw_allocd(), mw_refstr()
pointer	iw_rfits(), iw_findcard()
errchk	iw_rfits, mw_allocd, mw_newsystem, mw_swtype, iw_enterwcs
string	s_physical "physical"

begin
	call smark (sp)
	call salloc (sysname, SZ_FNAME, TY_CHAR)

	# Read the FITS image header into an IMWCS descriptor.
	iw = iw_rfits (mw, im, RF_REFERENCE)
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
	MI_NLOGDIM(mw) = ndim
	MI_LTV(mw) = mw_allocd (mw, ndim)
	MI_LTM(mw) = mw_allocd (mw, ndim * ndim)

	# Set the Lterm.
	call amovd (IW_LTV(iw,1), D(mw,MI_LTV(mw)), ndim)
	if (iw_findcard (iw, TY_LTM, ERR, 0) != NULL) {
	    do j = 1, ndim
		do i = 1, ndim
		    D(mw,MI_LTM(mw)+(j-1)*ndim+(i-1)) = IW_LTM(iw,i,j)
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

	# Apply the section transform, if the image was opened with an image
	# section.

	call iw_setaxmap (mw, im)

	# Set the default world system.
	call mw_sdefwcs (mw)

	call iw_cfits (iw)
	call sfree (sp)
end
