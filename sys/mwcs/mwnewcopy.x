# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"mwcs.h"

# MW_NEWCOPY -- Copy a MWCS.  The copy is done by constructing a new MWCS
# from the objects stored in the first one, freeing any dead storage in the
# process.

pointer procedure mw_newcopy (o_mw)

pointer	o_mw			#I pointer to old MWCS descriptor

int	ndim, nelem, i, j
pointer	mw, wp, o_wp, at, o_at

bool	streq()
int	mw_copys(), mw_copyd()
errchk	calloc, mw_copys, mw_copyd

begin
	# Make a copy of the main descriptor.
	call malloc (mw, LEN_MWCS, TY_STRUCT)
	call amovi (Memi[o_mw], Memi[mw], LEN_MWCS)

	# We have to allocate our own string and data buffers.
	MI_SBUF(mw) = NULL
	MI_SBUFLEN(mw) = 0
	MI_SBUFUSED(mw) = 0
	MI_DBUF(mw) = NULL
	MI_DBUFLEN(mw) = 0
	MI_DBUFUSED(mw) = 0

	# Copy the Lterm data.
	ndim = MI_NDIM(mw)
	nelem = ndim * ndim
	MI_LTV(mw) = mw_copyd (mw, o_mw, MI_LTV(o_mw), ndim)
	MI_LTM(mw) = mw_copyd (mw, o_mw, MI_LTM(o_mw), nelem)

	# We don't inherit open CTRAN descriptors.
	call aclri (MI_CTRAN(mw,1), MAX_CTRAN)

	# Copy the WCS.
	do i = 1, MI_NWCS(o_mw) {
	    wp = MI_WCSP(mw,i)
	    o_wp = MI_WCSP(o_mw,i)
	    ndim = WCS_NDIM(wp)
	    nelem = ndim * ndim

	    # Copy the WCS data.
	    WCS_R(wp) = mw_copyd (mw, o_mw, WCS_R(o_wp), ndim)
	    WCS_W(wp) = mw_copyd (mw, o_mw, WCS_W(o_wp), ndim)
	    WCS_CD(wp) = mw_copyd (mw, o_mw, WCS_CD(o_wp), nelem)

	    # Each axis can have its own sampled WCS.
	    do j = 1, ndim {
		WCS_PV(wp,j) =
		    mw_copyd (mw, o_mw, WCS_PV(o_wp,j), WCS_NPTS(o_wp,j))
		WCS_WV(wp,j) =
		    mw_copyd (mw, o_mw, WCS_WV(o_wp,j), WCS_NPTS(o_wp,j))
	    }

	    # Copy the WCS attributes.
	    do j = 1, WCS_NWATTR(o_wp) {
		at = WCS_WATTR(wp,j)
		o_at = WCS_WATTR(o_wp,j)
		AT_NAME(at) = mw_copys (mw, o_mw, AT_NAME(o_at))
		AT_VALUE(at) = mw_copys (mw, o_mw, AT_VALUE(o_at))
		if (streq (S(mw,AT_NAME(at)), "system"))
		    WCS_SYSTEM(wp) = AT_VALUE(at)
	    }

	    # Preserve the default WCS.
	    if (MI_WCS(o_mw) == o_wp)
		MI_WCS(mw) = wp
	}

	return (mw)
end


# MW_COPYD -- Copy a block of type double data from one MWCS to another.
# If the buffer offset in the old system is NULL, there was no data, and
# a null offset is output.

int procedure mw_copyd (mw, o_mw, o_off, nelem)

pointer	mw			#I pointer to output MWCS
pointer	o_mw			#I pointer to input (old) MWCS
int	o_off			#I buffer offset in old MWCS
int	nelem			#I number of type double data elements

int	off
int	mw_allocd()
errchk	mw_allocd

begin
	if (o_off == NULL)
	    off = NULL
	else {
	    off = mw_allocd (mw, nelem)
	    call amovd (D(o_mw,o_off), D(mw,off), nelem)
	}

	return (off)
end


# MW_COPYS -- Copy an EOS delimited string from one MWCS to another.
# If the buffer offset in the old system is NULL, there is no data, and
# a null offset is output.

int procedure mw_copys (mw, o_mw, o_off)

pointer	mw			#I pointer to output MWCS
pointer	o_mw			#I pointer to input (old) MWCS
int	o_off			#I buffer offset in old MWCS

int	off
int	mw_refstr()
errchk	mw_refstr

begin
	if (o_off == NULL)
	    off = NULL
	else
	    off = mw_refstr (mw, S(o_mw,o_off))

	return (off)
end
