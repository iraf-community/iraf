# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	"mwcs.h"

# MW_OPEN -- Create a new MWCS descriptor.  If the non-NULL address of a
# buffer containing a saved MWCS is given, the saved MWCS will be loaded,
# otherwise a unitary MWCS of the indicated dimension is created.

pointer procedure mw_open (bufptr, ndim)

pointer	bufptr		#I pointer to encoded MWCS, or NULL
int	ndim		#I dimension of system to be created

int	i
pointer	mw, wp
int	mw_allocd()
errchk	calloc, mw_load, syserrs, mw_allocd
string	s_physical "physical"

begin
	# Initialize the function drivers.
	call wf_init()

	# Allocate the base descriptor.
	call calloc (mw, LEN_MWCS, TY_STRUCT)

	# Load saved MWCS, if one was given.
	if (bufptr != NULL) {
	    call mw_load (mw, bufptr)
	    return (mw)
	}

	# Initialize the new descriptor to a unitary transform of dimension
	# NDIM.  Most of this is accomplished by merely creating a zeroed
	# descriptor.

	if (ndim < 1 || ndim > MAX_DIM) {
	    call mfree (mw, TY_STRUCT)
	    call syserrs (SYS_MWNDIM, "mw_open")
	}

	MI_MAGIC(mw) = MWCS_MAGIC
	MI_NDIM(mw) = ndim
	MI_NLOGDIM(mw) = ndim
	MI_LTV(mw) = mw_allocd (mw, ndim)
	MI_LTM(mw) = mw_allocd (mw, ndim * ndim)
	call mw_mkidmd (D(mw,MI_LTM(mw)), ndim)
	do i = 1, ndim {
	    MI_AXNO(mw,i) = i
	    MI_PHYSAX(mw,i) = i
	}

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

	# Set the default world system.
	call mw_sdefwcs (mw)

	return (mw)
end
