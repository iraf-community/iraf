# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	<knet.h>
include	"fmio.h"

# FM_CLOSE -- Close a datafile opened under FMIO.

procedure fm_close (fm)

pointer	fm			#I FMIO descriptor

size_t	sz_val
pointer	lf
int	status, i, j, i_max
errchk	fmio_bind, fm_fcfree, fmio_errchk

include "fmio.com"

begin
	# An open-new-file followed by a close should create an empty datafile.
	if (FM_ACTIVE(fm) == NO)
	    call fmio_bind (fm)

	# Shut down the file cache, if in use (does a sync).
	if (FM_FCACHE(fm) != NULL)
	    call fm_fcfree (fm)
	else
	    call fm_sync (fm)

	# Report any posted errors.
	call fmio_errchk (fm)

	# Close the physical datafile.
	call zclsbf (FM_CHAN(fm), status)
	if (status == ERR)
	    iferr (call syserrs (SYS_FMCLOSE, FM_DFNAME(fm)))
		call erract (EA_WARN)

	# Free any storage used by the runtime file table.
	lf = FM_FTABLE(fm)
	do i = 0, FM_NLFILES(fm) {
	    if (LF_PAGEMAP(lf) != NULL)
		call mfree (LF_PAGEMAP(lf), TY_LONG)
	    #
	    do j = 0, num_lf-1 {
		if ( Memp[lf_ptrs0+j] == lf ) Memp[lf_ptrs0+j] = NULL
	    }
	    #
	    lf = lf + LEN_FTE
	}
	# Setup lf address table
	i_max = -1
	do i = 0, num_lf-1 {
	    if ( Memp[lf_ptrs0+i] != NULL ) i_max = i
	}
	sz_val = i_max + 1
	if ( sz_val == 0 ) {
	    call mfree (lf_ptrs0, TY_POINTER)
	    lf_ptrs0 = NULL
	} else {
	    call realloc (lf_ptrs0, sz_val, TY_POINTER)
	}
	lf_ptrs = lf_ptrs0 - 1
	num_lf = sz_val
	#

	# Free the main descriptor.
	call mfree (FM_PTABLE(fm), TY_SHORT)
	call mfree (FM_PTINDEX(fm), TY_LONG)
	call mfree (FM_FTABLE(fm), TY_LONG)
	call mfree (fm, TY_STRUCT)
end
