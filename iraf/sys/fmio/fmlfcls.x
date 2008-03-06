# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"fmio.h"

# FM_LFCLOSE -- Close an lfile descriptor.  There isn't much for us to do,
# since the physical datafile remains open, and opening an lfile does not
# allocate a descriptor.

procedure fm_lfclose (lf_chan, status)

int	lf_chan			#I lfile descriptor
int	status			#O i/o status (nbytes transferred or ERR)

pointer	lf, fm
size_t	sz_val
int	i, i_max

include "fmio.com"

begin
	lf = Memp[lf_ptrs+lf_chan]

	fm = LF_FM(lf)

	if (fm == NULL)
	    status = ERR
	else if (FM_MAGIC(fm) != FMIO_MAGIC)
	    status = ERR
	else
	    status = OK

	# Setup lf address table
	Memp[lf_ptrs+lf_chan] = NULL
	i_max = -1
	do i = 0, num_lf-1 {
	    if ( Memp[lf_ptrs+i] != NULL ) i_max = i
	}
	sz_val = i_max + 1
	if ( sz_val == 0 ) {
	    call mfree (lf_ptrs, TY_POINTER)
	    lf_ptrs = NULL
	} else {
	    call realloc (lf_ptrs, sz_val, TY_POINTER)
	}
	num_lf = sz_val
	#

	call fmio_tick (fm)
end
