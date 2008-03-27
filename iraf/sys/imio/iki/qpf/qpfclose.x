# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imhdr.h>
include <imio.h>
include "qpf.h"

# QPF_CLOSE -- Close a QPOE image.

procedure qpf_close (im, status)

pointer	im			#I image descriptor
int	status			#O output status

size_t	sz_val
pointer	qpf
int	i, i_max

include "qpf.com"

begin
	# Close the QPF virtual file driver.
	if (IM_PFD(im) != NULL)
	    call close (IM_PFD(im))

	# Close the various descriptors.
	qpf = IM_KDES(im)
	if (QPF_IO(qpf) != NULL)
	    call qpio_close (QPF_IO(qpf))
	if (QPF_QP(qpf) != NULL)
	    call qp_close (QPF_QP(qpf))

	#
	do i = 0, num_qpf-1 {
	    if ( Memp[qpf_ptrs0+i] == qpf ) Memp[qpf_ptrs0+i] = NULL
	}
	# Setup qpf address table
	i_max = -1
	do i = 0, num_qpf-1 {
	    if ( Memp[qpf_ptrs0+i] != NULL ) i_max = i
	}
	sz_val = i_max + 1
	if ( sz_val == 0 ) {
	    call mfree (qpf_ptrs0, TY_POINTER)
	    qpf_ptrs0 = NULL
	} else {
	    call realloc (qpf_ptrs0, sz_val, TY_POINTER)
	}
	qpf_ptrs = qpf_ptrs0 - 1
	num_qpf = sz_val
	#

	call mfree (qpf, TY_STRUCT)
end
