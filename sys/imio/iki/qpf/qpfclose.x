# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imhdr.h>
include <imio.h>
include "qpf.h"

# QPF_CLOSE -- Close a QPOE image.

procedure qpf_close (im, status)

pointer	im			#I image descriptor
int	status			#O output status

pointer	qpf

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

	call mfree (qpf, TY_STRUCT)
end
