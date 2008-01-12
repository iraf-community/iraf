# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"qpoe.h"

# QP_CLOSE -- Close an open QPOE descriptor and file.

procedure qp_close (qp)

pointer	qp			#I QPOE descriptor

begin
	# An open/close should produce an empty poefile.
	if (QP_ACTIVE(qp) == NO) {
	    QP_MODIFIED(qp) = YES
	    call qp_bind (qp)
	}

	# Update the poefile on disk.
	call qp_flushpar (qp)
	call qp_sync (qp)

	# Shut everything down.
	call stclose (QP_ST(qp))
	call fm_close (QP_FM(qp))
	call mfree (qp, TY_STRUCT)
end
