# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"qpoe.h"

# QP_SYNC -- Update the poefile on disk.

procedure qp_sync (qp)

pointer	qp			#I QPOE descriptor

int	n, fd
pointer	sp, qph
int	fm_fopen()
errchk	qp_flushpar, fm_fopen, write, stsave

begin
	# Flush the put-parameter buffer.
	call qp_flushpar (qp)

	# Update the QPOE descriptor and symbol table in the datafile.
	if (QP_MODIFIED(qp) != NO) {
	    call smark (sp)
	    call salloc (qph, LEN_QPH, TY_STRUCT)
	    call aclri (Memi[qph], LEN_QPH)

	    QPH_MAGIC(qph)	= QP_MAGIC(qp)
	    QPH_VERSION(qph)	= QPOE_VERSION
	    QPH_STOFFSET(qph)	= LEN_QPH * SZ_STRUCT + 1

	    # The encoded QPOE header and symbol table are stored in a
	    # binary lfile in the datafile.

	    fd = fm_fopen (QP_FM(qp), LF_QPOE, NEW_FILE, BINARY_FILE)

	    # Update the QPOE file header.
	    n = LEN_QPH * SZ_STRUCT
	    call miipak32 (Memi[qph], Memi[qph], LEN_QPH, TY_STRUCT)
	    call write (fd, Memi[qph], n)

	    # Update the symbol table.
	    call stsqueeze (QP_ST(qp))
	    call stsave (QP_ST(qp), fd)

	    QP_MODIFIED(qp) = NO
	    call close (fd)
	    call sfree (sp)
	}

	# Update the datafile itself.
	call fm_fcsync (QP_FM(qp))
end
