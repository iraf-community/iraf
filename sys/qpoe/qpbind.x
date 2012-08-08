# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fmset.h>
include	"qpoe.h"

# QP_BIND -- Fix the create-time QPOE file parameters.  This is called
# after the open, when the first datafile access occurs.

procedure qp_bind (qp)

pointer	qp			#I QPOE descriptor

int	fd
pointer	fm
pointer	stopen()
int	fm_fopen()
errchk	stopen, fm_fopen

begin
	if (QP_ACTIVE(qp) == NO) {
	    fm = QP_FM(qp)

	    # Create the initial symbol table. 
	    QP_ST(qp) = stopen (QPOE_TITLE,
		QP_STINDEXLEN(qp), QP_STSTABLEN(qp), QP_STSBUFSIZE(qp))

	    # Fix the datafile parameters.
	    call fm_seti (fm, FM_PAGESIZE, QP_FMPAGESIZE(qp))
	    call fm_seti (fm, FM_MAXLFILES, QP_FMMAXLFILES(qp))
	    call fm_seti (fm, FM_MAXPTPAGES, QP_FMMAXPTPAGES(qp))
	    call fm_seti (fm, FM_FCACHESIZE, QP_FMCACHESIZE(qp))

	    # Create the QPOE header and static storage lfiles.
	    fd = fm_fopen (fm, LF_QPOE, NEW_FILE, BINARY_FILE)
	    call close (fd)
	    fd = fm_fopen (fm, LF_STATICPARS, NEW_FILE, BINARY_FILE)
	    call close (fd)

	    # Must flag descriptor as active here to prevent reentrant
	    # calls via the procedures called by qp_inherit.

	    QP_ACTIVE(qp) = YES

	    # Inherit selected data objects from parent if NEW_COPY.
	    if (QP_MODE(qp) == NEW_COPY)
		call qp_inherit (qp, QP_OQP(qp), STDERR)
	}
end
