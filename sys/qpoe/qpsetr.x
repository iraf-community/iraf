# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<qpset.h>
include	"qpoe.h"

# QP_SETR -- Set an QPOE real valued parameter.

procedure qp_setr (qp, param, value)

pointer	qp			#I QPOE descriptor
int	param			#I parameter to be set
real	value			#I new value for parameter

begin
	switch (param) {
	case QPOE_BLOCKFACTOR:
	    QP_XBLOCK(qp) = value
	    QP_YBLOCK(qp) = value
	case QPOE_XBLOCKFACTOR:
	    QP_XBLOCK(qp) = value
	case QPOE_YBLOCKFACTOR:
	    QP_YBLOCK(qp) = value
	}
end
