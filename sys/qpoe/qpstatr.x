# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<qpset.h>
include	"qpoe.h"

# QP_STATR -- Get the value of an QPOE real parameter.

real procedure qp_statr (qp, param)

pointer	qp			#I QPOE descriptor
int	param			#I parameter to be queried

bool	fp_equalr()

begin
	switch (param) {
	case QPOE_BLOCKFACTOR:			# standard params
	    if (fp_equalr (QP_XBLOCK(qp), QP_YBLOCK(qp)))
		return (QP_XBLOCK(qp))
	    else
		return (ERR)
	case QPOE_XBLOCKFACTOR:
	    return (QP_XBLOCK(qp))
	case QPOE_YBLOCKFACTOR:
	    return (QP_YBLOCK(qp))
	}

	return (ERR)
end
