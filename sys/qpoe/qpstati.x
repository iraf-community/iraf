# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<qpset.h>
include	"qpoe.h"

# QP_STATI -- Get the value of an QPOE integer parameter.

int procedure qp_stati (qp, param)

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
	case QPOE_BUCKETLEN:
	    return (QP_BUCKETLEN(qp))
	case QPOE_CACHESIZE:
	    return (QP_FMCACHESIZE(qp))
	case QPOE_DATABUFLEN:
	    return (QP_EXDBLEN(qp))
	case QPOE_DEBUGLEVEL:
	    return (QP_DEBUG(qp))
	case QPOE_INDEXLEN:
	    return (QP_STINDEXLEN(qp))
	case QPOE_MAXLFILES:
	    return (QP_FMMAXLFILES(qp))
	case QPOE_MAXPTPAGES:
	    return (QP_FMMAXPTPAGES(qp))
	case QPOE_MAXFRLUTLEN:
	    return (QP_EXMAXFRLLEN(qp))
	case QPOE_MAXRRLUTLEN:
	    return (QP_EXMAXRRLLEN(qp))
	case QPOE_LUTMINRANGES:
	    return (QP_EXLMINRANGES(qp))
	case QPOE_LUTSCALE:
	    return (QP_EXLSCALE(qp))
	case QPOE_MAXPUSHBACK:
	    return (QP_SZPBBUF(qp))
	case QPOE_NODEFFILT:
	    return (QP_NODEFFILT(qp))
	case QPOE_NODEFMASK:
	    return (QP_NODEFMASK(qp))
	case QPOE_OPTBUFSIZE:
	    return (QP_OPTBUFSIZE(qp))
	case QPOE_PAGESIZE:
	    return (QP_FMPAGESIZE(qp))
	case QPOE_PROGBUFLEN:
	    return (QP_EXPBLEN(qp))
	case QPOE_SBUFSIZE:
	    return (QP_STSBUFSIZE(qp))
	case QPOE_STABLEN:
	    return (QP_STSTABLEN(qp))

	case QPOE_FM:				# read-only params
	    return (QP_FM(qp))
	case QPOE_MODE:
	    return (QP_MODE(qp))
	case QPOE_ST:
	    return (QP_ST(qp))
	case QPOE_VERSION:
	    return (QP_VERSION(qp))
	}

	return (ERR)
end
