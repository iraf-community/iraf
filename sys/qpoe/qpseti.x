# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<qpset.h>
include	"qpoe.h"

# QP_SETI -- Set an QPOE integer parameter.

procedure qp_seti (qp, param, value)

pointer	qp			#I QPOE descriptor
int	param			#I parameter to be set
int	value			#I new value for parameter

begin
	switch (param) {
	case QPOE_BLOCKFACTOR:
	    QP_XBLOCK(qp) = value
	    QP_YBLOCK(qp) = value
	case QPOE_XBLOCKFACTOR:
	    QP_XBLOCK(qp) = value
	case QPOE_YBLOCKFACTOR:
	    QP_YBLOCK(qp) = value
	case QPOE_BUCKETLEN:
	    QP_BUCKETLEN(qp) = value
	case QPOE_CACHESIZE:
	    QP_FMCACHESIZE(qp) = value
	case QPOE_DATABUFLEN:
	    QP_EXDBLEN(qp) = value
	case QPOE_DEBUGLEVEL:
	    QP_DEBUG(qp) = value
	case QPOE_INDEXLEN:
	    QP_STINDEXLEN(qp) = value
	case QPOE_LUTMINRANGES:
	    QP_EXLMINRANGES(qp) = value
	case QPOE_LUTSCALE:
	    QP_EXLSCALE(qp) = value
	case QPOE_MAXFRLUTLEN:
	    QP_EXMAXFRLLEN(qp) = value
	case QPOE_MAXLFILES:
	    QP_FMMAXLFILES(qp) = value
	case QPOE_MAXPTPAGES:
	    QP_FMMAXPTPAGES(qp) = value
	case QPOE_MAXRRLUTLEN:
	    QP_EXMAXRRLLEN(qp) = value
	case QPOE_MAXPUSHBACK:
	    QP_SZPBBUF(qp) = value
	case QPOE_NODEFFILT:
	    QP_NODEFFILT(qp) = value
	case QPOE_NODEFMASK:
	    QP_NODEFMASK(qp) = value
	case QPOE_OPTBUFSIZE:
	    QP_OPTBUFSIZE(qp) = value
	case QPOE_PAGESIZE:
	    QP_FMPAGESIZE(qp) = value
	case QPOE_PROGBUFLEN:
	    QP_EXPBLEN(qp) = value
	case QPOE_SBUFSIZE:
	    QP_STSBUFSIZE(qp) = value
	case QPOE_STABLEN:
	    QP_STSTABLEN(qp) = value
	}
end
