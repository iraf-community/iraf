# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<qpset.h>
include	"qpoe.h"

# QP_SET[IL] -- Set an QPOE integer parameter.

procedure qp_setl (qp, param, lvalue)

pointer	qp			#I QPOE descriptor
int	param			#I parameter to be set
long	lvalue			#I new value for parameter

begin
	switch (param) {
	case QPOE_BLOCKFACTOR:
	    QP_XBLOCK(qp) = lvalue
	    QP_YBLOCK(qp) = lvalue
	case QPOE_XBLOCKFACTOR:
	    QP_XBLOCK(qp) = lvalue
	case QPOE_YBLOCKFACTOR:
	    QP_YBLOCK(qp) = lvalue
	case QPOE_BUCKETLEN:
	    QP_BUCKETLEN(qp) = lvalue
	case QPOE_CACHESIZE:
	    QP_FMCACHESIZE(qp) = lvalue
	case QPOE_DATABUFLEN:
	    QP_EXDBLEN(qp) = lvalue
	case QPOE_DEBUGLEVEL:
	    QP_DEBUG(qp) = lvalue
	case QPOE_INDEXLEN:
	    QP_STINDEXLEN(qp) = lvalue
	case QPOE_LUTMINRANGES:
	    QP_EXLMINRANGES(qp) = lvalue
	case QPOE_LUTSCALE:
	    QP_EXLSCALE(qp) = lvalue
	case QPOE_MAXFRLUTLEN:
	    QP_EXMAXFRLLEN(qp) = lvalue
	case QPOE_MAXLFILES:
	    QP_FMMAXLFILES(qp) = lvalue
	case QPOE_MAXPTPAGES:
	    QP_FMMAXPTPAGES(qp) = lvalue	# size_t
	case QPOE_MAXRRLUTLEN:
	    QP_EXMAXRRLLEN(qp) = lvalue
	case QPOE_MAXPUSHBACK:
	    QP_SZPBBUF(qp) = lvalue
	case QPOE_NODEFFILT:
	    QP_NODEFFILT(qp) = lvalue
	case QPOE_NODEFMASK:
	    QP_NODEFMASK(qp) = lvalue
	case QPOE_OPTBUFSIZE:
	    QP_OPTBUFSIZE(qp) = lvalue		# size_t
	case QPOE_PAGESIZE:
	    QP_FMPAGESIZE(qp) = lvalue		# long
	case QPOE_PROGBUFLEN:
	    QP_EXPBLEN(qp) = lvalue
	case QPOE_SBUFSIZE:
	    QP_STSBUFSIZE(qp) = lvalue
	case QPOE_STABLEN:
	    QP_STSTABLEN(qp) = lvalue
	}
end


procedure qp_seti (qp, param, value)

pointer	qp			#I QPOE descriptor
int	param			#I parameter to be set
int	value			#I new value for parameter

long	lvalue

begin
	lvalue = value
	call qp_setl(qp, param, lvalue)
end
