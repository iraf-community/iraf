# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<mach.h>
include	"qpoe.h"
include	"qpex.h"

# QPEX_OPEN -- Open the expression evaluator.  If an expression is given it
# is compiled into the descriptor, otherwise a null (pass all) expression
# is compiled.  The compiled expression may be modified or read out at any
# time via calls to other routines in the QPEX package (e.g., qpex_modfilter,
# qpex_getfilter).

pointer procedure qpex_open (qp, expr)

pointer	qp			#I QPOE descriptor
char	expr[ARB]		#I selection expression (filter) 

pointer	ex, pb, db
int	pb_len, db_len
int	qpex_modfilter()
errchk	calloc

begin
	# Allocate the main QPEX descriptor.
	call calloc (ex, LEN_EXDES, TY_STRUCT)

	# Allocate the program buffer.
	pb_len = QP_EXPBLEN(qp)
	call calloc (pb, pb_len, TY_INT)

	# Allocate the data buffer.
	db_len = QP_EXDBLEN(qp)
	call calloc (db, db_len, TY_CHAR)

	# Initialize the descriptor.

	EX_QP(ex)	= qp
	EX_DEBUG(ex)	= QP_DEBUG(qp)
	EX_START(ex)	= pb

	EX_PB(ex)	= pb
	EX_PBTOP(ex)	= pb + pb_len
	EX_PBOP(ex)	= pb

	EX_DB(ex)	= db
	EX_DBTOP(ex)	= db + db_len
	EX_DBOP(ex)	= db

	EX_MAXFRLUTLEN(ex)  = QP_EXMAXFRLLEN(qp)
	EX_MAXRRLUTLEN(ex)  = QP_EXMAXRRLLEN(qp)
	EX_LUTMINRANGES(ex) = QP_EXLMINRANGES(qp)
	EX_LUTSCALE(ex)     = QP_EXLSCALE(qp)

	if (EX_DEBUG(ex) > 1) {
	    call eprintf ("QPEX activated, expr = `%s'\n")
		call pargstr (expr)
	}

	# If a selection expression was given, compile it into the descriptor.
	if (qpex_modfilter (ex, expr) == ERR) {
	    call qpex_close (ex)
	    call syserrs (SYS_QPEXSYN, QP_DFNAME(qp))
	}

	return (ex)
end
