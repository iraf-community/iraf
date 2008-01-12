#  IG_EVALUATE -- Modify one of the predefined plot vectors by
#  evaluating an expression using evexpr().  Each vector element is
#  evaluated using the same expression.  Another vector buffer may be
#  used in the expression as "X", "Y", "Z", "E", "P", "S", or "L".
#  The row number may be specified by "R" and the number of rows by "N".
#  There is some ambiguity in dealing with inherently 2-D Z data.

#  8/20/91  Removed ^Ls. ZGL
## 7/9/92  Added ZEVALUATE, ii_zevaluate().  ZGL

include <evexpr.h>
include "igi.h"
include "commands.h"

procedure ig_evaluate (cmd, igs)

int	cmd		# Command
pointer	igs		# igi parameters structure

pointer	sp, expr

begin
	call lcmdcat (igs, YES)

	call smark (sp)
	call salloc (expr, SZ_LINE, TY_CHAR)

	call igstarg (igs, Memc[expr], SZ_LINE)

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("Evaluate:  %s ")
		call pargstr (Memc[expr])
	}

	switch (cmd) {
	case XEVALUATE:
	    call ii_xevaluate (igs, Memc[expr])

	case YEVALUATE:
	    call ii_yevaluate (igs, Memc[expr])

	case ZEVALUATE:
	    call ii_zevaluate (igs, Memc[expr])

	case EEVALUATE:
	    call ii_eevaluate (igs, Memc[expr])

	case PEVALUATE:
	    call ii_pevaluate (igs, Memc[expr])

	case LEVALUATE:
	    call ii_levaluate (igs, Memc[expr])

	case SEVALUATE:
	    call ii_sevaluate (igs, Memc[expr])
	}

	call sfree (sp)

	call cmdcat (igs, NO)
end


procedure ii_xevaluate (igs, expr)

pointer	igs		# igi parameters structure
char	expr[ARB]

pointer	igps

begin
	igps = PLOT_PARMS(igs)
	if (MG_XDATAP(igps) == NULL)
	    MG_XNPTS(igps) = max (MG_YNPTS(igps),
				  max (MG_ENPTS(igps),
				       max (MG_PNPTS(igps), MG_LNPTS(igps))))
	call igseval (igps, expr, MG_XDATAP(igps), MG_XNPTS(igps))
end


procedure ii_yevaluate (igs, expr)

pointer	igs		# igi parameters structure
char	expr[ARB]

pointer	igps

begin
	igps = PLOT_PARMS(igs)
	if (MG_YDATAP(igps) == NULL)
	    MG_YNPTS(igps) = max (MG_XNPTS(igps),
				  max (MG_ENPTS(igps),
				       max (MG_PNPTS(igps), MG_LNPTS(igps))))
	call igseval (igps, expr, MG_YDATAP(igps), MG_YNPTS(igps))
end


procedure ii_zevaluate (igs, expr)

##  7/9/92  ZGL

pointer	igs		# igi parameters structure
char	expr[ARB]

pointer	igps

begin
	igps = PLOT_PARMS(igs)
	call igseval (igps, expr, MG_ZDATAP(igps), MG_ZNPTS(igps))
end


procedure ii_eevaluate (igs, expr)

pointer	igs		# igi parameters structure
char	expr[ARB]

pointer	igps

begin
	igps = PLOT_PARMS(igs)
	if (MG_EDATAP(igps) == NULL)
	    MG_ENPTS(igps) = max (MG_YNPTS(igps),
				  max (MG_XNPTS(igps),
				       max (MG_PNPTS(igps), MG_LNPTS(igps))))
	call igseval (igps, expr, MG_EDATAP(igps), MG_ENPTS(igps))
end


procedure ii_pevaluate (igs, expr)

pointer	igs		# igi parameters structure
char	expr[ARB]

pointer	igps

begin
	igps = PLOT_PARMS(igs)
	if (MG_PDATAP(igps) == NULL)
	    MG_PNPTS(igps) = max (MG_YNPTS(igps),
				  max (MG_ENPTS(igps),
				       max (MG_XNPTS(igps), MG_LNPTS(igps))))
	call igseval (igps, expr, MG_PDATAP(igps), MG_PNPTS(igps))
end


procedure ii_levaluate (igs, expr)

pointer	igs		# igi parameters structure
char	expr[ARB]

pointer	igps

begin
	igps = PLOT_PARMS(igs)
	if (MG_LDATAP(igps) == NULL)
	    MG_LNPTS(igps) = max (MG_YNPTS(igps),
				  max (MG_ENPTS(igps),
				       max (MG_PNPTS(igps), MG_XNPTS(igps))))
	call igseval (igps, expr, MG_LDATAP(igps), MG_LNPTS(igps))
end


procedure ii_sevaluate (igs, expr)

pointer	igs		# igi parameters structure
char	expr[ARB]

pointer	igps

begin
	igps = PLOT_PARMS(igs)
	call igseval (igps, expr, MG_SDATAP(igps), MG_SNPTS(igps))
end


procedure igseval (igps, expr, vector, npts)

pointer	igps
char	expr[ARB]
pointer	vector
int	npts

int	i
pointer	op

pointer	igp_struct
int	vec_index, vec_npts
common	/igsopm/ igp_struct, vec_index, vec_npts

extern	igget_op()
pointer	evexpr()
int	locpr(), errcode()

errchk	evexpr

begin
	igp_struct = igps

	if (vector == NULL) {
	    # Allocate the output vector
	    if (npts == 0)
		npts = DATA_SIZE
	    call malloc (vector, npts, TY_REAL)
	}

	vec_npts = npts

	do i = 1, npts {
	    # For each data vector element
	    vec_index = i

	    # Evaluate the expression
	    iferr (op = evexpr (expr, locpr (igget_op), 0)) {
		# Not a valid value
		if (errcode() == NULL_OPER)
		    # An operand was INDEF
		    Memr[vector+i-1] = INDEFR
		else
		    return

	    } else {
		# Assign the expression value to the vector element
		switch (O_TYPE(op)) {
		case TY_REAL:
		    Memr[vector+i-1] = LOP_VALR(op)
		case TY_INT:
		    Memr[vector+i-1] = real (LOP_VALI(op))
		}
	    }
	}

	call mfree (op, TY_STRUCT)

	MG_NPTS(igps) = npts
end


procedure igget_op (operand, op)

char	operand[ARB]		# operand name
pointer	op			# operand (output)

pointer	igp_struct
int	vec_index, vec_npts
real	toper
common	/igsopm/ igp_struct, vec_index, vec_npts

string	nulvalue	"INDEF value in array"

char	chrupr()

begin
	call xev_initop (op, 0, TY_REAL)

	switch (chrupr (operand[1])) {
	case 'E':
	    if (MG_EDATAP(igp_struct) == NULL)
		call error (NO_DATA, "No Error data defined ")
	    else if (vec_index > MG_ENPTS(igp_struct))
		call error (INSUF_DATA, "Not enough Error data ")
	    else {
		toper = Memr[MG_EDATAP(igp_struct)+vec_index-1]
		if (IS_INDEF(toper))
		    call error (NULL_OPER, nulvalue)
		LOP_VALR(op) = Memr[MG_EDATAP(igp_struct)+vec_index-1]
	    }

	case 'L':
	    if (MG_LDATAP(igp_struct) == NULL)
		call error (NO_DATA, "No Limits data defined ")
	    else if (vec_index > MG_LNPTS(igp_struct))
		call error (NO_DATA, "Not enough Limits data ")
	    else {
		toper = Memr[MG_LDATAP(igp_struct)+vec_index-1]
		if (IS_INDEF(toper))
		    call error (NULL_OPER, nulvalue)
		LOP_VALR(op) = Memr[MG_LDATAP(igp_struct)+vec_index-1]
	    }

	case 'N':
	    LOP_VALR(op) = real (vec_npts)

	case 'P':
	    if (MG_PDATAP(igp_struct) == NULL)
		call error (NO_DATA, "No Point data defined ")
	    else if (vec_index > MG_PNPTS(igp_struct))
		call error (INSUF_DATA, "Not enough Points data ")
	    else {
		toper = Memr[MG_PDATAP(igp_struct)+vec_index-1]
		if (IS_INDEF(toper))
		    call error (NULL_OPER, nulvalue)
		LOP_VALR(op) = Memr[MG_PDATAP(igp_struct)+vec_index-1]
	    }

	case 'R':
	    LOP_VALR(op) = real (vec_index)

	case 'S':
	    if (MG_SDATAP(igp_struct) == NULL)
		call error (NO_DATA, "No scratch data defined ")
	    else if (vec_index > MG_SNPTS(igp_struct))
		call error (NO_DATA, "Not enough scratch data ")
	    else {
		toper = Memr[MG_SDATAP(igp_struct)+vec_index-1]
		if (IS_INDEF(toper))
		    call error (NULL_OPER, nulvalue)
		LOP_VALR(op) = Memr[MG_SDATAP(igp_struct)+vec_index-1]
	    }

	case 'X':
	    if (MG_XDATAP(igp_struct) == NULL)
		call error (NO_DATA, "No X data defined ")
	    else if (vec_index > MG_XNPTS(igp_struct))
		call error (INSUF_DATA, "Not enough X data ")
	    else {
		toper = Memr[MG_XDATAP(igp_struct)+vec_index-1]
		if (IS_INDEF(toper))
		    call error (NULL_OPER, nulvalue)
		LOP_VALR(op) = Memr[MG_XDATAP(igp_struct)+vec_index-1]
	    }

	case 'Y':
	    if (MG_YDATAP(igp_struct) == NULL)
		call error (NO_DATA, "No Y data defined ")
	    else if (vec_index > MG_YNPTS(igp_struct))
		call error (INSUF_DATA, "Not enough Y data ")
	    else {
		toper = Memr[MG_YDATAP(igp_struct)+vec_index-1]
		if (IS_INDEF(toper))
		    call error (NULL_OPER, nulvalue)
		LOP_VALR(op) = Memr[MG_YDATAP(igp_struct)+vec_index-1]
	    }

	case 'Z':
	    if (MG_ZDATAP(igp_struct) == NULL)
		call error (NO_DATA, "No Z data defined ")

	    else if (vec_index > MG_ZNPTS(igp_struct))
		call error (INSUF_DATA, "Not enough Z data ")

	    else {
		toper = Memr[MG_ZDATAP(igp_struct)+vec_index-1]

		if (IS_INDEF(toper))
		    call error (NULL_OPER, nulvalue)

		LOP_VALR(op) = Memr[MG_ZDATAP(igp_struct)+vec_index-1]
	    }

	default:
	    call eprintf ("Syntax error in expression ")
	}
end
