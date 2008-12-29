# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<mach.h>
include	"qpex.h"

.help qpexdata
.nf --------------------------------------------------------------------------
QPEXDATA -- Data management package for QPEX.  The QPEX data structures
consist of the QPEX descriptor and two main data buffers, the program buffer
(pb), containing the instructions to be executed (interpreted) to evaluate
an expression, and the data buffer (db), containing assorted data structures,
e.g., the linked list of expression term descriptors, the lookup table
descriptors, storage for DOUBLE data appearing in the compiled expression,
and so on.  The program and data buffers are dynamically allocated but are
not relocatable, so to absolute pointers may be used to reference the objects
therein (hence, runtime overflow is possible).

During expression compilation the following routines are used to add data
objects to the program and data buffers:

		      qpex_mark (ex, pb_save, db_save)
		      qpex_free (ex, pb_save, db_save)

	        ip = qpex_pbpos (ex)
		     qpex_[ipr]_pbpin_[01] (ex, opcode, arg1, arg2, arg3)

	      ptr = qpex_dbpstr (ex, strval)
	     ptrval = qpex_refd (ex, dval)
	     ptr = qpex_dballoc (ex, nelem, dtype)

QPEX_MARK and QPEX_FREE are used to mark the current tops of the two buffers
and subspequently free storage back to that point, e.g., for error recovery
following detection of a compilation error.  QPEX_PBPOS returns a pointer to
the location in the program buffer where next instruction will be placed.
QPEX_PBPIN compiles an instruction at that location.

The main storage allocator for the data buffer is QPEX_DBALLOC, which allocates
a properly aligned buffer of the indicated type in the data buffer, and returns
a pointer of the same type as the function value.  QPEX_DBPSTR stores a string
constant in the data buffer and returns a pointer to the stored string.
QPEX_REFD stores the given type double constant in the data buffer and returns
(as an integer) a pointer to the stored value (this is necessary to permit
only SZ_INT argument fields in instructions).
.endhelp ---------------------------------------------------------------------


# QPEX_MARK -- Mark the top of the program and data buffers.

procedure qpex_mark (ex, pb_save, db_save)

pointer	ex			#I QPEX descriptor
pointer	pb_save, db_save	#O saved pointers

begin
	pb_save = EX_PBOP(ex)
	db_save = EX_DBOP(ex)
end


# QPEX_FREE -- Free storage back to the marked points.

procedure qpex_free (ex, pb_save, db_save)

pointer	ex			#I QPEX descriptor
pointer	pb_save, db_save	#I saved pointers

pointer	top, prev, lt, et
pointer	coerce()

begin
	# Free space in program buffer.
	call aclrp (Memp[pb_save], EX_PBTOP(ex) - pb_save)
	EX_PBOP(ex) = pb_save

	# Free space in the data buffer.  Prune the LUT and ETERM lists
	# and then reset the data buffer pointer.

	# The LT list is backward linked from the most recent entry.
	top = coerce (db_save, TY_CHAR, TY_STRUCT)
	for (lt=EX_LTHEAD(ex);  lt != NULL;  lt=LT_NEXT(lt))
	    if (lt >= top) {
		call mfree (LT_LUTP(lt), TY_SHORT)
		EX_LTHEAD(ex) = LT_NEXT(lt)
	    }

	# The ET list is forward linked from the first entry.
	prev = NULL
	for (et=EX_ETHEAD(ex);  et != NULL;  et=ET_NEXT(et))
	    if (et >= top) {
		if (prev != NULL)
		    ET_NEXT(prev) = NULL
		EX_ETTAIL(ex) = prev
		break
	    }

	EX_DBOP(ex) = db_save
end


# QPEX_PBPOS -- Return a pointer to the program buffer location where the
# next instruction to be compiled will be located.

pointer procedure qpex_pbpos (ex)

pointer	ex			#I QPEX descriptor

begin
	return (EX_PBOP(ex))
end


# QPEX_I_PBPIN_[01] -- Add an insruction at the end of the program buffer.
#                      args: I-I-I

procedure qpex_i_pbpin_0 (ex, opcode, arg1, arg2, arg3)

pointer	ex			#I QPEX descriptor
int	opcode			#I instruction opcode
int	arg1,arg2,arg3		#I instruction data fields

pointer	op
errchk	syserr

begin
	op = EX_PBOP(ex)
	if (op >= EX_PBTOP(ex))
	    call syserr (SYS_QPEXPBOVFL)

	OPCODE(op) = opcode
	IARG1(op)  = arg1
	IARG2(op)  = arg2
	IARG3(op)  = arg3

	EX_PBOP(ex) = op + LEN_INSTRUCTION
end

procedure qpex_i_pbpin_1 (ex, opcode, arg1, arg2, arg3)

pointer	ex			#I QPEX descriptor
int	opcode			#I instruction opcode
int	arg1,arg2,arg3		#I instruction data fields

pointer	op
errchk	syserr

begin
	op = EX_PBOP(ex)
	if (op >= EX_PBTOP(ex))
	    call syserr (SYS_QPEXPBOVFL)

	OPCODE(op) = opcode
	IARG1(op)  = arg1
	IARG2(op)  = arg2
	IARG3(op)  = arg3

	EX_PBOP(ex) = op + LEN_INSTRUCTION
end


# QPEX_R_PBPIN_0 -- Add an insruction at the end of the program buffer.
#                   args: R-R-R

procedure qpex_r_pbpin_0 (ex, opcode, arg1, arg2, arg3)

pointer	ex			#I QPEX descriptor
int	opcode			#I instruction opcode
real	arg1,arg2,arg3		#I instruction data fields

pointer	op
errchk	syserr

begin
	op = EX_PBOP(ex)
	if (op >= EX_PBTOP(ex))
	    call syserr (SYS_QPEXPBOVFL)

	OPCODE(op) = opcode
	RARG1(op)  = arg1
	RARG2(op)  = arg2
	RARG3(op)  = arg3

	EX_PBOP(ex) = op + LEN_INSTRUCTION
end

# QPEX_R_PBPIN_1 -- Add an insruction at the end of the program buffer.
#                   args: I-R-R

procedure qpex_r_pbpin_1 (ex, opcode, arg1, arg2, arg3)

pointer	ex			#I QPEX descriptor
int	opcode			#I instruction opcode
int	arg1			#I instruction data fields
real	arg2,arg3		#I instruction data fields

pointer	op
errchk	syserr

begin
	op = EX_PBOP(ex)
	if (op >= EX_PBTOP(ex))
	    call syserr (SYS_QPEXPBOVFL)

	OPCODE(op) = opcode
	IARG1(op)  = arg1
	RARG2(op)  = arg2
	RARG3(op)  = arg3

	EX_PBOP(ex) = op + LEN_INSTRUCTION
end


# QPEX_P_PBPIN_0 -- Add an insruction at the end of the program buffer.
#                   args: P-P-P

procedure qpex_p_pbpin_0 (ex, opcode, arg1, arg2, arg3)

pointer	ex			#I QPEX descriptor
int	opcode			#I instruction opcode
pointer	arg1,arg2,arg3		#I instruction data fields

pointer	op
errchk	syserr

begin
	op = EX_PBOP(ex)
	if (op >= EX_PBTOP(ex))
	    call syserr (SYS_QPEXPBOVFL)

	OPCODE(op) = opcode
	PARG1(op)  = arg1
	PARG2(op)  = arg2
	PARG3(op)  = arg3

	EX_PBOP(ex) = op + LEN_INSTRUCTION
end

# QPEX_P_PBPIN_1 -- Add an insruction at the end of the program buffer.
#                   args: I-P-P

procedure qpex_p_pbpin_1 (ex, opcode, arg1, arg2, arg3)

pointer	ex			#I QPEX descriptor
int	opcode			#I instruction opcode
int	arg1			#I instruction data fields
pointer	arg2,arg3		#I instruction data fields

pointer	op
errchk	syserr

begin
	op = EX_PBOP(ex)
	if (op >= EX_PBTOP(ex))
	    call syserr (SYS_QPEXPBOVFL)

	OPCODE(op) = opcode
	IARG1(op)  = arg1
	PARG2(op)  = arg2
	PARG3(op)  = arg3

	EX_PBOP(ex) = op + LEN_INSTRUCTION
end


# QPEX_DBPSTR -- Store a string constant in the data buffer, returning a
# pointer to the stored string as the function value.

pointer procedure qpex_dbpstr (ex, strval)

pointer	ex			#I QPEX descriptor
char	strval[ARB]		#I string to be stored

pointer	op
int	nchars
int	strlen()
errchk	syserr

begin
	op = EX_DBOP(ex)
	nchars = strlen (strval) + 1

	if (op + nchars >= EX_DBTOP(ex))
	    call syserr (SYS_QPEXDBOVFL)

	call strcpy (strval, Memc[op], nchars)
	EX_DBOP(ex) = op + nchars

	return (op)
end


# QPEX_REFD -- Reference a type DOUBLE datum, returning (as an integer) a
# pointer to the double value, which is stored in the data buffer.

pointer procedure qpex_refd (ex, value)

pointer	ex			#I QPEX descriptor
double	value			#I double value

pointer	dp
pointer	qpex_dballoc()
errchk	qpex_dballoc

begin
	dp = qpex_dballoc (ex, 1, TY_DOUBLE)
	Memd[dp] = value
	return (dp)
end


# QPEX_DBALLOC -- Allocate storage of the indicated type in the data
# buffer, returning a typed pointer to the buffer.  The buffer is fully
# aligned.

pointer procedure qpex_dballoc (ex, nelem, dtype)

pointer	ex			#I QPEX descriptor
int	nelem			#I amount of storage desired
int	dtype			#I datatype of the storage element

pointer	op, top
long	lp, lval
pointer	coerce()
int	sizeof()
long	lmod()
errchk	syserr

begin
	lp = EX_DBOP(ex)
	lval = SZ_DOUBLE
	while (lmod (lp-1, lval) != 0)
	    lp = lp + 1
	op = lp

	top = op + nelem * sizeof(dtype)
	if (top >= EX_DBTOP(ex))
	    call syserr (SYS_QPEXDBOVFL)

	EX_DBOP(ex) = top
	return (coerce (op, TY_CHAR, dtype))
end
