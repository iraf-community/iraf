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
		     qpex_pbpin (ex, opcode, arg1, arg2, arg3)

	      ptr = qpex_dbpstr (ex, strval)
	     intval = qpex_refd (ex, dval)
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
	call aclri (Memi[pb_save], EX_PBTOP(ex) - pb_save)
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


# QPEX_PBPIN -- Add an insruction at the end of the program buffer.

procedure qpex_pbpin (ex, opcode, arg1, arg2, arg3)

pointer	ex			#I QPEX descriptor
int	opcode			#I instruction opcode
int	arg1,arg2,arg3		#I instruction data fields (typeless)

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

int procedure qpex_refd (ex, value)

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
pointer	coerce()
int	sizeof()
errchk	syserr

begin
	op = EX_DBOP(ex)
	while (mod (op-1, SZ_DOUBLE) != 0)
	    op = op + 1

	top = op + nelem * sizeof(dtype)
	if (top >= EX_DBTOP(ex))
	    call syserr (SYS_QPEXDBOVFL)

	EX_DBOP(ex) = top
	return (coerce (op, TY_CHAR, dtype))
end
