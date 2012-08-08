# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"qpex.h"

# QPEX_DELETE -- Delete any previously compiled expression terms for the
# event attribute with the given offset and datatype.  Only terms up to and
# including ET_LAST are affected (allowing deletion while compiling additional
# terms).

procedure qpex_delete (ex, et_last, offset, dtype)

pointer	ex			#I QPEX descriptor
pointer	et_last			#I last expression term to be edited
int	offset			#I typed offset of attribute in event struct
int	dtype			#I datatype of attribute

pointer	et, ip
int	ninstr, i

begin
	if (et_last == NULL)
	    return

	for (et=EX_ETHEAD(ex);  et != NULL;  et=ET_NEXT(et)) {
	    # Skip over already deleted terms or terms for other attributes.
	    if (ET_DELETED(et) == YES)
		next
	    else if (ET_ATTOFF(et) != offset || ET_ATTTYPE(et) != dtype)
		next

	    # Physically and logically delete the term.  Edit the program
	    # buffer and replace the compiled sequence of instructions by
	    # a GOTO followed by a series of NO-OPs.

	    ip = ET_PROGPTR(et)
	    ninstr = ET_NINSTR(et)

	    OPCODE(ip) = GOTO
	    IARG1(ip)  = ip + ninstr * LEN_INSTRUCTION
	    IARG2(ip)  = NULL
	    IARG3(ip)  = NULL

	    do i = 2, ninstr {
		ip = ET_PROGPTR(et) + (i-1) * LEN_INSTRUCTION
		OPCODE(ip) = NOP
		IARG1(ip)  = NULL
		IARG2(ip)  = NULL
		IARG3(ip)  = NULL
	    }

	    # Flag the eterm as deleted.
	    ET_DELETED(et) = YES

	    if (et == et_last)
		break
	}
end
