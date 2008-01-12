include "igi.h"

procedure spshfd (in, ifds)

int	in		# Input command source descriptor
pointer	ifds		# Input file stack descriptor 

begin
	STK_INDEX(ifds) = STK_INDEX(ifds) + 1

	if (STK_INDEX(ifds) > STK_DEPTH(ifds)) {
	    # Ran out of room on the stack
	    STK_DEPTH(ifds) = STK_DEPTH(ifds) + STK_DEF_SIZE
	    call realloc (STK_STACK(ifds), STK_DEPTH(ifds), TY_INT)
	}

	# Push the new input file descriptor on the stack
	STK_VALUE(ifds) = in
end


int procedure spopfd (ifds)

pointer	ifds		# Input file stack descriptor 

int	in		# Input command source descriptor

begin
	in = STK_VALUE(ifds)
	if (STK_INDEX(ifds) > 0)
	    STK_INDEX(ifds) = STK_INDEX(ifds) - 1
	return (in)
end
