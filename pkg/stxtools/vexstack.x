include "vex.h"

# VEX_STACK -- Procedures which manipulate the vex stack
#
# The expression evaluator, vex_eval, uses a stack to hold intermediate
# results, constants, and variable names in the expression. There are
# actually two stacks, a type stack wich contains the data types of the
# elements on the stack, and a value stack, which contains pointers to
# the stack elements. Constants and variable names are stored in two
# buffers which are part of the stack structure and pointers to their
# locations are placed on the stack. Intermediate results are stored in 
# malloc'ed arrays and their pointers are also placed on the stack. The
# stack structure contains three indices, bottom, an index one greater
# than the last constant or variable name, top, an index that is one 
# greater than the current top of stack, and high, an index that is one 
# greater than the last valid pointer on the stack. Valid pointers exist
# beyond the top of stack because the arrays which store intermediate 
# results are not mfree'd when the stack is popped, instead, they are 
# kept in case they may be needed for a future intermediate result. The 
# only user callable procedure in this file is stk_alloc, which should 
# be called by getvar, the user's function which fills an array when
# passed the name of a variable.
#
# B.Simon	24-Apr-91	Original
# B.Simon	15-Oct-98	Store strings in pseudocode, not on stack

# STK_ALLOC -- Allocate an array of the specified length and type

pointer procedure stk_alloc (stack, len, type)

pointer	stack		# i: Stack structure
int	len		# i: Length of array to allocate
int	type		# i: Data type of array (spp type)
#--
int	index, stype, top
pointer	var, svar

string	badstack  "stk_alloc: illegal type on stack"
string	badsize   "Requested array size does not match previous requests"

errchk	stk_find

begin
	# Check to see if array length is being defined for the first time

	if (STK_LENVAL(stack) == 0 && len != 0) {

	    # Store length in stack structure

	    STK_LENVAL(stack) = len

	    # Free all stack arrays not currently being used

	    index = STK_TOP(stack)
	    while (index < STK_HIGH(stack)) {
		svar = STK_VALUE(stack,index)
		stype = STK_TYPE(stack,index)
		call mfree (svar, stype)
		index = index + 1
	    }
	    STK_HIGH(stack) = STK_TOP(stack)

	    # Reallocate the null buffer

	    if (STK_NULLARY(stack) != NULL) {
		call stk_freenull (stack)
		call stk_initnull (stack, true)
	    }

	    # Convert length one arrays to their full length

	    index = 0
	    while (index < STK_TOP(stack)) {
	        svar = STK_VALUE(stack,index)
		stype = STK_TYPE(stack,index)

		call malloc (var, len, stype)
		STK_VALUE(stack,index) = var
		switch (stype) {
		case TY_INT,TY_LONG:
		    call amovki (Memi[svar], Memi[var], len)
		    call mfree (svar, TY_INT)
		case TY_REAL:
		    call amovkr (Memr[svar], Memr[var], len)
		    call mfree (svar, TY_REAL)
		case TY_DOUBLE:
		    call amovkd (Memd[svar], Memd[var], len)
		    call mfree (svar, TY_DOUBLE)
		default:
		    call error (1, badstack)
		}		
		index = index + 1
	    }
	}

	# Check requested size

	if (len != 0 && len != STK_LENVAL(stack))
	    call error (1, badsize)

	# Look for an existing array of the same type

	call stk_find (stack, type, index)

	# Increment top of stack pointer

	top = STK_TOP(stack)
	STK_TOP(stack) = top + 1

	# Swap array with one currently at top of stack

	if (top != index) {
	    stype = STK_TYPE(stack,top)
	    STK_TYPE(stack,top) = STK_TYPE(stack,index)
	    STK_TYPE(stack,index) = stype

	    svar = STK_VALUE(stack,top)
	    STK_VALUE(stack,top) = STK_VALUE(stack,index)
	    STK_VALUE(stack,index) = svar
	}

	var = STK_VALUE(stack,top)
	return (var)
end

# STK_CLEAR -- Clear all stack elements above the bottom

procedure stk_clear (stack)

pointer	stack		# u: Stack pointer
#--
int	index

begin
	# Free all value arrays above the bottom of stack

	index = 0
	while (index < STK_HIGH(stack)) {
	    call mfree (STK_VALUE(stack,index), STK_TYPE(stack,index))
	    index = index + 1
	}

	# Free null array

	call stk_freenull (stack)

	# Reset scalars

	STK_TOP(stack) = 0
	STK_HIGH(stack) = 0
	STK_LENVAL(stack) = 0
end

# STK_COERCE -- Coerce an array in the stack to the specified type

procedure stk_coerce (stack, pos, type, var)

pointer	stack		# i: Stack descriptor
int	pos		# i: Position of array in stack
int	type		# i: New type for array
pointer	var		# o: New pointer to array
#--
int	index, last, stype, len, i
pointer	svar

string	underflow  "stk_coerce: underflow in expression evaluator"

errchk	stk_find

begin
	# Convert relative to absolute position

	if (pos == TOP) {
	    index = STK_TOP(stack) - 1
	    if (index < 0)
		call error (1, underflow)
	} else {
	    index = pos
	}

	# If type of array matches requested type, return pointer to array
	# Otherwise, get new array and copy old array to it

	if (type == STK_TYPE(stack,index)) {
	    var = STK_VALUE(stack,index)

	} else {
	    # Find array of correct type

	    last = index
	    call stk_find (stack, type, index)

	    # Copy array, converting to new type

	    len = max (1, STK_LENVAL(stack))
	    var = STK_VALUE(stack,index)

	    stype = STK_TYPE(stack,last)
	    svar = STK_VALUE(stack,last)

	    switch (type) {
	    case TY_INT,TY_LONG:
		switch (stype) {
		case TY_INT,TY_LONG:
		    ; # can't happen
		case TY_REAL:
		    do i = 0, len-1 {
			if (IS_INDEFR(Memr[svar+i])) {
			    Memi[var+i] = INDEFI
			} else {
			    Memi[var+i] = Memr[svar+i]
			}
		    }
		case TY_DOUBLE:
		    do i = 0, len-1 {
			if (IS_INDEFD(Memd[svar+i])) {
			    Memi[var+i] = INDEFI
			} else {
			    Memi[var+i] = Memd[svar+i]
			}
		    }
		}
	    case TY_REAL:
		switch (stype) {
		case TY_INT,TY_LONG:
		    do i = 0, len-1 {
			if (IS_INDEFI(Memi[svar+i])) {
			    Memr[var+i] = INDEFR
			} else {
			    Memr[var+i] = Memi[svar+i]
			}
		    }
		case TY_REAL:
		    ; # can't happen
		case TY_DOUBLE:
		    do i = 0, len-1 {
			if (IS_INDEFD(Memd[svar+i])) {
			    Memr[var+i] = INDEFR
			} else {
			    Memr[var+i] = Memd[svar+i]
			}
		    }
		}
	    case TY_DOUBLE:
		switch (stype) {
		case TY_INT,TY_LONG:
		    do i = 0, len-1 {
			if (IS_INDEFI(Memi[svar+i])) {
			    Memd[var+i] = INDEFD
			} else {
			    Memd[var+i] = Memi[svar+i]
			}
		    }
		case TY_REAL:
		    do i = 0, len-1 {
			if (IS_INDEFR(Memr[svar+i])) {
			    Memd[var+i] = INDEFD
			} else {
			    Memd[var+i] = Memr[svar+i]
			}
		    }
		case TY_DOUBLE:
		    ; # can't happen
		}
	    }

	    # Swap position of new and old arrays on stack

	    STK_TYPE(stack,last) = STK_TYPE(stack,index)
	    STK_TYPE(stack,index) = stype

	    STK_VALUE(stack,last) = STK_VALUE(stack,index)
	    STK_VALUE(stack,index) = svar
	}
	
end

# STK_FETCH -- Fetch the specified number of arrays from the stack

procedure stk_fetch (stack, nvar, var, len, type)

pointer	stack		# i: Stack descriptor
int	nvar		# i: Number of pointers requested
pointer	var[ARB]	# o: Array pointers
int	len		# o: Length of arrays
int	type		# o: Type of arrays
#--
int	one, two, index, ivar

string	underflow  "stk_fetch: underflow in expression evaluator"

errchk	stk_coerce

begin
	# If length is not yet defined, STK_LENVAL equals zero

	len = STK_LENVAL(stack)

	# Find the highest type in the pointers to be returned

	one = STK_TOP(stack) - 1
	two = STK_TOP(stack) - 2

	type = STK_TYPE(stack,one)
	if (nvar > 1) {
	    switch (STK_TYPE(stack,two)) {
	    case TY_INT, TY_LONG:
		;
	    case TY_REAL:
		if (type == TY_INT)
		    type = TY_REAL
	    case TY_DOUBLE:
		type = TY_DOUBLE
	    }
	}

	# Retrieve pointers to arrays from stack. var[nvar] is top of stack
	# Convert arrays to output type when the type differs

	index = STK_TOP(stack) - nvar
	do ivar = 1, nvar {
	    if (index < 0)
		call error (1, underflow)

	    if (type == STK_TYPE(stack,index) || index < two) {
		var[ivar] = STK_VALUE(stack,index)
	    } else {
	        call stk_coerce (stack, index, type, var[ivar])
	    }
	    index = index + 1
	}

end

# STK_FIND -- Find a free array of the proper type on the stack

procedure stk_find (stack, type, index)

pointer	stack		# i: Stack descriptor
int	type		# i: Required type
int	index		# o: Position on the stack
#--
int	len
pointer	var

string	overflow  "Expression too complex to be evaluated"

begin
	# Try to find an array of the proper type already on the stack

	index = STK_TOP(stack)
	while (index < STK_HIGH(stack)) {
	    if (type == STK_TYPE(stack,index))
		break

	    index = index + 1
	}

	# If not found, allocate a new array

	if (index == MAX_STACK) {
	    call error (1, overflow)

	} else if (index == STK_HIGH(stack)) {
	    len = max (1, STK_LENVAL(stack))
	    call malloc (var, len, type)

	    STK_TYPE(stack,index) = type
	    STK_VALUE(stack,index) = var
	    STK_HIGH(stack) = STK_HIGH(stack) + 1
	}

end

# STK_FREE -- Free memory used by the stack

procedure stk_free (stack)

pointer	stack		# u: Stack pointer
#--

begin
	# Free all values above the stack bottom

	call stk_clear (stack)

	# Free substructures in stack

	if (STK_NULLARY(stack) != NULL)
	    call mfree (STK_NULLARY(stack), TY_BOOL)

	call mfree (STK_VALARY(stack), TY_INT)
	call mfree (STK_TYPARY(stack), TY_INT)

	# Free the stack structure
	call mfree (stack, TY_INT)
end

# STK_FREENULL -- Free the null array in the stack

procedure stk_freenull (stack)

pointer	stack		# u: Stack structure
#--

begin
	if (STK_NULLARY(stack) != NULL)
	    call mfree (STK_NULLARY(stack), TY_BOOL)

	STK_NULLARY(stack) = NULL
end

# STK_GET -- Get a single array from the stack

procedure stk_get (stack, pos, var, len, type)

pointer	stack		# i: Stack descriptor
int	pos		# i: Position on the stack
pointer	var		# o: Pointer to array
int	len		# o: Length of array
int	type		# o: Type of the array
#--
int	index

string	underflow  "stk_get: underflow in expression evaluator"

begin
	# Convert relative to absolute position

	if (pos == TOP) {
	    index = STK_TOP(stack) - 1
	    if (index < 0)
		call error (1, underflow)
	} else {
	    index = pos
	}

	var = STK_VALUE(stack,index)
	len = STK_LENVAL(stack)
	type = STK_TYPE(stack,index)
end

# STK_GETNULL -- Get the null array from the stack

procedure stk_getnull (stack, nullvec)

pointer	stack		# i: Stack structure
pointer nullvec		# o: Null array
#--

begin
	nullvec = STK_NULLARY(stack)
end

# STK_INIT -- Initialize the stack

procedure stk_init (stack)

pointer	stack		# o: Stack pointer
#--

begin
	# Allocate stack and initialize members to zero

	call calloc (stack, SZ_STKSTRUCT, TY_INT)

	# Allocate substructures in stack

	call malloc (STK_VALARY(stack), MAX_STACK, TY_INT)
	call malloc (STK_TYPARY(stack), MAX_STACK, TY_INT)

end

# STK_INITNULL -- Initialize the null array on the stack

procedure stk_initnull (stack, value)

pointer	stack		# u: Stack structure
bool	value		# i: Value used in initialization
#--
int	len, i
pointer	nullvec

begin
	# Only initialize if array doesn't exist

	if (STK_NULLARY(stack) == NULL) {
	    len = STK_LENVAL(stack)

	    # Allocate array
	    call malloc (nullvec, len, TY_BOOL)
	    STK_NULLARY(stack) = nullvec

	    # Initialize to value
	    do i = 0, len-1
		Memb[nullvec+i] = value
	}

end

# STK_ORNULL -- Update null array by doing a logical or

procedure stk_ornull (stack, newvec, newlen)

pointer	stack		# u: Stack structure
bool	newvec[ARB]	# i: Array of new values
int	newlen		# i: Length of new array 
#--
int	len, i
pointer	nullvec

string	badlength  "stk_ornull: length of array does not match null array"

begin
	len = STK_LENVAL(stack)
	if (len != newlen)
	    call error (1, badlength)

	call stk_initnull (stack, false)
	nullvec = STK_NULLARY(stack)

	do i = 1, len {
	    Memb[nullvec] = Memb[nullvec] || newvec[i]
	    nullvec = nullvec + 1
	}

end

# STK_POP -- Remove the specified number of arrays from the stack

procedure stk_pop (stack, npop)

pointer	stack		# u: Stack structure
int	npop		# i: Number of arrays to pop
#--
int	top, index, type
pointer	var

string	underflow  "stk_pop: underflow in expression evaluator"

begin

	top = STK_TOP(stack) - 1
	index = top - npop

	if (index < 0) {
	    call error (1, underflow)
	} else {
	    STK_TOP(stack) = index + 1
	}

	var = STK_VALUE(stack,index)
	STK_VALUE(stack,index) = STK_VALUE(stack,top)
	STK_VALUE(stack,top) = var

	type = STK_TYPE(stack,index)
	STK_TYPE(stack,index) = STK_TYPE(stack,top)
	STK_TYPE(stack,top) = type

end

# STK_POS -- Compute absolute position on stack

int procedure stk_pos (stack, pos)

pointer	stack		# i: Stack structure
int	pos		# i: Position relative to top of stack
#--

begin
	return (STK_TOP(stack) - pos)
end

# STK_SETNULL -- Set a single value in the null array to true

procedure stk_setnull (stack, index)

pointer	stack		# u: Stack structure
int	index		# i: Index into null array
#--

begin
	STK_NULL(stack,index) = true
end

