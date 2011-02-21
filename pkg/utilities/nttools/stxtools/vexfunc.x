include <mach.h>
include	"vex.h"

define	MAX_EXP		(2.3 * MAX_EXPONENT)
define	MIN_REAL	1.0e-20
define	MIN_DOUBLE	1.0d-20

# VEX_FUNC -- Miscelaneous procedures used by the vex expression evaluator.
# 
# Mostly these functions implement single opcodes such as add, sin, and 
# push. However, it also includes vex_copy[dir], which copies the array
# on the top of the stack into a user array and vex_errf, which returns
# the substitute value used when an opcode would return an undefined 
# value. The only functions which should be called directly from a user's
# program are vex_copy[dir]. 
#
# B.Simon	24-Apr-91	Original
# B.Simon	15-Oct-98	Rewrite vex_push to use embedded strings

# VEX_ABS -- Absolute value function

procedure vex_abs (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in

pointer stk_alloc()

begin
	call stk_fetch (stack, 1, in, len, type)
	out = stk_alloc (stack, len, type)
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1
		Memi[out+i] = abs (Memi[in+i])

	case TY_REAL:
	    do i = 0, len-1
		Memr[out+i] = abs (Memr[in+i])

	case TY_DOUBLE:
	    do i = 0, len-1
		Memd[out+i] = abs (Memd[in+i])

	}

	call stk_pop (stack, 1)
end

# VEX_ACOS -- Arc cosine function

procedure vex_acos (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in

double	vex_errf()
pointer stk_alloc()

begin
	call stk_fetch (stack, 1, in, len, type)
	if (type == TY_INT) {
	    out = stk_alloc (stack, len, TY_REAL)
	} else {
	    out = stk_alloc (stack, len, type)
	}
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1 {
		if (Memi[in+i] < -1 || Memi[in+i] > 1) {
		    Memr[out+i] = vex_errf (stack, i)
		} else {
		    Memr[out+i] = acos (real (Memi[in+i]))
		}
	    }

	case TY_REAL:
	    do i = 0, len-1 {
		if (Memr[in+i] < -1.0 || Memr[in+i] > 1.0) {
		    Memr[out+i] = vex_errf (stack, i)
		} else {
		    Memr[out+i] = acos (Memr[in+i])
		}
	    }

	case TY_DOUBLE:
	    do i = 0, len-1 {
		if (Memd[in+i] < -1.0 || Memd[in+i] > 1.0) {
		    Memd[out+i] = vex_errf (stack, i)
		} else {
		    Memd[out+i] = acos (Memd[in+i])
		}
	    }

	}

	call stk_pop (stack, 1)
end

# VEX_ADD -- Addition function

procedure vex_add (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in[2]

pointer stk_alloc()

begin
	call stk_fetch (stack, 2, in, len, type)
	out = stk_alloc (stack, len, type)
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1
		Memi[out+i] = Memi[in[1]+i] + Memi[in[2]+i]

	case TY_REAL:
	    do i = 0, len-1
		Memr[out+i] = Memr[in[1]+i] + Memr[in[2]+i]

	case TY_DOUBLE:
	    do i = 0, len-1
		Memd[out+i] = Memd[in[1]+i] + Memd[in[2]+i]

	}

	call stk_pop (stack, 2)
end

# VEX_AND -- Logical and

procedure vex_and (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in[2]

pointer stk_alloc()

begin
	call stk_fetch (stack, 2, in, len, type)
	out = stk_alloc (stack, len, TY_INT)
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1 {
		if (Memi[in[1]+i] != 0 && Memi[in[2]+i] != 0) {
		    Memi[out+i] = 1
		} else {
		    Memi[out+i] = 0
		}
	    }
	case TY_REAL:
	    do i = 0, len-1 {
		if (Memr[in[1]+i] != 0.0 && Memr[in[2]+i] != 0.0) {
		    Memi[out+i] = 1
		} else {
		    Memi[out+i] = 0
		}
	    }
	case TY_DOUBLE:
	    do i = 0, len-1 {
		if (Memd[in[1]+i] != 0.0 && Memd[in[2]+i] != 0.0) {
		    Memi[out+i] = 1
		} else {
		    Memi[out+i] = 0
		}
	    }
	}

	call stk_pop (stack, 2)
end

# VEX_ASIN -- Arc sine function

procedure vex_asin (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in

double	vex_errf()
pointer stk_alloc()

begin
	call stk_fetch (stack, 1, in, len, type)
	if (type == TY_INT) {
	    out = stk_alloc (stack, len, TY_REAL)
	} else {
	    out = stk_alloc (stack, len, type)
	}
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1 {
		if (Memi[in+i] < -1 || Memi[in+i] > 1) {
		    Memr[out+i] = vex_errf (stack, i)
		} else {
		    Memr[out+i] = asin (real (Memi[in+i]))
		}
	    }

	case TY_REAL:
	    do i = 0, len-1 {
		if (Memr[in+i] < -1.0 || Memr[in+i] > 1.0) {
		    Memr[out+i] = vex_errf (stack, i)
		} else {
		    Memr[out+i] = asin (Memr[in+i])
		}
	    }

	case TY_DOUBLE:
	    do i = 0, len-1 {
		if (Memd[in+i] < -1.0 || Memd[in+i] > 1.0) {
		    Memd[out+i] = vex_errf (stack, i)
		} else {
		    Memd[out+i] = asin (Memd[in+i])
		}
	    }

	}

	call stk_pop (stack, 1)
end

# VEX_ATAN -- Arc tangent function with one argument

procedure vex_atan (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in

pointer stk_alloc()

begin
	call stk_fetch (stack, 1, in, len, type)
	if (type == TY_INT) {
	    out = stk_alloc (stack, len, TY_REAL)
	} else {
	    out = stk_alloc (stack, len, type)
	}
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1
		Memr[out+i] = atan (real (Memi[in+i]))

	case TY_REAL:
	    do i = 0, len-1
		Memr[out+i] = atan (Memr[in+i])

	case TY_DOUBLE:
	    do i = 0, len-1
		Memd[out+i] = atan (Memd[in+i])

	}

	call stk_pop (stack, 1)
end

# VEX_ATAN2 -- Arc tangent function with two arguments

procedure vex_atan2 (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in[2]

double	vex_errf()
pointer stk_alloc()

begin
	call stk_fetch (stack, 2, in, len, type)
	if (type == TY_INT) {
	    out = stk_alloc (stack, len, TY_REAL)
	} else {
	    out = stk_alloc (stack, len, type)
	}
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1 {
		if (Memi[in[1]+i] == 0 && Memi[in[2]+i] == 0) {
		    Memr[out+i] = vex_errf (stack, i)
		} else {
		    Memr[out+i] = atan2 (real (Memi[in[1]+i]), 
					 real (Memi[in[2]+i]))
		}
	    }

	case TY_REAL:
	    do i = 0, len-1 {
		if (Memr[in[1]+i] == 0.0 && Memr[in[2]+i] == 0.0) {
		    Memr[out+i] = vex_errf (stack, i)
		} else {
		    Memr[out+i] = atan2 (Memr[in[1]+i], Memr[in[2]+i])
		}
	    }

	case TY_DOUBLE:
	    do i = 0, len-1 {
		if (Memd[in[1]+i] == 0.0 && Memd[in[2]+i] == 0.0) {
		    Memd[out+i] = vex_errf (stack, i)
		} else {
		    Memd[out+i] = atan2 (Memd[in[1]+i], Memd[in[2]+i])
		}
	    }

	}

	call stk_pop (stack, 2)
end

# VEX_COPYD -- Copy the top element of the stack into a double array

procedure vex_copyd (code, nullval, buffer, maxbuf)

pointer	code		# i: Pseudocode structure pointer
double	nullval		# i: Value to substitute for nulls
double	buffer[ARB]	# o: Output array
int	maxbuf		# i: Length of buffer
#--
int	len, type, ibuf
pointer	stack, var, nullbuf

string	badsize   "Cannot copy more elements than stack contains"

begin
	stack = VEX_STACK(code)
	call stk_get (stack, TOP, var, len, type)

	if (type != TY_DOUBLE)
	    call stk_coerce (stack, TOP, TY_DOUBLE, var)

	if (maxbuf <= len) {
	    do ibuf = 1, maxbuf {
		buffer[ibuf] = Memd[var]
		var = var + 1
	    }

	} else if (len != 0) {
	    call error (1, badsize)

	} else {
	    do ibuf = 1, maxbuf
		buffer[ibuf] = Memd[var]
	}

	# Set the null value in the output array

	call stk_getnull (stack, nullbuf)
	if (nullbuf != NULL) {
	    do ibuf = 1, maxbuf {
		if (Memb[nullbuf])
		    buffer[ibuf] = nullval
		if (len > 0)
		    nullbuf = nullbuf + 1
	    }
	}

	call stk_clear (stack)
end

# VEX_COPYI -- Copy the top element of the stack into an integer array

procedure vex_copyi (code, nullval, buffer, maxbuf)

pointer	code		# i: Pseudocode structure pointer
int	nullval		# i: Value to substitute for nulls
int	buffer[ARB]	# o: Output array
int	maxbuf		# i: Length of buffer
#--
int	len, type, ibuf
pointer	stack, var, nullbuf

string	badsize   "Cannot copy more elements than stack contains"

begin
	stack = VEX_STACK(code)
	call stk_get (stack, TOP, var, len, type)

	if (type != TY_INT)
	    call stk_coerce (stack, TOP, TY_INT, var)

	if (maxbuf <= len) {
	    do ibuf = 1, maxbuf {
		buffer[ibuf] = Memi[var]
		var = var + 1
	    }

	} else if (len != 0) {
	    call error (1, badsize)

	} else {
	    do ibuf = 1, maxbuf
		buffer[ibuf] = Memi[var]
	}

	# Set the null value in the output array

	call stk_getnull (stack, nullbuf)
	if (nullbuf != NULL) {
	    do ibuf = 1, maxbuf {
		if (Memb[nullbuf])
		    buffer[ibuf] = nullval
		if (len > 0)
		    nullbuf = nullbuf + 1
	    }
	}

	call stk_clear (stack)
end

# VEX_COPYR -- Copy the top element of the stack into a real array

procedure vex_copyr (code, nullval, buffer, maxbuf)

pointer	code		# i: Pseudocode structure pointer
real	nullval		# i: Value to substitute for nulls
real	buffer[ARB]	# o: Output array
int	maxbuf		# i: Length of buffer
#--
int	len, type, ibuf
pointer	stack, var, nullbuf

string	badsize   "Cannot copy more elements than stack contains"

begin
	stack = VEX_STACK(code)
	call stk_get (stack, TOP, var, len, type)

	if (type != TY_REAL)
	    call stk_coerce (stack, TOP, TY_REAL, var)

	if (maxbuf <= len) {
	    do ibuf = 1, maxbuf {
		buffer[ibuf] = Memr[var]
		var = var + 1
	    }

	} else if (len != 0) {
	    call error (1, badsize)

	} else {
	    do ibuf = 1, maxbuf
		buffer[ibuf] = Memr[var]
	}

	# Set the null value in the output array

	call stk_getnull (stack, nullbuf)
	if (nullbuf != NULL) {
	    do ibuf = 1, maxbuf {
		if (Memb[nullbuf])
		    buffer[ibuf] = nullval
		if (len > 0)
		    nullbuf = nullbuf + 1
	    }
	}

	call stk_clear (stack)
end

# VEX_COS -- Cosine function

procedure vex_cos (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in

pointer stk_alloc()

begin
	call stk_fetch (stack, 1, in, len, type)
	if (type == TY_INT) {
	    out = stk_alloc (stack, len, TY_REAL)
	} else {
	    out = stk_alloc (stack, len, type)
	}
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1
		Memr[out+i] = cos (real (Memi[in+i]))

	case TY_REAL:
	    do i = 0, len-1
		Memr[out+i] = cos (Memr[in+i])

	case TY_DOUBLE:
	    do i = 0, len-1
		Memd[out+i] = cos (Memd[in+i])

	}

	call stk_pop (stack, 1)
end

# VEX_COSH -- Hyperbolic cosine function

procedure vex_cosh (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in

double	vex_errf()
pointer stk_alloc()

begin
	call stk_fetch (stack, 1, in, len, type)
	if (type == TY_INT) {
	    out = stk_alloc (stack, len, TY_REAL)
	} else {
	    out = stk_alloc (stack, len, type)
	}
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1 {
		if (Memi[in+i] > MAX_EXP) {
		    Memr[out+i] = vex_errf (stack, i)
		} else {
		    Memr[out+i] = cosh (real (Memi[in+i]))
		}
	    }

	case TY_REAL:
	    do i = 0, len-1 {
		if (Memr[in+i] > MAX_EXP) {
		    Memr[out+i] = vex_errf (stack, i)
		} else {
		    Memr[out+i] = cosh (Memr[in+i])
		}
	    }

	case TY_DOUBLE:
	    do i = 0, len-1 {
		if (Memd[in+i] > MAX_EXP) {
		    Memd[out+i] = vex_errf (stack, i)
		} else {
		    Memd[out+i] = cosh (Memd[in+i])
		}
	    }
	}

	call stk_pop (stack, 1)
end

# VEX_CUBE -- Third power

procedure vex_cube (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in

pointer stk_alloc()

begin
	call stk_fetch (stack, 1, in, len, type)
	out = stk_alloc (stack, len, type)
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1
		Memi[out+i] = Memi[in+i] * Memi[in+i] * Memi[in+i]

	case TY_REAL:
	    do i = 0, len-1
		Memr[out+i] = Memr[in+i] * Memr[in+i] * Memr[in+i]

	case TY_DOUBLE:
	    do i = 0, len-1
		Memd[out+i] = Memd[in+i] * Memd[in+i] * Memd[in+i]

	}

	call stk_pop (stack, 1)
end

# VEX_DIM -- Positive difference

procedure vex_dim (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in[2]

pointer stk_alloc()

begin
	call stk_fetch (stack, 2, in, len, type)
	out = stk_alloc (stack, len, type)
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1
		Memi[out+i] = dim (Memi[in[1]+i], Memi[in[2]+i])

	case TY_REAL:
	    do i = 0, len-1
		Memr[out+i] = dim (Memr[in[1]+i], Memr[in[2]+i])

	case TY_DOUBLE:
	    do i = 0, len-1
		Memd[out+i] = dim (Memd[in[1]+i], Memd[in[2]+i])

	}

	call stk_pop (stack, 2)
end

# VEX_DIV -- Division function

procedure vex_div (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in[2]

double	vex_errf()
pointer stk_alloc()

begin
	call stk_fetch (stack, 2, in, len, type)
	out = stk_alloc (stack, len, type)
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1 {
		if (Memi[in[2]+i] == 0) {
		    Memi[out+i] = vex_errf (stack, i)
		} else {
		    Memi[out+i] = Memi[in[1]+i] / Memi[in[2]+i]
		}
	    }
	case TY_REAL:
	    do i = 0, len-1 {
		if (abs(Memr[in[2]+i]) < MIN_REAL) {
		    Memr[out+i] = vex_errf (stack, i)
		} else {
		    Memr[out+i] = Memr[in[1]+i] / Memr[in[2]+i]
		}
	    }

	case TY_DOUBLE:
	    do i = 0, len-1 {
		if (abs(Memd[in[2]+i]) < MIN_DOUBLE) {
		    Memd[out+i] = vex_errf (stack, i)
		} else {
		    Memd[out+i] = Memd[in[1]+i] / Memd[in[2]+i]
		}
	    }
	}

	call stk_pop (stack, 2)
end

# VEX_DOUBLE -- Convert to double

procedure vex_double (stack)

pointer	stack		# u: Stack descriptor
#--
pointer	out

begin
	call stk_coerce (stack, TOP, TY_DOUBLE, out)

end

# VEX_EQ -- Logical equality

procedure vex_eq (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in[2]

pointer stk_alloc()

begin
	call stk_fetch (stack, 2, in, len, type)
	out = stk_alloc (stack, len, TY_INT)
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1 {
		if (Memi[in[1]+i] == Memi[in[2]+i]) {
		    Memi[out+i] = 1
		} else {
		    Memi[out+i] = 0
		}
	    }
	case TY_REAL:
	    do i = 0, len-1 {
		if (Memr[in[1]+i] == Memr[in[2]+i]) {
		    Memi[out+i] = 1
		} else {
		    Memi[out+i] = 0
		}
	    }
	case TY_DOUBLE:
	    do i = 0, len-1 {
		if (Memd[in[1]+i] == Memd[in[2]+i]) {
		    Memi[out+i] = 1
		} else {
		    Memi[out+i] = 0
		}
	    }
	}

	call stk_pop (stack, 2)
end

# VEX_ERRF -- Called when a function cannot be evaluated

double procedure vex_errf (stack, index)

pointer	stack		# u: Stack descriptor
int	index		# i: Index to row with illegal operation
double	nil		# i: Value substituted for illegal operation
#--
double	substitute
double	temp

data	substitute  / 0.0 /

double	vex_nilf()

begin
	call stk_initnull (stack, false)
	call stk_setnull (stack, index)

	return (substitute)

	entry vex_nilf (nil)

	temp = substitute
	substitute = nil
	return (temp)

end

# VEX_EXP -- Exponentiation function

procedure vex_exp (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in

double	vex_errf()
pointer stk_alloc()

begin
	call stk_fetch (stack, 1, in, len, type)
	if (type == TY_INT) {
	    out = stk_alloc (stack, len, TY_REAL)
	} else {
	    out = stk_alloc (stack, len, type)
	}
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1 {
		if (Memi[in+i] > MAX_EXP) {
		    Memr[out+i] = vex_errf (stack, i)
		} else {
		    Memr[out+i] = exp (real (Memi[in+i]))
		}
	    }

	case TY_REAL:
	    do i = 0, len-1 {
		if (Memr[in+i] > MAX_EXP) {
		    Memr[out+i] = vex_errf (stack, i)
		} else {
		    Memr[out+i] = exp (Memr[in+i])
		}
	    }

	case TY_DOUBLE:
	    do i = 0, len-1 {
		if (Memd[in+i] > MAX_EXP) {
		    Memd[out+i] = vex_errf (stack, i)
		} else {
		    Memd[out+i] = exp (Memd[in+i])
		}
	    }

	}

	call stk_pop (stack, 1)
end

# VEX_GE -- Greater than or equal to function

procedure vex_ge (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in[2]

pointer stk_alloc()

begin
	call stk_fetch (stack, 2, in, len, type)
	out = stk_alloc (stack, len, TY_INT)
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1 {
		if (Memi[in[1]+i] >= Memi[in[2]+i]) {
		    Memi[out+i] = 1
		} else {
		    Memi[out+i] = 0
		}
	    }
	case TY_REAL:
	    do i = 0, len-1 {
		if (Memr[in[1]+i] >= Memr[in[2]+i]) {
		    Memi[out+i] = 1
		} else {
		    Memi[out+i] = 0
		}
	    }
	case TY_DOUBLE:
	    do i = 0, len-1 {
		if (Memd[in[1]+i] >= Memd[in[2]+i]) {
		    Memi[out+i] = 1
		} else {
		    Memi[out+i] = 0
		}
	    }
	}

	call stk_pop (stack, 2)
end

# VEX_GT -- Greater than function

procedure vex_gt (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in[2]

pointer stk_alloc()

begin
	call stk_fetch (stack, 2, in, len, type)
	out = stk_alloc (stack, len, TY_INT)
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1 {
		if (Memi[in[1]+i] > Memi[in[2]+i]) {
		    Memi[out+i] = 1
		} else {
		    Memi[out+i] = 0
		}
	    }
	case TY_REAL:
	    do i = 0, len-1 {
		if (Memr[in[1]+i] > Memr[in[2]+i]) {
		    Memi[out+i] = 1
		} else {
		    Memi[out+i] = 0
		}
	    }
	case TY_DOUBLE:
	    do i = 0, len-1 {
		if (Memd[in[1]+i] > Memd[in[2]+i]) {
		    Memi[out+i] = 1
		} else {
		    Memi[out+i] = 0
		}
	    }
	}

	call stk_pop (stack, 2)
end

# VEX_IF -- Conditional evaluation

procedure vex_if (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, index, i
pointer	out, in[3]

int	stk_pos()
pointer stk_alloc()

begin
	call stk_fetch (stack, 3, in, len, type)
	index = stk_pos (stack, 3)
	call stk_coerce (stack, index, TY_INT, in[1])

	out = stk_alloc (stack, len, type)
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1 {
		if (Memi[in[1]+i] != 0) {
		    Memi[out+i] = Memi[in[2]+i]
		} else {
		    Memi[out+i] = Memi[in[3]+i]
		}
	    }
	case TY_REAL:
	    do i = 0, len-1 {
		if (Memi[in[1]+i] != 0) {
		    Memr[out+i] = Memr[in[2]+i]
		} else {
		    Memr[out+i] = Memr[in[3]+i]
		}
	    }
	case TY_DOUBLE:
	    do i = 0, len-1 {
		if (Memi[in[1]+i] != 0) {
		    Memd[out+i] = Memd[in[2]+i]
		} else {
		    Memd[out+i] = Memd[in[3]+i]
		}
	    }
	}

	call stk_pop (stack, 3)
end

# VEX_INT -- Convert to integer

procedure vex_int (stack)

pointer	stack		# u: Stack descriptor
#--
pointer	out

begin
	call stk_coerce (stack, TOP, TY_INT, out)

end

# VEX_LE -- Less than or equal to function

procedure vex_le (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in[2]

pointer stk_alloc()

begin
	call stk_fetch (stack, 2, in, len, type)
	out = stk_alloc (stack, len, TY_INT)
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1 {
		if (Memi[in[1]+i] <= Memi[in[2]+i]) {
		    Memi[out+i] = 1
		} else {
		    Memi[out+i] = 0
		}
	    }
	case TY_REAL:
	    do i = 0, len-1 {
		if (Memr[in[1]+i] <= Memr[in[2]+i]) {
		    Memi[out+i] = 1
		} else {
		    Memi[out+i] = 0
		}
	    }
	case TY_DOUBLE:
	    do i = 0, len-1 {
		if (Memd[in[1]+i] <= Memd[in[2]+i]) {
		    Memi[out+i] = 1
		} else {
		    Memi[out+i] = 0
		}
	    }
	}

	call stk_pop (stack, 2)
end

# VEX_LT -- Less than function

procedure vex_lt (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in[2]

pointer stk_alloc()

begin
	call stk_fetch (stack, 2, in, len, type)
	out = stk_alloc (stack, len, TY_INT)
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1 {
		if (Memi[in[1]+i] < Memi[in[2]+i]) {
		    Memi[out+i] = 1
		} else {
		    Memi[out+i] = 0
		}
	    }
	case TY_REAL:
	    do i = 0, len-1 {
		if (Memr[in[1]+i] < Memr[in[2]+i]) {
		    Memi[out+i] = 1
		} else {
		    Memi[out+i] = 0
		}
	    }
	case TY_DOUBLE:
	    do i = 0, len-1 {
		if (Memd[in[1]+i] < Memd[in[2]+i]) {
		    Memi[out+i] = 1
		} else {
		    Memi[out+i] = 0
		}
	    }
	}

	call stk_pop (stack, 2)
end

# VEX_LOG -- Natural log function

procedure vex_log (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in

double	vex_errf()
pointer stk_alloc()

begin
	call stk_fetch (stack, 1, in, len, type)
	if (type == TY_INT) {
	    out = stk_alloc (stack, len, TY_REAL)
	} else {
	    out = stk_alloc (stack, len, type)
	}
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1 {
		if (Memi[in+i] <= 0) {
		    Memr[out+i] = vex_errf (stack, i)
		} else {
		    Memr[out+i] = log (real (Memi[in+i]))
		}
	    }

	case TY_REAL:
	    do i = 0, len-1 {
		if (Memr[in+i] <= 0.0) {
		    Memr[out+i] = vex_errf (stack, i)
		} else {
		    Memr[out+i] = log (Memr[in+i])
		}
	    }

	case TY_DOUBLE:
	    do i = 0, len-1 {
		if (Memd[in+i] <= 0.0) {
		    Memd[out+i] = vex_errf (stack, i)
		} else {
		    Memd[out+i] = log (Memd[in+i])
		}
	    }

	}

	call stk_pop (stack, 1)
end

# VEX_LOG10 -- Common log function

procedure vex_log10 (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in

double	vex_errf()
pointer stk_alloc()

begin
	call stk_fetch (stack, 1, in, len, type)
	if (type == TY_INT) {
	    out = stk_alloc (stack, len, TY_REAL)
	} else {
	    out = stk_alloc (stack, len, type)
	}
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1 {
		if (Memi[in+i] <= 0) {
		    Memr[out+i] = vex_errf (stack, i)
		} else {
		    Memr[out+i] = log10 (real (Memi[in+i]))
		}
	    }

	case TY_REAL:
	    do i = 0, len-1 {
		if (Memr[in+i] <= 0.0) {
		    Memr[out+i] = vex_errf (stack, i)
		} else {
		    Memr[out+i] = log10 (Memr[in+i])
		}
	    }

	case TY_DOUBLE:
	    do i = 0, len-1 {
		if (Memd[in+i] <= 0.0) {
		    Memd[out+i] = vex_errf (stack, i)
		} else {
		    Memd[out+i] = log10 (Memd[in+i])
		}
	    }

	}

	call stk_pop (stack, 1)
end

# VEX_MAX -- Maximum of two numbers

procedure vex_max (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in[2]

pointer stk_alloc()

begin
	call stk_fetch (stack, 2, in, len, type)
	out = stk_alloc (stack, len, type)
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1
		Memi[out+i] = max (Memi[in[1]+i], Memi[in[2]+i])

	case TY_REAL:
	    do i = 0, len-1
		Memr[out+i] = max (Memr[in[1]+i], Memr[in[2]+i])

	case TY_DOUBLE:
	    do i = 0, len-1
		Memd[out+i] = max (Memd[in[1]+i], Memd[in[2]+i])

	}

	call stk_pop (stack, 2)
end

# VEX_MIN -- Minimum of two numbers

procedure vex_min (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in[2]

pointer stk_alloc()

begin
	call stk_fetch (stack, 2, in, len, type)
	out = stk_alloc (stack, len, type)
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1
		Memi[out+i] = min (Memi[in[1]+i], Memi[in[2]+i])

	case TY_REAL:
	    do i = 0, len-1
		Memr[out+i] = min (Memr[in[1]+i], Memr[in[2]+i])

	case TY_DOUBLE:
	    do i = 0, len-1
		Memd[out+i] = min (Memd[in[1]+i], Memd[in[2]+i])

	}

	call stk_pop (stack, 2)
end

# VEX_MOD -- Remainder function

procedure vex_mod (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in[2]

pointer stk_alloc()

begin
	call stk_fetch (stack, 2, in, len, type)
	out = stk_alloc (stack, len, type)
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1
		Memi[out+i] = mod (Memi[in[1]+i], Memi[in[2]+i])

	case TY_REAL:
	    do i = 0, len-1
		Memr[out+i] = mod (Memr[in[1]+i], Memr[in[2]+i])

	case TY_DOUBLE:
	    do i = 0, len-1
		Memd[out+i] = mod (Memd[in[1]+i], Memd[in[2]+i])

	}

	call stk_pop (stack, 2)
end

# VEX_MUL -- Multiplication function

procedure vex_mul (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in[2]

pointer stk_alloc()

begin
	call stk_fetch (stack, 2, in, len, type)
	out = stk_alloc (stack, len, type)
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1
		Memi[out+i] = Memi[in[1]+i] * Memi[in[2]+i]

	case TY_REAL:
	    do i = 0, len-1
		Memr[out+i] = Memr[in[1]+i] * Memr[in[2]+i]

	case TY_DOUBLE:
	    do i = 0, len-1
		Memd[out+i] = Memd[in[1]+i] * Memd[in[2]+i]

	}

	call stk_pop (stack, 2)
end

# VEX_NE -- Logical inequality

procedure vex_ne (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in[2]

pointer stk_alloc()

begin
	call stk_fetch (stack, 2, in, len, type)
	out = stk_alloc (stack, len, TY_INT)
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1 {
		if (Memi[in[1]+i] != Memi[in[2]+i]) {
		    Memi[out+i] = 1
		} else {
		    Memi[out+i] = 0
		}
	    }
	case TY_REAL:
	    do i = 0, len-1 {
		if (Memr[in[1]+i] != Memr[in[2]+i]) {
		    Memi[out+i] = 1
		} else {
		    Memi[out+i] = 0
		}
	    }
	case TY_DOUBLE:
	    do i = 0, len-1 {
		if (Memd[in[1]+i] != Memd[in[2]+i]) {
		    Memi[out+i] = 1
		} else {
		    Memi[out+i] = 0
		}
	    }
	}

	call stk_pop (stack, 2)
end

# VEX_NEG -- Negation function

procedure vex_neg (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in

pointer stk_alloc()

begin
	call stk_fetch (stack, 1, in, len, type)
	out = stk_alloc (stack, len, type)
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1
		Memi[out+i] = - Memi[in+i]

	case TY_REAL:
	    do i = 0, len-1
		Memr[out+i] = - Memr[in+i]

	case TY_DOUBLE:
	    do i = 0, len-1
		Memd[out+i] = - Memd[in+i]

	}

	call stk_pop (stack, 1)
end

# VEX_NINT -- Nearest integer

procedure vex_nint (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	in, out

pointer stk_alloc()

begin
	call stk_fetch (stack, 1, in, len, type)
	out = stk_alloc (stack, len, type)
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1 
		Memi[out+i] = Memi[in+i]

	case TY_REAL:
	    do i = 0, len-1
		Memr[out+i] = anint (Memr[in+i])

	case TY_DOUBLE:
	    do i = 0, len-1
		Memd[out+i] = anint (Memd[in+i])

	}

	call stk_pop (stack, 1)
end

# VEX_NOT -- Logical negation

procedure vex_not (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in

pointer stk_alloc()

begin
	call stk_fetch (stack, 1, in, len, type)
	out = stk_alloc (stack, len, TY_INT)
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1 {
		if (Memi[in+i] != 0) {
		    Memi[out+i] = 0
		} else {
		    Memi[out+i] = 1
		}
	    }
	case TY_REAL:
	    do i = 0, len-1 {
		if (Memr[in+i] != 0.0) {
		    Memi[out+i] = 0
		} else {
		    Memi[out+i] = 1
		}
	    }
	case TY_DOUBLE:
	    do i = 0, len-1 {
		if (Memd[in+i] != 0.0) {
		    Memi[out+i] = 0
		} else {
		    Memi[out+i] = 1
		}
	    }
	}

	call stk_pop (stack, 1)
end

# VEX_OR -- Logical or

procedure vex_or (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in[2]

pointer stk_alloc()

begin
	call stk_fetch (stack, 2, in, len, type)
	out = stk_alloc (stack, len, TY_INT)
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1 {
		if (Memi[in[1]+i] != 0 || Memi[in[2]+i] != 0) {
		    Memi[out+i] = 1
		} else {
		    Memi[out+i] = 0
		}
	    }
	case TY_REAL:
	    do i = 0, len-1 {
		if (Memr[in[1]+i] != 0.0 || Memr[in[2]+i] != 0.0) {
		    Memi[out+i] = 1
		} else {
		    Memi[out+i] = 0
		}
	    }
	case TY_DOUBLE:
	    do i = 0, len-1 {
		if (Memd[in[1]+i] != 0.0 || Memd[in[2]+i] != 0.0) {
		    Memi[out+i] = 1
		} else {
		    Memi[out+i] = 0
		}
	    }
	}

	call stk_pop (stack, 2)
end

# VEX_POW -- Exponentiation function

procedure vex_pow (stack)

pointer	stack		# u: Stack descriptor
#--
double	dtemp
int	index, len, type, i
pointer	out, in[2]
real	rtemp

double	vex_errf()
int	stk_pos()
pointer stk_alloc()

begin
	# If the exponent is an integer, use the normal exponentiation
	# otherwise, use the logarithmic formulation

	call stk_get (stack, TOP, in[2], len, type)

	if (type == TY_INT) {
	    index = stk_pos (stack, 2)
	    call stk_get (stack, index, in[1], len, type)

	    out = stk_alloc (stack, len, type)
	    len = max (len, 1)

	    switch (type) {
	    case TY_INT, TY_LONG:
		do i = 0, len-1
		    Memi[out+i] = Memi[in[1]+i] ** Memi[in[2]+i]
	    case TY_REAL:
		do i = 0, len-1
		    Memr[out+i] = Memr[in[1]+i] ** Memi[in[2]+i]
	    case TY_DOUBLE:
		do i = 0, len-1
		    Memd[out+i] = Memd[in[1]+i] ** Memi[in[2]+i]
	    }

	} else {
	    call stk_fetch (stack, 2, in, len, type)
	    out = stk_alloc (stack, len, type)
	    len = max (len, 1)

	    switch (type) {
	    case TY_REAL:
		do i = 0, len-1 {
		    if (Memr[in[1]+i] <= 0.0) {
			Memr[out+i] = vex_errf (stack, i)
		    } else {
			rtemp = Memr[in[2]+i] * log(Memr[in[1]+i])
			if (rtemp > MAX_EXP) {
			    Memr[out+i] = vex_errf (stack, i)
			} else {
			    Memr[out+i] = exp (rtemp)
			}
		    }
		}

	    case TY_DOUBLE:
		do i = 0, len-1 {
		    if (Memd[in[1]+i] <= 0.0) {
			Memd[out+i] = vex_errf (stack, i)
		    } else {
			dtemp = Memd[in[2]+i] * log(Memd[in[1]+i])
			if (dtemp > MAX_EXP) {
			    Memd[out+i] = vex_errf (stack, i)
			} else {
			    Memd[out+i] = exp (dtemp)
			}
		    }
		}
	    }
	}

	call stk_pop (stack, 2)
end

# VEX_PUSH -- Push a token onto the stack

procedure vex_push (stack, getvar, type, token)

pointer	stack		# i: Stack structure
extern	getvar		# i: Function to return a variable
int	type		# i: Token type
char	token[ARB]	# i: Token string
#--
double	dval
int	len, ic, nc, ival
pointer	sp, errmsg, var
real	rval

string	badtype  "Unrecognized token type (%d)"

int	ctoi(), ctor(), ctod()
pointer stk_alloc()
errchk	getvar

begin
	call smark (sp)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	len = STK_LENVAL(stack)

	switch (type) {
	case Y_VAR:
	    call getvar (stack, token)

	case Y_INT:
	    var = stk_alloc (stack, len, TY_INT)

	    ic = 1
	    len = max (len, 1)
	    nc = ctoi (token, ic, ival)
	    call amovki (ival, Memi[var], len)

	case Y_REAL:
	    var = stk_alloc (stack, len, TY_REAL)

	    ic = 1
	    len = max (len, 1)
	    nc = ctor (token, ic, rval)
	    call amovkr (rval, Memr[var], len)

	case Y_DOUBLE:
	    var = stk_alloc (stack, len, TY_DOUBLE)

	    ic = 1
	    len = max (len, 1)
	    nc = ctod (token, ic, dval)
	    call amovkd (dval, Memd[var], len)

	default:
	    call sprintf (Memc[errmsg], SZ_LINE, badtype)
	    call pargi (type)
	    call error (1, Memc[errmsg])
	}

	call sfree (sp)
end

# VEX_REAL -- Convert to real

procedure vex_real (stack)

pointer	stack		# u: Stack descriptor
#--
pointer	out

begin
	call stk_coerce (stack, TOP, TY_REAL, out)

end

# VEX_SIG -- Sign transfer function

procedure vex_sig (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in[2]

pointer stk_alloc()

begin
	call stk_fetch (stack, 2, in, len, type)
	out = stk_alloc (stack, len, type)
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1
		Memi[out+i] = sign (Memi[in[1]+i], Memi[in[2]+i])

	case TY_REAL:
	    do i = 0, len-1
		Memr[out+i] = sign (Memr[in[1]+i], Memr[in[2]+i])

	case TY_DOUBLE:
	    do i = 0, len-1
		Memd[out+i] = sign (Memd[in[1]+i], Memd[in[2]+i])

	}

	call stk_pop (stack, 2)
end

# VEX_SIN -- Sine function

procedure vex_sin (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in

pointer stk_alloc()

begin
	call stk_fetch (stack, 1, in, len, type)
	if (type == TY_INT) {
	    out = stk_alloc (stack, len, TY_REAL)
	} else {
	    out = stk_alloc (stack, len, type)
	}
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1
		Memr[out+i] = sin (real (Memi[in+i]))

	case TY_REAL:
	    do i = 0, len-1
		Memr[out+i] = sin (Memr[in+i])

	case TY_DOUBLE:
	    do i = 0, len-1
		Memd[out+i] = sin (Memd[in+i])

	}

	call stk_pop (stack, 1)
end

# VEX_SINH -- Hyperbolic sine function

procedure vex_sinh (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in

double	vex_errf()
pointer stk_alloc()

begin
	call stk_fetch (stack, 1, in, len, type)
	if (type == TY_INT) {
	    out = stk_alloc (stack, len, TY_REAL)
	} else {
	    out = stk_alloc (stack, len, type)
	}
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1 {
		if (Memi[in+i] > MAX_EXP) {
		    Memr[out+i] = vex_errf (stack, i)
		} else {
		    Memr[out+i] = sinh (real (Memi[in+i]))
		}
	    }

	case TY_REAL:
	    do i = 0, len-1 {
		if (Memr[in+i] > MAX_EXP) {
		    Memr[out+i] = vex_errf (stack, i)
		} else {
		    Memr[out+i] = sinh (Memr[in+i])
		}
	    }

	case TY_DOUBLE:
	    do i = 0, len-1 {
		if (Memd[in+i] > MAX_EXP) {
		    Memd[out+i] = vex_errf (stack, i)
		} else {
		Memd[out+i] = sinh (Memd[in+i])
		}
	    }
	}

	call stk_pop (stack, 1)
end

# VEX_SQR -- Second power

procedure vex_sqr (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in

pointer stk_alloc()

begin
	call stk_fetch (stack, 1, in, len, type)
	out = stk_alloc (stack, len, type)
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1
		Memi[out+i] = Memi[in+i] * Memi[in+i]

	case TY_REAL:
	    do i = 0, len-1
		Memr[out+i] = Memr[in+i] * Memr[in+i]

	case TY_DOUBLE:
	    do i = 0, len-1
		Memd[out+i] = Memd[in+i] * Memd[in+i]

	}

	call stk_pop (stack, 1)
end

# VEX_SQRT -- Square root function

procedure vex_sqrt (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in

double	vex_errf()
pointer stk_alloc()

begin
	call stk_fetch (stack, 1, in, len, type)
	if (type == TY_INT) {
	    out = stk_alloc (stack, len, TY_REAL)
	} else {
	    out = stk_alloc (stack, len, type)
	}
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1 {
		if (Memi[in+i] < 0) {
		    Memr[out+i] = vex_errf (stack, i)
		} else {
		    Memr[out+i] = sqrt (real (Memi[in+i]))
		}
	    }

	case TY_REAL:
	    do i = 0, len-1 {
		if (Memr[in+i] < 0.0) {
		    Memr[out+i] = vex_errf (stack, i)
		} else {
		    Memr[out+i] = sqrt (Memr[in+i])
		}
	    }

	case TY_DOUBLE:
	    do i = 0, len-1 {
		if (Memd[in+i] < 0.0) {
		    Memd[out+i] = vex_errf (stack, i)
		} else {
		    Memd[out+i] = sqrt (Memd[in+i])
		}
	    }

	}

	call stk_pop (stack, 1)
end

# VEX_SUB -- Subtraction function

procedure vex_sub (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in[2]

pointer stk_alloc()

begin
	call stk_fetch (stack, 2, in, len, type)
	out = stk_alloc (stack, len, type)
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1
		Memi[out+i] = Memi[in[1]+i] - Memi[in[2]+i]

	case TY_REAL:
	    do i = 0, len-1
		Memr[out+i] = Memr[in[1]+i] - Memr[in[2]+i]

	case TY_DOUBLE:
	    do i = 0, len-1
		Memd[out+i] = Memd[in[1]+i] - Memd[in[2]+i]

	}

	call stk_pop (stack, 2)
end

# VEX_TAN -- Tangent function

procedure vex_tan (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in

pointer stk_alloc()

begin
	call stk_fetch (stack, 1, in, len, type)
	if (type == TY_INT) {
	    out = stk_alloc (stack, len, TY_REAL)
	} else {
	    out = stk_alloc (stack, len, type)
	}
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1
		Memr[out+i] = tan (real (Memi[in+i]))

	case TY_REAL:
	    do i = 0, len-1
		Memr[out+i] = tan (Memr[in+i])

	case TY_DOUBLE:
	    do i = 0, len-1
		Memd[out+i] = tan (Memd[in+i])

	}

	call stk_pop (stack, 1)
end

# VEX_TANH -- Hyperbolic tangent function

procedure vex_tanh (stack)

pointer	stack		# u: Stack descriptor
#--
int	len, type, i
pointer	out, in

pointer stk_alloc()

begin
	call stk_fetch (stack, 1, in, len, type)
	if (type == TY_INT) {
	    out = stk_alloc (stack, len, TY_REAL)
	} else {
	    out = stk_alloc (stack, len, type)
	}
	len = max (len, 1)

	switch (type) {
	case TY_INT, TY_LONG:
	    do i = 0, len-1
		Memr[out+i] = tanh (real (Memi[in+i]))

	case TY_REAL:
	    do i = 0, len-1
		Memr[out+i] = tanh (Memr[in+i])

	case TY_DOUBLE:
	    do i = 0, len-1
		Memd[out+i] = tanh (Memd[in+i])

	}

	call stk_pop (stack, 1)
end




