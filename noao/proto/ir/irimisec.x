include <ctype.h>

# IR_DECODE_SECTION -- Procedure to decode the reference section.

int procedure ir_decode_section (section, ncols, nrows, c1ref, c2ref, l1ref,
	l2ref)

char	section[ARB]	# reference subraster section
int	ncols		# number of columns in the image
int	nrows		# number of rows in the image
int	c1ref		# initial column
int	c2ref		# final reference column
int	l1ref		# initial reference line
int	l2ref		# final reference line

char	leftbkt
int	index, ip, step
int	ir_decode_subscript(), stridx()

begin
	leftbkt = '['
	index = stridx (leftbkt, section)
	if (index == 0)
	    return (ERR)
	ip = index + 1
        if (ir_decode_subscript (section, ip, ncols, c1ref, c2ref, step) == ERR)
	    return (ERR)
        if (ir_decode_subscript (section, ip, nrows, l1ref, l2ref, step) == ERR)
	    return (ERR)
	return (OK)
end


# IR_DECODE_SUBSCRIPT -- Decode a single subscript expression to produce the
# range of values for that subscript (X1:X2), and the sampling step size, STEP.
# Note that X1 may be less than, greater than, or equal to X2, and STEP may
# be a positive or negative nonzero integer.  Various shorthand notations are
# permitted, as is embedded whitespace.

int procedure ir_decode_subscript (section, ip, maxnumber, x1, x2, step)

char	section[ARB]
int	ip
int	maxnumber
long	x1, x2, step, temp
int	ctol()

begin
	x1 = 1
	x2 = maxnumber
	step = 1

	while (IS_WHITE(section[ip]))
	    ip = ip + 1

	# Get X1, X2.
	if (ctol (section, ip, temp) > 0) {			# [x1
	    x1 = temp
	    if (section[ip] == ':') {	
		ip = ip + 1
		if (ctol (section, ip, x2) == 0)		# [x1:x2
		    return (ERR)
	    } else
		x2 = x1

	} else if (section[ip] == '-') {
	    x1 = maxnumber						# [-*
	    x2 = 1
	    ip = ip + 1
	    if (section[ip] == '*')
		ip = ip + 1

	} else if (section[ip] == '*')				# [*
	    ip = ip + 1

	while (IS_WHITE(section[ip]))
	    ip = ip + 1

	# Get sample step size, if give.
	if (section[ip] == ':') {				# ..:step
	    ip = ip + 1
	    if (ctol (section, ip, step) == 0)
		return (ERR)
	    else if (step == 0)
		return (ERR)
	}

	# Allow notation such as "-*:5", (or even "-:5") where the step
	# is obviously supposed to be negative.

	if (x1 > x2 && step > 0)
	    step = -step
	
	while (IS_WHITE(section[ip]))
	    ip = ip + 1

	if (section[ip] == ',') {
	    ip = ip + 1
	    return (OK)
	} else if (section[ip] == ']')
	    return (OK)
	else
	    return (ERR)

end
