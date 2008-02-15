# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<ctype.h>

.help ranges xtools "Range Parsing Tools"
.ih
PURPOSE

These tools
parse a string using a syntax to represent integer values, ranges, and
steps.   The parsed string is used to generate a list of integers for various
purposes such as specifying lines or columns in an image or tape file numbers.
.ih
SYNTAX

The syntax for the range string consists of positive integers, '-' (minus),
'x', ',' (comma), and whitespace.  The commas and whitespace are ignored
and may be freely used for clarity.  The remainder of the string consists
of sequences of five fields.  The first field is the beginning of a range,
the second is a '-', the third is the end of the range, the fourth is
a 'x', and the fifth is a step size.  Any of the five fields may be
missing causing various default actions.  The defaults are illustrated in
the following table.

.nf
-3x1	A missing starting value defaults to 1.
2-x1	A missing ending value defaults to MAX_INT.
2x1	A missing ending value defaults to MAX_INT.
2-4	A missing step defaults to 1.
4	A missing ending value and step defaults to an ending
	value equal to the starting value and a step of 1.
x2	Missing starting and ending values defaults to
	the range 1 to MAX_INT with the specified step.
""	The null string is equivalent to "1 - MAX_INT x 1",
	i.e all positive integers.
.fi

The specification of several ranges yields the union of the ranges.
.ih
EXAMPLES

The following examples further illustrate the range syntax.

.nf
-	All positive integers.
1,5,9	A list of integers equivalent to 1-1x1,5-5x1,9-9x1.
x2	Every second positive integer starting with 1.
2x3	Every third positive integer starting with 2.
-10	All integers between 1 and 10.
5-	All integers greater than or equal to 5.
9-3x1	The integers 3,6,9.
.fi
.ih
PROCEDURES

.ls 4 is_decode_ranges

.nf
int procedure is_decode_ranges (range_string, ranges, max_ranges, minimum,
    maximum, nvalues)

char	range_string[ARB]	# Range string to be decoded
int	ranges[3, max_ranges]	# Range array
int	max_ranges		# Maximum number of ranges
int	minimum, maximum	# Minimum and maximum range values allowed
int	nvalues			# The number of values in the ranges
.fi

The range string is decoded into an integer array of maximum dimension
3 * max_ranges.  Each range consists of three consecutive integers
corresponding to the starting and ending points of the range and the
step size.  The number of integers covered by the ranges is returned
as nvalue.  The end of the set of ranges is marked by a NULL.
The returned status is either ERR or OK.
.le
.ls 4 is_next_number, is_previous_number

.nf
int procedure is_next_number (ranges, number)
int procedure is_previous_number (ranges, number)

int	ranges[ARB]		# Range array
int	number			# Both input and output parameter
.fi

Given a value for number the procedures find the next (previous) number in
increasing (decreasing)
value within the set of ranges.  The next (previous) number is returned in
the number argument.  A returned status is either OK or EOF.
EOF indicates that there are no greater values.  The usual usage would
be in a loop of the form:

.nf
	number = 0
	while (is_next_number (ranges, number) != EOF) {
	    <Statements using number>
	}
.fi
.le
.ls 4 is_in_rangelist

.nf
bool procedure is_in_rangelist (ranges, number)

int	ranges[ARB]		# Ranges array
int	number			# Number to check againts ranges
.fi

A boolean value is returned indicating whether number is covered by
the ranges.

.endhelp


# IS_DECODE_RANGES -- Parse a string containing a list of integer numbers or
# ranges, delimited by either spaces or commas.  Return as output a list
# of ranges defining a list of numbers, and the count of list numbers.
# Range limits must be positive nonnegative integers.  ERR is returned as
# the function value if a conversion error occurs.  The list of ranges is
# delimited by a single NULL.


int procedure is_decode_ranges (range_string, ranges, max_ranges, minimum,
    maximum, nvalues)

char	range_string[ARB]	# Range string to be decoded
int	ranges[3, max_ranges]	# Range array
int	max_ranges		# Maximum number of ranges
int	minimum, maximum	# Minimum and maximum range values allowed
int	nvalues			# The number of values in the ranges

int	ip, nrange, out_of_range, a, b, first, last, step, ctoi()

begin
	ip = 1
	nrange = 1
	nvalues = 0
	out_of_range = 0

	while (nrange < max_ranges) {
	    # Default values
	    a = minimum
	    b = maximum
	    step = 1

	    # Skip delimiters
	    while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
		ip = ip + 1

	    # Get first limit.
	    # Must be a number, '*', '-', 'x', or EOS.  If not return ERR.
	    if (range_string[ip] == EOS) {			# end of list
		if (nrange == 1) {
		    if (out_of_range == 0) {
		        # Null string defaults
		        ranges[1, 1] = a
		        ranges[2, 1] = b
		        ranges[3, 1] = step
		        ranges[1, 2] = NULL
	    	        nvalues = (b - a) / step + 1
		        return (OK)
		    } else {
			# Only out of range data
			return (ERR)
		    }
		} else {
		    ranges[1, nrange] = NULL
		    return (OK)
		}
	    } else if (range_string[ip] == '-')
		;
	    else if (range_string[ip] == '*')
		;
	    else if (range_string[ip] == 'x')
		;
	    else if (IS_DIGIT(range_string[ip])) {		# ,n..
		if (ctoi (range_string, ip, a) == 0)
		    return (ERR)
	    } else
		return (ERR)

	    # Skip delimiters
	    while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
		ip = ip + 1

	    # Get last limit
	    # Must be '-', '*', or 'x' otherwise b = a.
	    if (range_string[ip] == 'x')
		;
	    else if ((range_string[ip] == '-') || (range_string[ip] == '*')) {
		ip = ip + 1
	        while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
		    ip = ip + 1
		if (range_string[ip] == EOS)
		    ;
		else if (IS_DIGIT(range_string[ip])) {
		    if (ctoi (range_string, ip, b) == 0)
		        return (ERR)
		} else if (range_string[ip] == 'x')
		    ;
		else
		    return (ERR)
	    } else
		b = a

	    # Skip delimiters
	    while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
		ip = ip + 1

	    # Get step.
	    # Must be 'x' or assume default step.
	    if (range_string[ip] == 'x') {
		ip = ip + 1
	        while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
		    ip = ip + 1
		if (range_string[ip] == EOS)
		    ;
		else if (IS_DIGIT(range_string[ip])) {
		    if (ctoi (range_string, ip, step) == 0)
		        ;
		} else if (range_string[ip] == '-')
		    ;
		else if (range_string[ip] == '*')
		    ;
		else
		    return (ERR)
	    }

	    # Output the range triple.
	    first = min (a, b)
	    last = max (a, b)
	    if (first < minimum)
		first = minimum + mod (step - mod (minimum - first, step), step)
	    if (last > maximum)
		last = maximum - mod (last - maximum, step)
	    if (first <= last) {
	        ranges[1, nrange] = first
	        ranges[2, nrange] = last
	        ranges[3, nrange] = step
	        nvalues = nvalues + (last - first) / step + 1
		nrange = nrange + 1
	    } else
		out_of_range = out_of_range + 1
	}

	return (ERR)					# ran out of space
end


# IS_NEXT_NUMBER -- Given a list of ranges and the current file number,
# find and return the next file number.  Selection is done in such a way
# that list numbers are always returned in monotonically increasing order,
# regardless of the order in which the ranges are given.  Duplicate entries
# are ignored.  EOF is returned at the end of the list.

int procedure is_next_number (ranges, number)

int	ranges[ARB]		# Range array
int	number			# Both input and output parameter

int	ip, first, last, step, next_number, remainder

begin
	# If number+1 is anywhere in the list, that is the next number,
	# otherwise the next number is the smallest number in the list which
	# is greater than number+1.

	number = number + 1
	next_number = MAX_INT

	for (ip=1;  ranges[ip] != NULL;  ip=ip+3) {
	    first = ranges[ip]
	    last = ranges[ip+1]
	    step = ranges[ip+2]
	    if (number >= first && number <= last) {
		remainder = mod (number - first, step)
		if (remainder == 0)
		    return (number)
		if (number - remainder + step <= last)
		    next_number = number - remainder + step
	    } else if (first > number)
		next_number = min (next_number, first)
	}

	if (next_number == MAX_INT)
	    return (EOF)
	else {
	    number = next_number
	    return (number)
	}
end


# IS_PREVIOUS_NUMBER -- Given a list of ranges and the current file number,
# find and return the previous file number.  Selection is done in such a way
# that list numbers are always returned in monotonically decreasing order,
# regardless of the order in which the ranges are given.  Duplicate entries
# are ignored.  EOF is returned at the end of the list.

int procedure is_previous_number (ranges, number)

int	ranges[ARB]		# Range array
int	number			# Both input and output parameter

int	ip, first, last, step, next_number, remainder

begin
	# If number-1 is anywhere in the list, that is the previous number,
	# otherwise the previous number is the largest number in the list which
	# is less than number-1.

	number = number - 1
	next_number = 0

	for (ip=1;  ranges[ip] != NULL;  ip=ip+3) {
	    first = ranges[ip]
	    last = ranges[ip+1]
	    step = ranges[ip+2]
	    if (number >= first && number <= last) {
		remainder = mod (number - first, step)
		if (remainder == 0)
		    return (number)
		if (number - remainder >= first)
		    next_number = number - remainder
	    } else if (last < number) {
		remainder = mod (last - first, step)
		if (remainder == 0)
		    next_number = max (next_number, last)
		else if (last - remainder >= first)
		    next_number = max (next_number, last - remainder)
	    }
	}

	if (next_number == 0)
	    return (EOF)
	else {
	    number = next_number
	    return (number)
	}
end


# IS_IN_RANGELLIST -- Test number to see if it is in range.

bool procedure is_in_rangelist (ranges, number)

int	ranges[ARB]		# Range array
int	number			# Number to be tested against ranges

int	ip, first, last, step

begin
	for (ip=1;  ranges[ip] != NULL;  ip=ip+3) {
	    first = ranges[ip]
	    last = ranges[ip+1]
	    step = ranges[ip+2]
	    if (number >= first && number <= last)
		if (mod (number - first, step) == 0)
		    return (TRUE)
	}

	return (FALSE)
end


# IS_EXPAND_RANGES -- Expand a range string into a array of values.

int procedure is_expand_ranges (ranges, array, max_nvalues)

int	ranges[ARB]			# Range array
int	array[max_nvalues]		# Array of values
int	max_nvalues			# Maximum number of values

int	n, value

int	is_next_number()

begin
	n = 0
	value = 0
	while ((n < max_nvalues) && (is_next_number (ranges, value) != EOF)) {
	    n = n + 1
	    array[n] = value
	}

	return (n)
end


# IS_SELECT_RANGES -- Select array values in the ranges.
# The input and output arrays may be the same.

procedure is_select_ranges (a, b, ranges)

real	a[ARB]				# Input array
real	b[ARB]				# Output array
int	ranges[3, ARB]			# Ranges

int	i, j, npts, nmove

begin
	npts = 0
	for (i = 1; ranges[1, i] != NULL; i = i + 1) {
	    if (ranges[3, i] == 1) {
	        nmove = ranges[2, i] - ranges[1, i] + 1
	        call amovr (a[ranges[1, i]], b[npts + 1], nmove)
		npts = npts + nmove
	    } else {
		do j = ranges[1, i], ranges[2, i], ranges[3, i] {
		    npts = npts + 1
		    b[npts] = a[j]
		}
	    }
	}
end


# IS_CHOOSE_RANGESI -- Copy the selected values from array a to b.

int procedure is_choose_rangesi (indices, a, b, npts, ifirst, ilast)

int	indices[ARB]		# array of indices
int	a[ARB]			# input array
int	b[ARB]			# output array
int	npts			# number of points
int	ifirst			# first index
int	ilast			# last index

int	i, element

begin
	element = 1
	do i = 1, npts {
	    if (indices[i] < ifirst || indices[i] > ilast)
		next
	    b[element] = a[indices[i]]
	    element = element + 1
	}
	return (element - 1)
end


# IS_CHOOSE_RANGESR -- Copy the selected values from array a to b.

int procedure is_choose_rangesr (indices, a, b, npts, ifirst, ilast)

int	indices[ARB]		# array of indices
real	a[ARB]			# input array
real	b[ARB]			# output array
int	npts			# number of points
int	ifirst			# first element to be extracted
int	ilast			# last element to be extracted

int	i, element

begin
	element = 1
	do i = 1, npts {
	    if (indices[i] < ifirst || indices[i] > ilast)
		next
	    b[element] = a[indices[i]]
	    element = element + 1
	}
	return (element - 1)
end


# IS_MAKE_RANGES -- Procedure to make a set of ranges from an ordered list
# of column numbers. Only a step size of 1 is checked for.

int procedure is_make_ranges (list, npts, ranges, max_nranges)

int	list[ARB]	# list of column numbers in increasing order
int	npts		# number of list elements
int	ranges[ARB]	# output ranges
int	max_nranges	# the maximum number of ranges

bool	next_range
int	ip, op, nranges

begin
	# If zero list elements return
	if (npts == 0) {
	    ranges[1] = NULL	
	    return (0)
	}

	# Initialize
	nranges = 0
	ranges[1] = list[1]
	op = 2
	next_range = false

	# Loop over column list
	for (ip = 2; ip <= npts && nranges < max_nranges; ip = ip + 1) {
	    if ((list[ip] != (list[ip-1] + 1))) {
		ranges[op] = list[ip-1]
		op = op + 1
		ranges[op] = 1
		op = op + 1
		nranges = nranges + 1
		ranges[op] = list[ip]
		op = op + 1
	    }
	}

	# finish off
	if (npts == 1) {
	    ranges[op] = list[npts]
	    ranges[op+1] = 1
	    ranges[op+2] = NULL
	    nranges = 1
	} else if (nranges == max_nranges) {
	    ranges[op-1] = NULL
	} else {
	    ranges[op] = list[npts]
	    ranges[op+1] = 1
	    ranges[op+2] = NULL
	    nranges = nranges + 1
	}

	return (nranges)
end
