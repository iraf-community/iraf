include <time.h>
include <pkg/mef.h>

# MEFENCODE -- Routines to encode keyword, value and comment into a FITS card

define  LEN_OBJECT  63
define  CENTURY     1900

# MEF_ENCODEB -- Procedure to encode a boolean parameter into a FITS card.

procedure mef_encodeb (keyword, param, card, comment)

char	keyword[ARB]	#I FITS keyword
int	param		#I integer parameter equal to YES/NO
char	card[ARB]	#O FITS card image
char	comment[ARB]	#I FITS comment string

char	truth

begin
	if (param == YES)
	    truth = 'T'
	else
	    truth = 'F'

	call sprintf (card, LEN_CARD, "%-8.8s= %20c / %-47.47s")
	    call pargstr (keyword)
	    call pargc (truth)
	    call pargstr (comment)
end


# MEF_ENCODEI -- Procedure to encode an integer parameter into a FITS card.

procedure mef_encodei (keyword, param, card, comment)

char	keyword[ARB]	#I FITS keyword
int	param		#I integer parameter
char	card[ARB]	#O FITS card image
char	comment[ARB]	#I FITS comment string

begin
	call sprintf (card, LEN_CARD, "%-8.8s= %20d / %-47.47s")
	    call pargstr (keyword)
	    call pargi (param)
	    call pargstr (comment)
end


# MEF_ENCODEL -- Procedure to encode a long parameter into a FITS card.

procedure mef_encodel (keyword, param, card, comment)

char	keyword[ARB]		#I FITS keyword
long	param			#I long integer parameter
char	card[ARB]		#O FITS card image
char	comment[ARB]		#I FITS comment string

begin
	call sprintf (card, LEN_CARD, "%-8.8s= %20d / %-47.47s")
	    call pargstr (keyword)
	    call pargl (param)
	    call pargstr (comment)
end


# MEF_ENCODER -- Procedure to encode a real parameter into a FITS card.

procedure mef_encoder (keyword, param, card, comment, precision)

char	keyword[ARB]		#I FITS keyword
real	param			#I real parameter
char	card[ARB]		#O FITS card image
char	comment[ARB]		#I FITS comment card
int	precision		#I precision of real

begin
	call sprintf (card, LEN_CARD, "%-8.8s= %20.*e / %-47.47s")
	    call pargstr (keyword)
	    call pargi (precision)
	    call pargr (param)
	    call pargstr (comment)
end


# MEF_ENCODED -- Procedure to encode a double parameter into a FITS card.

procedure mef_encoded (keyword, param, card, comment, precision)

char	keyword[ARB]		#I FITS keyword
double	param			#I double parameter
char	card[ARB]		#O FITS card image
char	comment[ARB]		#I FITS comment string
int	precision		#I FITS precision

begin
	call sprintf (card, LEN_CARD, "%-8.8s= %20.*e / %-47.47s")
	    call pargstr (keyword)
	    call pargi (precision)
	    call pargd (param)
	    call pargstr (comment)
end


# MEF_ENCODE_AXIS -- Procedure to add the axis number to axis dependent
# keywords.

procedure mef_encode_axis (root, keyword, axisno)

char	root[ARB]		#I FITS root keyword
char	keyword[ARB]		#O FITS keyword
int	axisno			#I FITS axis number

begin
	call strcpy (root, keyword, SZ_KEYWORD)
	call sprintf (keyword, SZ_KEYWORD, "%-5.5s%-3.3s")
	    call pargstr (root)
	    call pargi (axisno)
end


# MEF_ENCODEC -- Procedure to encode an IRAF string parameter into a FITS card.

procedure mef_encodec (keyword, param, maxch, card, comment)

char	keyword[LEN_CARD]	#I FITS keyword
char	param[LEN_CARD]		#I FITS string parameter
int	maxch			#I maximum number of characters in param
char	card[LEN_CARD+1]	#O FITS card image
char	comment[LEN_CARD]	#I comment string

int	nblanks, maxchar, slashp

begin
	maxchar = max(8, min (maxch, LEN_OBJECT))
	slashp = 32 
	nblanks = LEN_CARD - (slashp + 1)
	if (maxchar >= 19) {
	   slashp = 1
	   nblanks = max (LEN_OBJECT - maxchar - slashp+3, 1)
	}
        call sprintf (card, LEN_CARD, "%-8.8s= '%*.*s' %*t/ %*.*s")
	    call pargstr (keyword)
	    call pargi (-maxchar)
	    call pargi (maxchar)
	    call pargstr (param)
	    call pargi (slashp)
	    call pargi (-nblanks)
	    call pargi (nblanks)
	    call pargstr (comment)
end


# MEF_ENCODE_DATE -- Procedure to encode the date in the form dd/mm/yy.

procedure mef_encode_date (datestr, szdate)

char	datestr[ARB]	# string containing the date
int	szdate		# number of chars in the date string

long	ctime
int	time[LEN_TMSTRUCT]
long	clktime()

begin
	ctime = clktime (long (0))
	call brktime (ctime, time)

	call sprintf (datestr, szdate, "%02s/%02s/%02s")
	    call pargi (TM_MDAY(time))
	    call pargi (TM_MONTH(time))
	    call pargi (mod (TM_YEAR(time), CENTURY))
end


# MEF_AKWC -- Encode keyword, value and comment into a FITS card and
# append it to a buffer pointed by pn.
 
procedure mef_akwc (keyword, value, len, comment, pn)

char	keyword[SZ_KEYWORD]	# keyword name
char	value[ARB]		# Keyword value
int	len			# Lenght of value
char	comment[ARB]		# Comment
pointer	pn			# Pointer to a char area
char	card[LEN_CARD]

begin
	call mef_encodec (keyword, value, len, card, comment)
	call amovc (card, Memc[pn], LEN_CARD)
	pn = pn + LEN_CARD
end


# MEF_AKWB -- Encode keyword, value and comment into a FITS card and
# append it to a buffer pointed by pn.
 
procedure mef_akwb (keyword, value, comment, pn)

char	keyword[SZ_KEYWORD]	# I keyword name
int	value			# I Keyword value (YES, NO)
char	comment[ARB]		# I Comment
pointer	pn			# I/O Pointer to a char area

pointer sp, pc

begin
        call smark(sp)
        call salloc (pc, LEN_CARD, TY_CHAR)

	call mef_encodeb (keyword, value, Memc[pc], comment)
	call amovc (Memc[pc], Memc[pn], LEN_CARD)
	pn = pn + LEN_CARD

	call sfree(sp)
end


# MEF_AKWI -- Encode keyword, value and comment into a FITS card and
# append it to a buffer pointed by pn.
 
procedure mef_akwi (keyword, value, comment, pn)

char	keyword[SZ_KEYWORD]	# I keyword name
int	value			# I Keyword value 
char	comment[ARB]		# I Comment
pointer	pn			# I/O Pointer to a char area

pointer sp, pc

begin
        call smark(sp)
        call salloc (pc, LEN_CARD, TY_CHAR)

	call mef_encodei (keyword, value, Memc[pc], comment)
	call amovc (Memc[pc], Memc[pn], LEN_CARD)
	pn = pn + LEN_CARD

	call sfree(sp)
end


# MEF_AKWR -- Encode keyword, value and comment into a FITS card and
# append it to a buffer pointed by pn.
 
procedure mef_akwr (keyword, value, comment, precision,  pn)

char	keyword[SZ_KEYWORD]	# I keyword name
real	value			# I Keyword value 
char	comment[ARB]		# I Comment
int	precision
pointer	pn			# I/O Pointer to a char area

pointer sp, pc

begin
        call smark(sp)
        call salloc (pc, LEN_CARD, TY_CHAR)

	call mef_encoder (keyword, value, Memc[pc], comment, precision)
	call amovc (Memc[pc], Memc[pn], LEN_CARD)
	pn = pn + LEN_CARD

	call sfree(sp)
end


# MEF_AKWD -- Encode keyword, value and comment into a FITS card and
# append it to a buffer pointed by pn.
 
procedure mef_akwd (keyword, value, comment, precision, pn)

char	keyword[SZ_KEYWORD]	# I keyword name
double	value			# I Keyword value 
char	comment[ARB]		# I Comment
int	precision
pointer	pn			# I/O Pointer to a char area

pointer sp, pc

begin
        call smark(sp)
        call salloc (pc, LEN_CARD, TY_CHAR)

	call mef_encoded (keyword, value, Memc[pc], comment, precision)
	call amovc (Memc[pc], Memc[pn], LEN_CARD)
	pn = pn + LEN_CARD

	call sfree(sp)
end


# NOTE: This local version of the xtools routine call handle starting
# index of zero (0). Taken from dataio/lib and modified. NZ March, 98
#
# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<ctype.h>

define	FIRST	1		# Default starting range
define	LAST	MAX_INT		# Default ending range
define	STEP	1		# Default step
define	NULL	-1		# Ranges delimiter

# DECODE_RANGES -- Parse a string containing a list of integer numbers or
# ranges, delimited by either spaces or commas.  Return as output a list
# of ranges defining a list of numbers, and the count of list numbers.
# Range limits must be positive nonnegative integers.  ERR is returned as
# the function value if a conversion error occurs.  The list of ranges is
# delimited by a single NULL.

int procedure ldecode_ranges (range_string, ranges, max_ranges, nvalues)

char	range_string[ARB]	# Range string to be decoded
int	ranges[3, max_ranges]	# Range array
int	max_ranges		# Maximum number of ranges
int	nvalues			# The number of values in the ranges

int	ip, nrange, first, last, step, ctoi()

begin
	ip = 1
	nvalues = 0

	do nrange = 1, max_ranges - 1 {
	    # Defaults to all positive integers
	    first = FIRST
	    last = LAST
	    step = STEP

	    # Skip delimiters
	    while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
		ip = ip + 1

	    # Get first limit.
	    # Must be a number, '-', 'x', or EOS.  If not return ERR.
	    if (range_string[ip] == EOS) {			# end of list
		if (nrange == 1) {
		    # Null string defaults
		    ranges[1, 1] = first
		    ranges[2, 1] = last
		    ranges[3, 1] = step
		    ranges[1, 2] = NULL
	    	    nvalues = nvalues + abs (last-first) / step + 1
		    return (OK)
		} else {
		    ranges[1, nrange] = NULL
		    return (OK)
		}
	    } else if (range_string[ip] == '-')
		;
	    else if (range_string[ip] == 'x')
		;
	    else if (IS_DIGIT(range_string[ip])) {		# ,n..
		if (ctoi (range_string, ip, first) == 0)
		    return (ERR)
	    } else
		return (ERR)

	    # Skip delimiters
	    while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
		ip = ip + 1

	    # Get last limit
	    # Must be '-', or 'x' otherwise last = first.
	    if (range_string[ip] == 'x')
		;
	    else if (range_string[ip] == '-') {
		ip = ip + 1
	        while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
		    ip = ip + 1
		if (range_string[ip] == EOS)
		    ;
		else if (IS_DIGIT(range_string[ip])) {
		    if (ctoi (range_string, ip, last) == 0)
		        return (ERR)
		} else if (range_string[ip] == 'x')
		    ;
		else
		    return (ERR)
	    } else
		last = first

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
		else
		    return (ERR)
	    }

	    # Output the range triple.
	    ranges[1, nrange] = first
	    ranges[2, nrange] = last
	    ranges[3, nrange] = step
	    nvalues = nvalues + abs (last-first) / step + 1
	}

	return (ERR)					# ran out of space
end


# GET_NEXT_NUMBER -- Given a list of ranges and the current file number,
# find and return the next file number.  Selection is done in such a way
# that list numbers are always returned in monotonically increasing order,
# regardless of the order in which the ranges are given.  Duplicate entries
# are ignored.  EOF is returned at the end of the list.

int procedure lget_next_number (ranges, number)

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
	    first = min (ranges[ip], ranges[ip+1])
	    last = max (ranges[ip], ranges[ip+1])
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


# GET_PREVIOUS_NUMBER -- Given a list of ranges and the current file number,
# find and return the previous file number.  Selection is done in such a way
# that list numbers are always returned in monotonically decreasing order,
# regardless of the order in which the ranges are given.  Duplicate entries
# are ignored.  EOF is returned at the end of the list.

int procedure lget_previous_number (ranges, number)

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
	    first = min (ranges[ip], ranges[ip+1])
	    last = max (ranges[ip], ranges[ip+1])
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


# IS_IN_RANGE -- Test number to see if it is in range.

bool procedure lis_in_range (ranges, number)

int	ranges[ARB]		# Range array
int	number			# Number to be tested against ranges

int	ip, first, last, step

begin
	for (ip=1;  ranges[ip] != NULL;  ip=ip+3) {
	    first = min (ranges[ip], ranges[ip+1])
	    last = max (ranges[ip], ranges[ip+1])
	    step = ranges[ip+2]
	    if (number >= first && number <= last)
		if (mod (number - first, step) == 0)
		    return (true)
	}

	return (false)
end
