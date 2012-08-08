# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

task	ranges = t_ranges

define	MAX_RANGES	101	# Maximum number of range parameters


# T_RANGES -- Test the range expression expansion package.

procedure t_ranges ()

char	range_string[SZ_LINE]		# Range string
int	number				# Test integer number

int	ranges[3, MAX_RANGES]
int	nvalues, next_number
int	decode_ranges(), get_next_number(), get_previous_number()
bool	is_in_range()
int	clglpi()

begin
	# Get program parameters
	call clgstr ("range_string", range_string, SZ_LINE)

	# Decode the range string
	if (decode_ranges (range_string, ranges, MAX_RANGES, nvalues) == ERR)
	    call error (1, "Error parsing range string")
	call printf ("Number of values = %d\n")
	    call pargi (nvalues)

	# Test is_in_range
	while (clglpi ("number", number) != EOF) {
	    if (is_in_range (ranges, number)) {
		call printf ("%d is in range\n")
		    call pargi (number)
	    } else {
		call printf ("%d is not in range\n")
		    call pargi (number)
	    }
	    next_number = number
	    if (get_next_number (ranges, next_number) != EOF) {
		call printf ("Next number is %d\n")
		    call pargi (next_number)
	    }
	    next_number = number
	    if (get_previous_number (ranges, next_number) != EOF) {
		call printf ("Previous number is %d\n")
		    call pargi (next_number)
	    }
	}
end
