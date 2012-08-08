# Specialized CL get routines.


# CLGRANGES -- Get a range.  A range string is input and the string is
# decoded into a range array.  The number of values in the range array is
# returned by the function.

int procedure clgranges (param, min_value, max_value, ranges, max_ranges)

char	param[ARB]
int	min_value
int	max_value
int	ranges[ARB]
int	max_ranges

char	str[SZ_LINE]
int	n

int	decode_ranges()

begin
	call clgstr (param, str, SZ_LINE)

	if (decode_ranges (str,ranges,max_ranges,min_value,max_value,n) == ERR)
	    call error (0, "Error in range string")

	return (n)
end
