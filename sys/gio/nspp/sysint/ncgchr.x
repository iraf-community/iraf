# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# NCGCHR -- Get a single character (byte) from a packed array.  Return
# a blank if the index is out of bounds.

procedure ncgchr (ichars, len_ichars, index, char_value)

int	ichars[ARB]		# packed character array
int	len_ichars		# length of the array
int	index			# index of char to be extracted
int	char_value		# return value

char	ch

begin
	if (index < 1 || index > len_ichars)
	    char_value = ' '
	else {
	    call chrupk (ichars, index, ch, 1, 1)
	    char_value = ch
	}
end
