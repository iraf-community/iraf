# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# NCPCHR -- Put a single character (byte) into a packed array.  Do nothing if
# the index is out of bounds.

procedure ncpchr (ichars, len_ichars, index, char_value)

int	ichars[ARB]		# packed character array
int	len_ichars		# length of the array
int	index			# index of char to be set
int	char_value		# value to be stored

char	ch[1]

begin
	if (index >= 1 && index <= len_ichars) {
	    ch[1] = char_value
	    call chrpak (ch, 1, ichars, index, 1)
	}
end
