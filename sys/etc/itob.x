# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ITOB -- Convert integer to boolean.

bool procedure itob (integer_value)

int	integer_value

begin
	if (integer_value == NO)
	    return (false)
	else
	    return (true)
end
