# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# BTOI -- Convert boolean to integer.

int procedure btoi (boolean_value)

bool	boolean_value

begin
	if (boolean_value)
	    return (YES)
	else
	    return (NO)
end
