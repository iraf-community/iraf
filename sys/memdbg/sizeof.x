# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# SIZEOF -- Return the size in chars of one of the fundamental datatypes.

int procedure sizeof (dtype)

int	dtype
include	<szdtype.inc>

begin
	return (ty_size[dtype])
end
