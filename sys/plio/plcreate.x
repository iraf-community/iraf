# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# PL_CREATE -- Create an empty mask with the given dimensionality and size.
# A newly created mask has all line pointers pointing to the empty line,
# which is stored as the first entry in the line list buffer.

pointer procedure pl_create (naxes, axlen, depth)

int	naxes			#I number of axes (dimensionality of mask)
long	axlen[ARB]		#I length of each axis
int	depth			#I mask depth, bits

pointer	pl
pointer	pl_open()
errchk	pl_open

begin
	pl = pl_open (NULL)
	call pl_ssize (pl, naxes, axlen, depth)

	return (pl)
end
