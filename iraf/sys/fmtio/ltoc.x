# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

define	DECIMAL		10

# LTOC -- Convert long integer to decimal string.
# Returns the number of characters generated.

int procedure ltoc (lval, outstr, maxch)

long	lval				# long integer to be encoded
char	outstr[ARB]			# output buffer
int	maxch				# size of output buffer
int	gltoc()

begin
	return (gltoc (lval, outstr, maxch, DECIMAL))
end
