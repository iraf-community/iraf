# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"symtab.h"

# STPSTR -- Append a string to the string buffer.  The offset of the string
# in the string buffer is returned as the function value.  More storage is
# allocated if we run out of room in the buffer.  The number of chars of
# storage allocated (excluding space for the EOS) is strlen(str) or MINCHARS,
# whichever is larger.  To allocate but not initialize space STR may be passed
# as the null string.  To allocate precisely the amount of space required to
# store a string constant MINCHARS should be set to zero.

int procedure stpstr (stp, str, minchars)

pointer	stp			# symtab descriptor
char	str[ARB]		# string to be moved into storage
int	minchars		# minimum number of chars to reserve

int	offset, buflen, blklen
int	strlen()
errchk	realloc

begin
	offset = ST_SBUFOP(stp)
	buflen = ST_SBUFLEN(stp)
	blklen = max (strlen(str), minchars) + 1

	if (offset + blklen > buflen) {
	    # Overflow has occurred.  Allocate a larger buffer; if overflow
	    # continues to occur the increments grow successively larger to
	    # minimize reallocation.

	    buflen = buflen + max (blklen, ST_SBUFINC(stp))
	    ST_SBUFINC(stp) = min (MAX_INCREMENT, ST_SBUFINC(stp) * INC_GROW)
	    ST_SBUFLEN(stp) = buflen
	    ST_SBUFNGROW(stp) = ST_SBUFNGROW(stp) + 1

	    call realloc (ST_SBUFP(stp), buflen, TY_CHAR)
	}

	ST_SBUFOP(stp) = ST_SBUFOP(stp) + blklen
	call strcpy (str, Memc[ST_SBUFP(stp)+offset], blklen)

	return (offset)
end
