# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 
include <mii.h>
include "fxf.h"

# FXF_READ_CARD -- Read a FITS header card.

int procedure fxf_read_card (fd, ibuf, obuf, ncards)

int	fd			#I Input file descriptor
char	ibuf[ARB]		#I input buffer
char    obuf[ARB]		#O Output buffer
int	ncards			#I ncards read so far

size_t	sz_val
int	ip, nchars_read
int	imod()
long	read()
errchk	read

begin
	# We read one FITS block first, read card from it until 36
	# cards have been processed, where we read again.

	if (imod (ncards, 36) == 0) {
	    sz_val = FITS_BLOCK_CHARS
	    nchars_read = read (fd, ibuf, sz_val)
	    if (nchars_read ==  EOF)
	        return (EOF)
	    sz_val = FITS_BLOCK_BYTES
	    call miiupk (ibuf, ibuf, sz_val, MII_BYTE, TY_CHAR)
	    ip = 1
	}
	
	sz_val = LEN_CARD
	call amovc (ibuf[ip], obuf, sz_val)
	ip = ip + LEN_CARD

	return (LEN_CARD)
end
