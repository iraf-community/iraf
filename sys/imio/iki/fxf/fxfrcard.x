# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 
include <mii.h>
include "fxf.h"

# FXF_READ_CARD -- Read a FITS header card.

int procedure fxf_read_card (fd, ibuf, obuf, ncards)

int	fd			#I Input file descriptor
char	ibuf[ARB]		#I input buffer
char    obuf[ARB]		#O Output buffer
int	ncards			#I ncards read so far

int	ip, nchars_read
int	read()
errchk	read

begin
	# We read one FITS block first, read card from it until 36
	# cards have been processed, where we read again.

	if (mod (ncards, 36) == 0) {
	    nchars_read = read (fd, ibuf, FITS_BLOCK_CHARS)
	    if (nchars_read ==  EOF)
	        return (EOF)
	    call miiupk (ibuf, ibuf, FITS_BLOCK_BYTES, MII_BYTE, TY_CHAR)
	    ip = 1
	}
	
	call amovc (ibuf[ip], obuf, LEN_CARD)
	ip = ip + LEN_CARD

	return (LEN_CARD)
end
