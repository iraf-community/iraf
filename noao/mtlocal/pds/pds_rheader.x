include <imhdr.h>
include <mii.h>
include <mach.h>
include "rpds.h"

# PDS_READ_HEADER -- Read a PDS header. EOT is detected by an EOF on the
# first read and EOF is returned to the calling routine. Errors are
# passed to the calling routine.

int procedure pds_read_header (pds_fd, im, parameters)

int	pds_fd
pointer im
long	parameters[LEN_PAR_ARRAY]

int	nchars, sz_header, header[LEN_PDS_HEADER]
short	temptext[LEN_PDS_TEXT]
char	text[LEN_PDS_TEXT]

int	read(), pds_roundup()
short	pds_unpacks()
long	pds_unpackl()

errchk	read, miiupk

include	"rpds.com"

begin
	# Read the header record
	sz_header = pds_roundup (LEN_PDS_HEADER, SZB_CHAR) / SZB_CHAR
	nchars = read (pds_fd, header, sz_header)
	if (nchars == EOF)
	    return (EOF)
	else if (nchars != sz_header)
	    call error (1, "Error reading pds header.")

	# Unpack ID string, convert to ASCII and copy to image header
	call miiupk (header, temptext, LEN_PDS_TEXT, MII_SHORT, TY_SHORT)
	call pds_apdp8s (temptext, temptext, LEN_PDS_TEXT)
	call pds_apdp059 (temptext, text, LEN_PDS_TEXT)
	text[LEN_PDS_TEXT + 1] = EOS
	call strcpy (text, TITLE(im), SZ_TITLE)

	# Unpack the remainder of the header
	# If XCOORD or YCOORD are greater than TWO_TO_23 convert to
	# -ve number using TWO_TO_24

	P_DX(parameters) = DX(header)
	P_DY(parameters) = DY(header)
	P_NPTS_PER_SCAN(parameters) = NPTS_PER_SCAN(header)
	P_NSCANS(parameters) = NSCANS(header)
	P_SCANTYPE(parameters) = SCANTYPE(header)
	P_SCANSPEED(parameters) = SCANSPEED(header)
	P_SCANORIGIN(parameters) = SCANORIGIN(header)
	P_CORNER(parameters) = CORNER(header)
	P_NRECS_PER_SCAN(parameters) = NRECS_PER_SCAN(header)
	P_XTRAVEL(parameters) = XTRAVEL(header)
	P_YTRAVEL(parameters) = YTRAVEL(header)
	P_NPTS_PER_REC(parameters) = NPTS_PER_REC(header)
	P_XCOORD(parameters) = XCOORD(header)
	if (P_XCOORD(parameters) >= TWO_TO_23)
	    P_XCOORD(parameters) = P_XCOORD(parameters) - TWO_TO_24
	P_YCOORD(parameters) = YCOORD(header)
	if (P_YCOORD(parameters) >= TWO_TO_23)
	    P_YCOORD(parameters) = P_YCOORD(parameters) - TWO_TO_24

	# Write parameters to header
	CT_VALID(im) = NO
	NAXIS(im) = 2
	NCOLS(im) = P_NPTS_PER_SCAN(parameters)
	NLINES(im) = P_NSCANS(parameters)

	# print the header
	call pds_print_header (text, parameters)

	return (OK)
end

# PDS_UNPACKS -- Procedure to unpack a short header value. 
# The header value is stored in the integer array buffer, beginning
# at byte number offset. The value is unpacked into a temporary
# buffer, temp and converted to SPP format using the mii routines.
# Finally the PDS 10 or 12 bit bytes are converted to an SPP short value.

short procedure pds_unpacks (buffer, offset)

int	buffer[ARB], offset

short	value[1]
long	temp[1]
errchk	miiupk, miiupk

begin
	call bytmov (buffer, offset, temp, 1, SZB_MIISHORT)
	call miiupk (temp, value[1], 1, MII_SHORT, TY_SHORT)
	call pds_apdp8s (value[1], value[1], 1)
	return (value[1])
end

# PDS_UNPACKL -- Procedure to unpack a 24 bit long header value.

long	procedure pds_unpackl (buffer, offset)

int	buffer[ARB], offset

short	temps[2]
long	temp[1], value[1]
errchk	miiupk, bytmov

begin
	call bytmov (buffer, offset, temp, 1, SZB_MIILONG)
	call miiupk (temp, temps, 2, MII_SHORT, TY_SHORT)
	call pds_apdp8s (temps, temps, 2)
	value[1] = temps[1] * 10000b + temps[2]
	return (value[1])
end

# PDS_APDP8S -- Precedure to change a 12 or 10 bit PDP8 value to a short integer
# value. 

procedure pds_apdp8s (a, b, npix)

short	a[npix], b[npix]
int	npix

int	i

begin
	for (i=1; i <= npix; i = i + 1)
	    b[i] = (a[i] / 400b) * 100b + mod (int (a[i]), 400b)
end

# PDS_APDP059 -- Procedure to convert PDP 059 code into ASCII

procedure pds_apdp059 (a, b, nchar)

char	b[nchar]
int	nchar
short	a[nchar]

int	i, j
char	table[LEN_TABLE]

# Conversion table from PDS 059 code to ASCII
data	table/ ' ', ' ', ' ', ' ', '$', ' ', ' ', '"', '(', ')',
	       '*', '+', ',', '-', '.', '/', '0', '1', '2', '3',
	       '4', '5', '6', '7', '8', '9', ' ', ' ', ' ', '=',
	       ' ', ' ', ' ', 'A', 'B', 'C', 'D', 'E', 'F', 'G',
	       'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q',
	       'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', ' ',
	       ' ', ' ', ' ', ' ', ' ', 'a', 'b', 'c', 'd', 'e',
	       'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
	       'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y',
	       'z', ' ', ' ', ' ', ' ', ' ', EOS/ 

begin
	for (i = 1; i <= nchar; i = i + 1)
    	    b[i] = a[i] - 159

	for (i = 1; i <= nchar; i = i + 1) {
	    j = b[i]
	    if (j < 1 || j > LEN_TABLE)
		j = 1
	    b[i] = table[j]
	}
end



# PDS_PRINT_HEADER -- Procedure to print the header.

procedure pds_print_header (text, parameters)

char	text[LEN_PDS_TEXT]
long	parameters[LEN_PAR_ARRAY]

include "rpds.com"

begin
	if (long_header == YES)
	    call pds_long_header (text, parameters)
	if (short_header == YES && long_header == NO) {
	    call printf ("ID: %.30s  ")
		call pargstr (text)
	    call printf ("size =%d * %d \n")
		call pargl (P_NPTS_PER_SCAN(parameters))
		call pargl (P_NSCANS(parameters))
	}
end

# PDS_LONG_HEADER -- Print a long header

procedure pds_long_header (text, parameters)

char	text[LEN_PDS_TEXT]
long	parameters[LEN_PAR_ARRAY]

begin
	call printf ("ID:%s\n")
	    call pargstr (text)
	call printf ("NPTS = %d   ")
	    call pargl (P_NPTS_PER_SCAN(parameters))
	call printf ("NSCANS = %d   ")
	    call pargl (P_NSCANS(parameters))
	call printf ("NRECS/SCAN = %d   ")
	    call pargl (P_NRECS_PER_SCAN(parameters))
	call printf ("PPERR = %d\n")
	    call pargl (P_NPTS_PER_REC(parameters))
	call printf ("SCANTYPE = %s  ")
	    if (P_SCANTYPE(parameters) == RASTER)
	        call pargstr ("RASTER")
	    else if (P_SCANTYPE(parameters) == EDGE)
	        call pargstr ("EDGE")
	    else
	        call pargstr ("FLIPPED")
	call printf ("SCANSPEED = %d  ")
	    call pargl (P_SCANSPEED(parameters))
	call printf ("SCANORIGIN = %d  ")
	    call pargl (P_SCANORIGIN(parameters))
	call printf ("CORNER = %d\n")
	    call pargl (P_CORNER(parameters))
	call printf ("DX = %d  ")
	    call pargl (P_DX(parameters))
	call printf ("XTRAVEL = %d  ")
	    call pargl (P_XTRAVEL(parameters))
	call printf ("XCOORD = %d\n")
	    call pargl (P_XCOORD(parameters))
	call printf ("DY = %d  ")
	    call pargl (P_DY(parameters))
	call printf ("YTRAVEL = %d  ")
	    call pargl (P_YTRAVEL(parameters))
	call printf ("YCOORD = %d\n")
	    call pargl (P_YCOORD(parameters))
end
