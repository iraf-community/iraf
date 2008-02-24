# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mii.h>

# MIIREADC -- Read a block of character data stored externally in MII format.
# Data is returned in the machine independent character format.

long procedure miireadc (fd, spp, maxchars)

int	fd			# input file
char	spp[ARB]		# receives data
size_t	maxchars		# max number of chars to be read

pointer	sp, bp
size_t	pksize, nchars
long	lnchars
int	miipksize(), miinelem()
long	read()
errchk	read()

begin
	pksize = miipksize (maxchars, MII_BYTE)
	nchars = max (maxchars, pksize)
	lnchars = nchars

	if (nchars > maxchars) {
	    # Read data into local buffer and unpack into user buffer.

	    call smark (sp)
	    call salloc (bp, nchars, TY_CHAR)

	    lnchars = read (fd, Memc[bp], pksize)
	    if (lnchars != EOF) {
	        nchars = lnchars
		nchars = min (maxchars, miinelem (nchars, MII_BYTE))
		call miiupk8 (Memc[bp], spp, nchars, TY_CHAR)
	        lnchars = nchars
	    }

	    call sfree (sp)
	
	} else {
	    # Read data into user buffer and unpack in place.

	    lnchars = read (fd, spp, pksize)
	    if (lnchars != EOF) {
	        nchars = lnchars
		nchars = min (maxchars, miinelem (nchars, MII_BYTE))
		call miiupk8 (spp, spp, nchars, TY_CHAR)
	        lnchars = nchars
	    }
	}

	return (lnchars)
end
