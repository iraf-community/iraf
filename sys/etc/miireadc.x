# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mii.h>

# MIIREADC -- Read a block of character data stored externally in MII format.
# Data is returned in the machine independent character format.

int procedure mii_readc (fd, spp, maxchars)

int	fd			# input file
int	spp[ARB]		# receives data
int	maxchars		# max number of chars to be read

pointer	sp, bp
int	pksize, nchars
int	miipksize(), miinelem(), read()
errchk	read()

long	note()

begin
	pksize = miipksize (maxchars, MII_BYTE)
	nchars = max (maxchars, pksize)

	if (nchars > maxchars) {
	    # Read data into local buffer and unpack into user buffer.

	    call smark (sp)
	    call salloc (bp, nchars, TY_CHAR)

	    nchars = read (fd, Memc[bp], pksize)
	    if (nchars != EOF) {
		nchars = min (maxchars, miinelem (nchars, MII_BYTE))
		call miiupk8 (Memc[bp], spp, nchars, TY_CHAR)
	    }

	    call sfree (sp)
	
	} else {
	    # Read data into user buffer and unpack in place.

	    nchars = read (fd, spp, pksize)
	    if (nchars != EOF) {
		nchars = min (maxchars, miinelem (nchars, MII_BYTE))
		call miiupk8 (spp, spp, nchars, TY_CHAR)
	    }
	}

	return (nchars)
end
