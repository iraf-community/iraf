# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mii.h>

# MIIREADI -- Read a block of integer data stored externally in MII format.
# Data is returned in the machine independent integer format.

int procedure miireadi (fd, spp, maxints)

int	fd			# input file
int	spp[ARB]		# receives data
int	maxints			# max number of int units to be read

pointer	sp, bp
int	pksize, nchars, nints
int	miipksize(), miinelem(), read()
errchk	read()

begin
	pksize = miipksize (maxints, MII_LONG)
	nints  = EOF

	if (pksize > maxints * SZ_INT) {
	    # Read data into local buffer and unpack into user buffer.

	    call smark (sp)
	    call salloc (bp, pksize, TY_CHAR)

	    nchars = read (fd, Memc[bp], pksize)
	    if (nchars != EOF) {
		nints = min (maxints, miinelem (nchars, MII_LONG))
		call miiupk32 (Memc[bp], spp, nints, TY_INT)
	    }

	    call sfree (sp)
	
	} else {
	    # Read data into user buffer and unpack in place.

	    nchars = read (fd, spp, pksize)
	    if (nchars != EOF) {
		nints = min (maxints, miinelem (nchars, MII_LONG))
		call miiupk32 (spp, spp, nints, TY_INT)
	    }
	}

	return (nints)
end
