# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mii.h>

# MIIREADS -- Read a block of short integer data stored externally in MII
# format.  Data is returned in the machine independent short integer format.

int procedure miireads (fd, spp, maxints)

int	fd			# input file
int	spp[ARB]		# receives data
int	maxints			# max number of short int units to be read

pointer	sp, bp
int	pksize, nchars, nints
int	miipksize(), miinelem(), read()
errchk	read()

begin
	pksize = miipksize (maxints, MII_SHORT)
	nints  = EOF

	if (pksize > maxints * SZ_SHORT) {
	    # Read data into local buffer and unpack into user buffer.

	    call smark (sp)
	    call salloc (bp, pksize, TY_CHAR)

	    nchars = read (fd, Memc[bp], pksize)
	    if (nchars != EOF) {
		nints = min (maxints, miinelem (nchars, MII_SHORT))
		call miiupk16 (Memc[bp], spp, nints, TY_SHORT)
	    }

	    call sfree (sp)
	
	} else {
	    # Read data into user buffer and unpack in place.

	    nchars = read (fd, spp, pksize)
	    if (nchars != EOF) {
		nints = min (maxints, miinelem (nchars, MII_SHORT))
		call miiupk16 (spp, spp, nints, TY_SHORT)
	    }
	}

	return (nints)
end
