# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mii.h>

# MIIREAD -- Read a block of data stored externally in MII format.
# Data is returned in the format of the local host machine.

long procedure miireads (fd, spp, maxelem)

int	fd			#I input file
short	spp[ARB]		#O receives data
size_t	maxelem			# max number of data elements to be read

size_t	sz_val
pointer	sp, bp
size_t	pksize
long	nchars, nelem
size_t	miipksize(), miinelem()
long	read()
errchk	read()

begin
	pksize = miipksize (maxelem, MII_SHORT)
	nelem  = EOF

	if (pksize > maxelem * SZ_SHORT) {
	    # Read data into local buffer and unpack into user buffer.

	    call smark (sp)
	    call salloc (bp, pksize, TY_CHAR)

	    nchars = read (fd, Memc[bp], pksize)
	    if (nchars != EOF) {
		sz_val = nchars
		nelem = min (maxelem, miinelem (sz_val, MII_SHORT))
		sz_val = nelem
		call miiupks (Memc[bp], spp, sz_val, TY_SHORT)
	    }

	    call sfree (sp)
	
	} else {
	    # Read data into user buffer and unpack in place.

	    nchars = read (fd, spp, pksize)
	    if (nchars != EOF) {
		sz_val = nchars
		nelem = min (maxelem, miinelem (sz_val, MII_SHORT))
		sz_val = nelem
		call miiupks (spp, spp, sz_val, TY_SHORT)
	    }
	}

	return (nelem)
end
