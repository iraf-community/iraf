# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mii.h>

# MIIREAD -- Read a block of data stored externally in MII format.
# Data is returned in the format of the local host machine.

long procedure miireadl (fd, spp, maxelem)

int	fd			#I input file
long	spp[ARB]		#O receives data
size_t	maxelem			# max number of data elements to be read

size_t	sz_val
pointer	sp, bp
size_t	pksize
long	nchars, nelem
size_t	miipksize(), miinelem()
long	read()
errchk	read()

begin
	pksize = miipksize (maxelem, MII_LONG)
	nelem  = EOF

	if (pksize > maxelem * SZ_LONG) {
	    # Read data into local buffer and unpack into user buffer.

	    call smark (sp)
	    call salloc (bp, pksize, TY_CHAR)

	    nchars = read (fd, Memc[bp], pksize)
	    if (nchars != EOF) {
		sz_val = nchars
		nelem = min (maxelem, miinelem (sz_val, MII_LONG))
		sz_val = nelem
		call miiupkl (Memc[bp], spp, sz_val, TY_LONG)
	    }

	    call sfree (sp)
	
	} else {
	    # Read data into user buffer and unpack in place.

	    nchars = read (fd, spp, pksize)
	    if (nchars != EOF) {
		sz_val = nchars
		nelem = min (maxelem, miinelem (sz_val, MII_LONG))
		sz_val = nelem
		call miiupkl (spp, spp, sz_val, TY_LONG)
	    }
	}

	return (nelem)
end
