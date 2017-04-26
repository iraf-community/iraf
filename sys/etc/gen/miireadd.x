# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mii.h>

# MIIREAD -- Read a block of data stored externally in MII format.
# Data is returned in the format of the local host machine.

int procedure mii_readd (fd, spp, maxelem)

int	fd			#I input file
double	spp[ARB]		#O receives data
int	maxelem			# max number of data elements to be read

pointer	sp, bp
int	pksize, nchars, nelem
int	miipksize(), miinelem(), read()
errchk	read()

long	note()

begin
	pksize = miipksize (maxelem, MII_DOUBLE)
	nelem  = EOF

	if (pksize > maxelem * SZ_DOUBLE) {
	    # Read data into local buffer and unpack into user buffer.

	    call smark (sp)
	    call salloc (bp, pksize, TY_CHAR)

	    nchars = read (fd, Memc[bp], pksize)
	    if (nchars != EOF) {
		nelem = min (maxelem, miinelem (nchars, MII_DOUBLE))
		call miiupkd (Memc[bp], spp, nelem, TY_DOUBLE)
	    }

	    call sfree (sp)
	
	} else {
	    # Read data into user buffer and unpack in place.

	    nchars = read (fd, spp, pksize)
	    if (nchars != EOF) {
		nelem = min (maxelem, miinelem (nchars, MII_DOUBLE))
		call miiupkd (spp, spp, nelem, TY_DOUBLE)
	    }
	}

	return (nelem)
end
