# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <nmi.h>

# NMIREAD -- Read a block of data stored externally in NMI format.
# Data is returned in the format of the local host machine.

int procedure nmi_readb (fd, spp, maxelem)

int	fd			#I input file
bool	spp[ARB]		#O receives data
int	maxelem			# max number of data elements to be read

pointer	sp, bp
int	pksize, nchars, nelem
int	nmipksize(), nminelem(), read()
errchk	read()

long	note()

begin
	pksize = nmipksize (maxelem, NMI_BOOL)
	nelem  = EOF

	if (pksize > maxelem * SZ_BOOL) {
	    # Read data into local buffer and unpack into user buffer.

	    call smark (sp)
	    call salloc (bp, pksize, TY_CHAR)

	    nchars = read (fd, Memc[bp], pksize)
	    if (nchars != EOF) {
		nelem = min (maxelem, nminelem (nchars, NMI_BOOL))
		call nmiupkb (Memc[bp], spp, nelem, TY_BOOL)
	    }

	    call sfree (sp)
	
	} else {
	    # Read data into user buffer and unpack in place.

	    nchars = read (fd, spp, pksize)
	    if (nchars != EOF) {
		nelem = min (maxelem, nminelem (nchars, NMI_BOOL))
		call nmiupkb (spp, spp, nelem, TY_BOOL)
	    }
	}

	return (nelem)
end
