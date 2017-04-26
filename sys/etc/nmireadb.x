# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <nmi.h>


# NMI_READB -- Read a block of data stored externally in NMI format.
# Data is returned in the format of the local host machine.

int procedure nmi_readb (fd, spp, maxelem)

int	fd			#I input file
bool	spp[ARB]		#O receives data
int	maxelem			# max number of data elements to be read

pointer	sp, bp
int	pksize, nchars, nelem
int	nminelem(), read()
errchk	read()

long	note()

begin
	pksize = nminelem (maxelem, NMI_INT)
	nelem  = EOF

	# Read data into user buffer and unpack in place.
	nchars = read (fd, spp, pksize)
	if (nchars != EOF)
	    nelem = min (maxelem, pksize)

	return (nelem)
end
