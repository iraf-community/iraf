# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <nmi.h>


# NMI_WRITEB -- Write a block of data to a file in NMI format.
# The input data is in the host system native binary format.

procedure nmi_writeb (fd, spp, nelem)

int	fd			#I output file
int	spp[ARB]		#I native format data to be written
int	nelem			#I number of data elements to be written

int	bufsize
int	nminelem()

begin
	bufsize = nminelem (nelem, NMI_INT)
	call write (fd, spp, bufsize)
end
