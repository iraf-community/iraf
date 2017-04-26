# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <nmi.h>

# NMIWRITEC -- Write a block of character data to a file in NMI format.
# The input data is assumed to be in a native machine format. 

procedure nmi_writec (fd, spp, nchars)

int	fd			# output file
int	spp[ARB]		# data to be written
int	nchars			# number of chars units to be written

pointer	sp, bp
int	bufsize
int	nmipksize()

begin
	call smark (sp)

	bufsize = nmipksize (nchars, NMI_BYTE)
	call salloc (bp, bufsize, TY_CHAR)

	call nmipak8 (spp, Memc[bp], nchars, TY_CHAR)
	call write (fd, Memc[bp], bufsize)

	call sfree (sp)
end
