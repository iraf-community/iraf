# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMOPEN -- Open an existing imagefile.  Fortran callable version.

procedure imopen (f77nam, acmode, im, ier)

%       character*(*) f77nam
int	acmode			# image access mode (RO, WO)
pointer	im			# receives image descriptor pointer
int	ier			# receives error status

char	fname[SZ_PATHNAME]

begin
	# Unpack character string into SPP string.
	call f77upk (f77nam, fname, SZ_PATHNAME)
	call imopnx (fname, acmode, im, ier)
end
