# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMCREA -- Create a new image of the indicated size and pixel type.
# Fortran callable version.

procedure imcrea (f77nam, axlen, naxis, pixtype, ier)

%       character*(*) f77nam
int	axlen[ARB]		# receives axis lengths
int	naxis			# receives number of axes
int	pixtype			# receives pixel type
int	ier			# receives error status

char	fname[SZ_PATHNAME]

begin
	# Convert character string to SPP string.
	call f77upk (f77nam, fname, SZ_PATHNAME)
	call imcrex (fname, axlen, naxis, pixtype, ier)
end
