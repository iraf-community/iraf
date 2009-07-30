include "fitsio.h"

procedure fsphpr(ounit,simple,bitpix,naxis,naxes,
                 pcount,gcount,extend,status)

# write required primary header keywords

int     ounit           # i output file pointer
bool    simple          # i simple FITS file?
int     bitpix          # i bits per pixel
int     naxis           # i number of axes
long	naxes[ARB]      # i dimension of each axis
long	pcount          # i no. of group parameters
long	gcount          # i no. of groups
bool    extend          # i EXTEND keyword = TRUE?
int     status          # o error status

int	i_pcount
int	i_gcount
int	i
size_t	sz_val
pointer	i_naxes_ptr
errchk  malloc

begin
	sz_val = naxis
	call malloc (i_naxes_ptr, sz_val, TY_INT)
	do i = 1, naxis {
	    Memi[i_naxes_ptr + i - 1] = naxes[i]
	}
	i_pcount = pcount
	i_gcount = gcount
	call ftphpr(ounit,simple,bitpix,naxis,Memi[i_naxes_ptr],
		    i_pcount,i_gcount,extend,status)
	call mfree(i_naxes_ptr, TY_INT)
end
