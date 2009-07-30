include "fitsio.h"

procedure fsgtdm(iunit,colnum,maxdim,naxis,naxes,status)

# read the TDIMnnn keyword

int     iunit           # i input file pointer
int     colnum          # i column number
int     maxdim          # i maximum number of dimensions to return
int     naxis           # i number of axes
long	naxes[ARB]      # i dimension of each axis
int     status          # o error status

int	i
size_t	sz_val
pointer	i_naxes_ptr
errchk  calloc

begin
	sz_val = maxdim
	call calloc (i_naxes_ptr, sz_val, TY_INT)
	call ftgtdm(iunit,colnum,maxdim,naxis,Memi[i_naxes_ptr],status)
	if ( status == 0 ) {
	    do i = 1, naxis {
		Memi[i_naxes_ptr + i - 1] = naxes[i]
	    }
	}
	call mfree(i_naxes_ptr, TY_INT)
end
