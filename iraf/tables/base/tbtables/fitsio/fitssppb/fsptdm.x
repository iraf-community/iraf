include "fitsio.h"

procedure fsptdm(ounit,colnum,naxis,naxes,status)

# write the TDIMnnn keyword

int     ounit           # i output file pointer
int     colnum          # i column number
int     naxis           # i number of axes
long	naxes[ARB]      # i dimension of each axis
int     status          # o error status

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
	call ftptdm(ounit,colnum,naxis,Memi[i_naxes_ptr],status)
	call mfree(i_naxes_ptr, TY_INT)
end
